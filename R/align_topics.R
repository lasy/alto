#' Align topics from distinct LDA models
#'
#' This function takes a list of LDA models and returns an object of class
#' \code{alignment}. Each element in the models list must be itself a named
#' list, corresponding to the mixed memberships (\code{$gamma}) and topics
#' (\code{$beta}). The resulting alignment object can be plotted using `plot`
#' and its weights can be extracted using the `weights` accessor function. See
#' the documentation for class \code{alignment} for further details.
#'
#' @param models (required) A list of LDA models object. Each list component
#' must be a list with two named entries, $gamma (containing mixed memberships)
#' and $beta (containing topic parameters in log sapce). See
#' \code{run_lda_models} for details.
#' @param method (required) Either \code{product} or \code{transport}, giving
#' two types of alignment strategies, using inner products between gamma vectors
#' or optimal transport between gamma-beta pairs, respectively. Defaults to
#' \code{product}.
#' @param ... (optional) Further keyword arguments passed to the weight
#' function. For example, passing \code{reg = 10} when using the
#' \code{transport} method will use a regularization level to 10 in the Sinkhorn
#' optimal transport algorithm.
#' @return An object of class \code{alignment} providing the weights between
#' every pair of topics of each model pairs in the input edgelist
#' (\code{comparisons}).
#'
#' @details
#'
#' After topics are aligned, they are re-ordered such that topics connected
#' by high weights are ranked similarly within their respective models.
#'
#' Topic paths (sets of topics connected by high weights across
#' models) are then identified and alignment diagnostics (topic refinement and
#' coherence scores) are computed. These variables are included to the
#' \code{topics} container of the returned \code{alignment}.
#'
#'
#' @seealso alignment
#' @examples
#' library(purrr)
#' data <- rmultinom(10, 20, rep(0.1, 20))
#' lda_params <- setNames(map(1:5, ~ list(k = .)), 1:5)
#' lda_models <- run_lda_models(data, lda_params)
#'
#' alignment <- align_topics(lda_models)
#' alignment
#' plot(alignment)
#'
#' plot(alignment, color_by = "refinement")
#' alignment <- align_topics(lda_models, method = "transport")
#' plot(alignment)
#' plot_beta(alignment)
#'
#' topics(alignment)
#' weights(alignment)
#' models(alignment)
#' @importFrom purrr map
#' @export
align_topics <- function(
  models,
  method = "product",
  ...
) {

  # 1. Check input and initialize key objects
  .check_align_input(models, method)
  weight_fun <- ifelse(method == "product", product_weights, transport_weights)
  if (is.null(names(models))) { names(models) <- seq_along(models) }

  # 2. topics
  topics <- topics_list(models)

  # 3. perform alignment
  weights <-
    align_graph(
      edges = setup_edges("all", names(models)),
      gamma_hats = map(models, ~ .$gamma),
      beta_hats = map(models, ~ exp(.$beta)),
      weight_fun = weight_fun, ...
    )

  aligned_topics <-  new("alignment", topics = topics, weights = weights, models = models)

  # 4. re-order the topics, identify the branches and compute summary diagnostics
  aligned_topics %>%
    order_topics() %>%
    add_branches() %>%
    add_summaries()
}

#' @importFrom magrittr %>%
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble mutate
topics_list <-  function(models) {
  map_dfr(
    .x = names(models),
    .f = function(model) {
      tibble(m = model,
             k = 1:nrow(models[[model]]$beta),
             mass = colSums(models[[model]]$gamma)) %>%
        mutate(prop = mass / sum(mass))
    }
  ) %>%
    mutate(m = m %>% factor(., levels = names(models)))
}

order_topics <- function(aligned_topics) {

  perms <- consecutive_weights(aligned_topics) %>%
    mutate(k_init = k_next) %>%
    forward_ordering() %>%
    backward_ordering() %>%
    ungroup() %>%
    topic_ordering()

  aligned_topics@models <- reorder_models(aligned_topics@models, perms)
  aligned_topics@topics <- reorder_topics(aligned_topics@topics, perms)
  aligned_topics@weights <- reorder_weights(aligned_topics@weights, perms)

  new(
    "alignment",
    topics = aligned_topics@topics,
    weights = aligned_topics@weights,
    models = aligned_topics@models
  )
}

consecutive_weights <- function(aligned_topics) {
  model_names <- names(aligned_topics@models) %>%
    factor(., levels = names(aligned_topics@models))

  tibble(
      m = model_names %>% head(-1),
      m_next = model_names %>% tail(-1)
    ) %>%
    left_join(aligned_topics@weights, by = c("m", "m_next"))
}

compute_ordering_cost <- function(weights) {
  weights %>%
    group_by(m) %>%
    mutate(y = k / max(k), y_next = k_next / max(k_next)) %>%
    ungroup() %>%
    mutate(c = abs(y_next - y) * weight) %>%
    pull(c) %>%
    sum()
}

#' Edgelists for Default Alignments
#'
#' This is a helper function for setting up edges that can be used by
#' align_graph. It implements two types of comparisons, 'consecutive' and 'all'.
#' It returns a data frame specifying which topics to compare from across all
#' models.
#'
#' @param comparisons A string describing the type of model comparisons to
#'   compute.
#' @param model_names The names of the models to compare. The resulting edge
#'   list will refer to models by these names.
#' @importFrom magrittr set_colnames %>%
#' @importFrom dplyr filter
#' @importFrom tibble tibble as_tibble
#' @importFrom utils combn head tail
#' @export
setup_edges <- function(comparisons, model_names) {
  edges <- comparisons
  if (comparisons == "consecutive") {
    edges <- tibble(
      from = head(model_names, -1), to = tail(model_names, -1)
    )
  } else if (comparisons == "all") {
    edges <- t(combn(model_names, 2)) %>%
      as_tibble(.name_repair = "unique") %>%
      suppressMessages() %>%
      set_colnames(c("from", "to")) %>%
      filter(from != to)
  }

  edges
}

#' @importFrom purrr map map_int
#' @importFrom stringr str_starts
.check_align_input <- function(
  models,
  method
) {
  # check model list input
  stopifnot(typeof(models) == "list")
  stopifnot(
    all(map_int(models, ~ class(.) == "list"))
  )
  stopifnot(
    all(map_int(models, ~ all(names(.) %in% c("gamma", "beta"))))
  )

  # check method used
  match.arg(tolower(method), c("product", "transport"))
}

#' Alignment between Pairs of Topics
#'
#' This provides a more generic interface to alignment between arbitrary pairs
#' of topics, compared to `align_topics`. Rather than requiring sequential or
#' all-vs-all comparisons, this function supports comparisons between any pairs
#' of models, as specified by the `edges` parameter. Any graph linking pairs of
#' models can be the starting point for an alignment.
#'
#' @param edges A data frame with two columns, $from and $to, giving the names
#' of the models to be aligned. These names must be the names of the lists in
#' `gamma_hats` and `beta_hats`.
#' @param gamma_hats A named list of matrices, giving estimated mixed-membership
#' parameters across a collection of topic models. The names of this list must
#' correspond to the names of models to compare in `edges`.
#' @param beta_hats A named list of matrices, giving estimated topic parameters
#' across a collection of topic models. The names of this list must correspond
#' to the names of models to compare in `edges`.
#' @param weight_fun A function that returns a data.frame giving weights between
#' all pairs of topics between two models. The first argument must accept a list
#' of two gamma_hat matrices, the second argument must accept a list of two
#' beta_hat matrices. See `product_weights` or `transport_weights` for examples.
#' @param ... (optional) Further keyword arguments passed to the weight
#' function. For example, passing \code{reg = 10} when using the
#' \code{transport} method will use a regularization level fo 10 in the Sinkhorn
#' optimal transport algorithm.
#' @importFrom dplyr mutate ungroup
#' @importFrom magrittr %>%
#' @export
align_graph <- function(edges, gamma_hats, beta_hats, weight_fun, ...) {
  weights <- list()
  for (i in seq_len(nrow(edges))) {
    pair <- c(edges$from[i], edges$to[i])
    weights[[i]] <- weight_fun(gamma_hats[pair], beta_hats[pair], ...) %>%
      mutate(m = pair[1], m_next = pair[2])
  }
  postprocess_weights(weights, nrow(gamma_hats[[1]]), names(gamma_hats)) %>%
    ungroup()
}

#' Product Weights between a Model Pair
#'
#' An alignment based on product weights sets the weight between topics k and k'
#' according to \eqn{\gamma_{k}^T\gamma_{k}^\prime}, where \eqn{\gamma_{k} \in
#' \mathbb{R}^n_{+}} provides the mixed membership assigned to topic \eqn{k}
#' across the \eqn{n} samples (and similarly for topic \eqn{k^\prime}). This
#' function computes these weights given a list of two \eqn{n \times K} gamma
#' matrices.
#'
#' @param gammas (required) A list of length two, containing the mixed
#' membership matrices (a \code{matrix} of dimension n-samples by k-topics) to
#' compare. The number of columns may be different, but the number of samples
#' must be equal.
#' @param ... (optional) Other keyword arguments. These are unused by the
#' \code{product_weights} alignment strategy, but is included for consistency
#' across weight functions.
#' @return products A \code{data.frame} giving the product similarity of each
#' pair of topics across the two input matrices.
#'
#' @examples
#' g1 <- matrix(runif(20 * 2), 20, 2)
#' g2 <- matrix(runif(20 * 4), 20, 4)
#' product_weights(list(g1, g2))
#'
#' @seealso align_graph
#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @export
product_weights <- function(gammas, ...) {
  products <- t(gammas[[1]]) %*% gammas[[2]]
  dimnames(products) <- map(gammas, ~ colnames(.))
  data.frame(products) %>%
    .lengthen_weights()
}

#' Transport Weights between Model Pair
#'
#' An alignment based on transport weights sets the weight between topics k and
#' k' according to an optimal transport problem with (1) costs set by the
#' distance (specifically, Jensen-Shannon Divergence) between \eqn{\beta_{k}}
#' and \eqn{\beta_{k^\prime}} and (2) masses defined by the total topic mixed
#' memberships \eqn{\sum_{i}\gamma_{ik}} and \eqn{\sum_{i}\gamma_{ik^\prime}}.
#' If topics have similar mixed membership weight and similar topic \eqn{\beta},
#' then they will be given high transport alignment weight.
#'
#' @param gammas (required) A list of length two, containing the mixed
#' membership matrices (a \code{matrix} of dimension n-samples by k-topics) to
#' compare. The number of columns may be different, but the number of samples
#' must be equal.
#' @param betas (required). A list of length two, containing the topic matrices
#' (a \code{matrix} of dimension k-topics by d-dimensions).) The number of rows
#' may be different, but the number of columns must remain fixed.
#' @param reg (optional) How much regularization to use in the Sinkhorn optimal
#' transport algorithm? Defaults to 0.1.
#' @param ... (optional) Other keyword arguments. Not used here, but included
#' for consistency with other weight functions.
#' @return products A \code{data.frame} giving the product similarity of each
#' pair of topics across the two input matrices.
#'
#'
#' @examples
#' library(purrr)
#' data <- rmultinom(10, 20, rep(0.1, 20))
#' lda_params <- setNames(map(1:5, ~ list(k = .)), 1:5)
#' lda_models <- run_lda_models(data, lda_params)
#' gammas <- list(lda_models[[3]]$gamma, lda_models[[5]]$gamma)
#' betas <- list(lda_models[[3]]$beta, lda_models[[5]]$beta)
#' transport_weights(gammas, betas)
#'
#' @importFrom philentropy JSD
#' @importFrom purrr map
#' @importFrom Barycenter Sinkhorn
#' @export
transport_weights <- function(gammas, betas, reg = 0.1, ...) {
  betas_mat <- do.call(rbind, betas)
  costs <- suppressMessages(JSD(betas_mat))
  ix <- seq_len(nrow(betas[[1]]))

  a <- matrix(colSums(gammas[[1]]), ncol = 1)
  b <- matrix(colSums(gammas[[2]]), ncol = 1)
  plan <- Sinkhorn(a, b, costs[ix, -ix, drop = F], lambda = reg)$Transportplan

  if (any(is.na(plan))) {
    plan <- matrix(0, nrow(betas[[1]]), nrow(betas[[2]]))
    warning("OT diverged, considering increasing regularization.\n")
  }

  dimnames(plan) <- map(gammas, ~ colnames(.))
  data.frame(plan) %>%
    .lengthen_weights()
}

################################################################################
# Helper functions for reshaping and renaming
################################################################################
#' @importFrom dplyr mutate
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace
.lengthen_weights <- function(weights) {
  weights %>%
    rownames_to_column("k") %>%
    pivot_longer(-k, names_to = "k_next", values_to = "weight") %>%
    mutate(k_next = str_replace(k_next, "X", ""))
}

#' @importFrom dplyr group_by bind_rows summarize mutate ungroup arrange across
#' everything
#' @importFrom magrittr %>%
postprocess_weights <- function(weights, n_docs, m_levels) {
  bind_rows(weights) %>%
    mutate(document_mass = weight, weight = document_mass / n_docs) %>%
    group_by(m, m_next, k_next) %>%
    mutate(bw_weight = weight / sum(weight)) %>%
    ungroup() %>%
    mutate(
      across(c("m", "m_next"), factor, levels = m_levels),
      across(c("k", "k_next"), as.integer)
    ) %>%
    group_by(m, m_next, k) %>%
    mutate(
      fw_weight = weight / sum(weight)
    ) %>%
    arrange(m) %>%
    select(m, m_next, k, k_next, everything())
}

################################################################################
# Class construction and methods
################################################################################
#' @importFrom utils head
print_alignment <- function(object) {
  cat(sprintf(
    "# An %s: %d models, %d topics:\n",
    class(object), n_models(object), n_topics(object)
  ))

  print(head(object@weights))
  cat(sprintf("# ... with %s more rows", nrow(object@weights) - 6))
}

#' Alignment Class Definition
#'
#' The alignment class contains all the information available associated with an
#' alignment across an ensemble of topic models. The available accessor methods
#' are,
#'
#'  * \code{weights}: Extract weights between all pairs of topics within an
#'  alignment. Topic pairs with high alignment scores are more similar to one
#'  another, though the precise implementation will depend on the \code{method}
#'  used during \code{align_topics}. Note that only the weights are needed in
#'  order to compute stability, refinement, and key topics summaries.
#' * \code{models}: Extract the model parameters that were used in the original
#'   alignment. Note that the latent topics may have been reordered, to maximize
#'   the consistency across all models according to their alignment.
#' * \code{n_topics}: How many topics are there total, within the alignment
#'   object?
#' \code{n_models}: How many models total are there, within the alignment
#'   object?
#'
#' @seealso align_topics
#' @import methods
#' @exportClass alignment
setClass("alignment",
         representation(
           topics = "data.frame",
           weights = "data.frame",
           models = "list",
           n_models = "numeric",
           n_topics = "numeric"
         )
)

#' Show Method for Alignment Class
#' @param object An alignment object output from \code{align_topics}.
#' @import methods
#' @export
setMethod("show", "alignment", print_alignment)

setGeneric("weights", function(x) standardGeneric("weights"))
#' Weights Accessor for Alignment Class
#' @param x An alignment object output from \code{align_topics}.
#' @import methods
#' @export
setMethod("weights", "alignment", function(x) x@weights)

setGeneric("n_models", function(x) standardGeneric("n_models"))

#' Number of Models Method for Alignment Class
#' @param x An alignment object output from \code{align_topics}.
#' @import methods
#' @export
setMethod("n_models", "alignment", function(x) nlevels(x@weights$m))


setGeneric("n_topics", function(x) standardGeneric("n_topics"))
#' Number of Topics Method for Alignment Class
#' @param x An alignment object output from \code{align_topics}.
#' @import methods
#' @export
setMethod("n_topics", "alignment", function(x) nrow(x@topics))

setGeneric("models", function(x) standardGeneric("models"))
#' Extract Models underlying Alignment
#' @param x An alignment object output from \code{align_topics}.
#' @import methods
#' @export
setMethod("models", "alignment", function(x) x@models)


setGeneric("topics", function(x) standardGeneric("topics"))
#' Extract List of Topics and their Summaries
#' @param x An alignment object output from \code{align_topics}.
#' @import methods
#' @export
setMethod("topics", "alignment", function(x) x@topics)
