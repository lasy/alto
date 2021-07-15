#' Align topics from distinct LDA models
#'
#' [Description]
#'
#' @param models (required) a \code{lda_models} object. See
#'   \code{run_lda_models} for details.
#' @param comparisons (optional) either a character indicating if topics
#'   should be aligned between \code{consecutive} or \code{all} models, or a
#'   list of model pairs between which topics should be aligned.
#' @param perm_search (optional) How large should the search be for topic
#'    reordering?
#'
#' @return a \code{data.frame} (? or some specific object) providing the weights
#'   between every pair of topics of each model pairs in the input edgelist
#'   (\code{comparisons}). ? Do we also return the lda_models with ordered
#'   topics?
#' @importFrom purrr map
#' @export
align_topics <- function(
  models,
  comparisons = "consecutive",
  method = "product",
  order_constrain = NULL,
  perm_search = 2,
  order_version = "kris",
  ...
) {

  # 1. Check input and initialize key objects
  .check_align_input(models, comparisons, method)
  weight_fun <- ifelse(method == "product", product_weights, transport_weights)
  if (is.null(names(models))) {
    names(models) <- seq_along(models)
  }
  edges <- setup_edges(comparisons, names(models))

  # 2. perform alignment
  weights <- align_graph(
    edges,
    map(models, ~ .$gamma),
    map(models, ~ exp(.$beta)),
    weight_fun, ...
  )
  # 3. reorder the topics, if k's are sequenced
  if (comparisons == "consecutive") {
    if (order_version == "kris") ordered <- reorder_topics(weights, models, perm_search)
    else ordered <- reorder_topics_lsy(weights, models)
  } else {
    ordered <- list(weights = weights, models = models)
  }

  new("alignment", weights = ordered$weights, models = ordered$models)
}

#' Edgelists for Default Alignment
#'
#' @param comparisons
#' @param model_names
#' @importFrom magrittr set_colnames %>%
#' @importFrom dplyr filter
#' @importFrom tibble tibble as_tibble
#' @importFrom utils combn head tail
setup_edges <- function(comparisons, model_names) {
  edges <- comparisons
  if (comparisons == "consecutive") {
    edges <- tibble::tibble(
      from = head(model_names, -1), to = tail(model_names, -1)
    )
  } else if (comparisons == "all") {
    edges <- t(combn(model_names, 2)) %>%
      tibble::as_tibble() %>%
      magrittr::set_colnames(c("from", "to")) %>%
      dplyr::filter(from != to)
  }

  edges
}

#' @importFrom purrr map_int
#' @importFrom stringr str_starts
.check_align_input <- function(
  models,
  comparisons,
  method
) {
  # check model list input
  stopifnot(typeof(models) == "list")
  stopifnot(
    all(purrr::map(models, ~ class(.) == "list"))
  )
  stopifnot(
    all(purrr::map(models, ~ all(names(.) %in% c("gamma", "beta"))))
  )

  # check models to compare options
  if (!comparisons %in% c("consecutive", "all")) {
    stopifnot(typeof(comparisons) == "matrix")
    stopifnot(ncol(comparisons) == 2)
  }

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
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @export
align_graph <- function(edges, gamma_hats, beta_hats, weight_fun, ...) {
  weights <- list()
  for (i in seq_len(nrow(edges))) {
    pair <- c(edges$from[i], edges$to[i])
    weights[[i]] <- weight_fun(gamma_hats[pair], beta_hats[pair], ...) %>%
      mutate(m = pair[1], m_next = pair[2])
  }
  postprocess_weights(weights, nrow(gamma_hats[[1]]), names(gamma_hats))
}

#' @importFrom purrr map
#' @importFrom magrittr %>%
#' @export
product_weights <- function(gammas, ...) {
  products <- t(gammas[[1]]) %*% gammas[[2]]
  dimnames(products) <- purrr::map(gammas, ~ colnames(.))
  data.frame(products) %>%
    .lengthen_weights()
}

#' @importFrom philentropy JSD
#' @importFrom purrr map
#' @importFrom Barycenter Sinkhorn
#' @export
transport_weights <- function(gammas, betas, reg = 0.1, ...) {
  betas_mat <- do.call(rbind, betas)
  costs <- suppressMessages(philentropy::JSD(betas_mat))
  ix <- seq_len(nrow(betas[[1]]))

  a <- matrix(colSums(gammas[[1]]), ncol = 1)
  b <- matrix(colSums(gammas[[2]]), ncol = 1)
  plan <- Barycenter::Sinkhorn(
    a, b, costs[ix, -ix, drop = F], lambda = reg
  )$Transportplan

  if (any(is.na(plan))) {
    plan <- matrix(0, nrow(betas[[1]]), nrow(betas[[2]]))
    warning("OT diverged, considering increasing regularization.\n")
  }

  dimnames(plan) <- purrr::map(gammas, ~ colnames(.))
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
    rownames_to_column("k_LDA") %>%
    pivot_longer(-k_LDA, names_to = "k_LDA_next", values_to = "weight") %>%
    mutate(k_LDA_next = str_replace(k_LDA_next, "X", ""))
}

#' @importFrom dplyr group_by bind_rows summarize mutate ungroup arrange across
#' @importFrom magrittr %>%
postprocess_weights <- function(weights, n_docs, m_levels) {
  bind_rows(weights) %>%
    group_by(m, m_next, k_LDA, k_LDA_next) %>%
    summarize(document_mass = sum(weight), .groups = "drop") %>%
    mutate(weight = document_mass / n_docs) %>%
    group_by(m_next, k_LDA_next) %>%
    mutate(norm_weight = weight / sum(weight)) %>%
    ungroup() %>%
    mutate(across(c("m", "m_next"), factor, levels = m_levels)) %>%
    mutate(across(c("k_LDA", "k_LDA_next"), as.integer)) %>%
    arrange(m)
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
  cat(sprintf("# … with %s more rows", nrow(object@weights) - 6))
}

#' Alignment Class Definition
#' @import methods
#' @exportClass alignment
setClass("alignment",
  representation(
    weights = "data.frame",
    models = "list",
    n_models = "numeric",
    n_topics = "numeric"
  )
)

#' Show Method for Alignment Class
#' @import methods
#' @export
setMethod("show", "alignment", print_alignment)

setGeneric("weights", function(x) standardGeneric("weights"))
#' Weights Accessor for Alignment Class
#' @import methods
#' @export
setMethod("weights", "alignment", function(x) x@weights)

setGeneric("n_models", function(x) standardGeneric("n_models"))

#' Number of Models Method for Alignment Class
#' @import methods
#' @export
setMethod("n_models", "alignment", function(x) nlevels(x@weights$m))

#' Number of Topics in Alignment
#' @importFrom magrittr %>% set_colnames
#' @importFrom dplyr select bind_rows
.n_topics <- function(x) {
  w <- x@weights
  w1 <- w %>%
    dplyr::select(m, k_LDA)
  w2 <- w %>%
    dplyr::select(m_next, k_LDA_next) %>%
    magrittr::set_colnames(c("m", "k_LDA"))
  nrow(unique(dplyr::bind_rows(w1, w2)))
}

setGeneric("n_topics", function(x) standardGeneric("n_topics"))
#' Number of Topics Method for Alignment Class
#' @import methods
#' @export
setMethod("n_topics", "alignment", .n_topics)

setGeneric("models", function(x) standardGeneric("models"))
#' Extract Models underlying Alignment
#' @import methods
#' @export
setMethod("models", "alignment", function(x) x@models)
