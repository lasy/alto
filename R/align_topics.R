#' Align topics from distinct LDA models
#'
#' [Description]
#'
#' @param models (required) a \code{lda_models} object. See
#'   \code{run_lda_models} for details.
#' @param comparisons (optional) either a character indicating if topics
#'   should be aligned between \code{consecutive} or \code{all} models, or a
#'   list of model pairs between which topics should be aligned.
#'
#' @return a \code{data.frame} (? or some specific object) providing the weights
#'   between every pair of topics of each model pairs in the input edgelist
#'   (\code{comparisons}). ? Do we also return the lda_models with ordered
#'   topics?
#' @export
align_topics <- function(
  models,
  comparisons = "consecutive",
  method = "product",
  order_constrain = NULL,
  ...
) {
  check_input(models, comparisons, method)
  weight_fun <- ifelse(method == "product", product_weights, transport_weights)
  if (comparisons == "consecutive") {
    k_ <- length(models)
    comparisons <- data.frame(from = seq_len(k_ - 1), to = seq(2, k_))
  }

  alignment <- align_graph(
    comparisons,
    models$gammas,
    models$betas,
    weight_fun, ...
  )

  # 3. ORDER TOPICS

  # 4. Return results
  new(
    "alignment",
    weights = as.data.frame(alignment)
  )
}


#' @importFrom purrr map_int
check_input <- function(
  models,
  comparisons,
  method
) {
  # check model list input
  stopifnot(typeof(models) == "list")
  stopifnot(names(models) == c("betas", "gammas", "log_likelihood"))
  stopifnot(all(purrr::map_int(models, ~ typeof(.) == "list")))

  # check models to compare options
  if (!comparisons %in% c("consecutive", "all")) {
    stopifnot(typeof(comparisons) == "matrix")
    stopifnot(ncol(comparisons) == 2)
  }

  # check method used
  match.arg(tolower(method), c("product", "transport"))
}

#' Alignment between Pairs of Topics
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @export
align_graph <- function(edges, gamma_hats, beta_hats, weight_fun, ...) {
  weights <- list()
  for (i in seq_len(nrow(edges))) {
    pair <- c(edges[i, 1], edges[i, 2])
    weights[[i]] <- weight_fun(gamma_hats[pair], beta_hats[pair], ...) %>%
      mutate(m = pair[1], m_next = pair[2])
  }
  postprocess_weights(weights, nrow(gamma_hats[[1]]), names(gamma_hats))
}

#' @importFrom purrr map
#' @importFrom magrittr %>%
product_weights <- function(gammas, ...) {
  products <- t(gammas[[1]]) %*% gammas[[2]]
  dimnames(products) <- purrr::map(gammas, ~ colnames(.))
  data.frame(products) %>%
    .lengthen_weights()
}

transport_weights <- function(gammas, ...) {

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
    arrange(m)
}

################################################################################
# Class construction and methods
################################################################################
print_alignment <- function(x) {
  cat(
    "An object of class ", class(x),
    " comparing ",
    n_models(x),
    " models across ",
    n_topics(x),
    " topics\n"
  )

  cat("#################################################################\n")
  print(head(x@weights))
  cat("....")
  cat("#################################################################\n")
}

plot_alignment <- function(x, y, ...) {
  warning("plotting method is not implemented yet")
}

#' Alignment Class Definition
#' @import methods
#' @exportClass alignment
setClass("alignment",
  representation(
    weights = "data.frame",
    n_models = "numeric",
    n_topics = "numeric"
  )
)

#' Plot Method for Alignment Class
#' @import methods
#' @export
setMethod("plot", c(x = "alignment", y = "missing"), plot_alignment)

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
setMethod("n_models", "alignment", function(x) length(unique(x@weights$m, x@weights$m_next)))

setGeneric("n_topics", function(x) standardGeneric("n_topics"))
#' Number of Topics Method for Alignment Class
#' @import methods
#' @export
setMethod("n_topics", "alignment", function(x) length(unique(x@weights$k_LDA, x@weights$k_LDA_next)))