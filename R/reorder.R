
#' Cost Function associated with Weights
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join mutate pull
cost_fun <- function(weights) {
  function(pi) {
    weights %>%
      left_join(pi, by = c(k_LDA_next = "pi_v")) %>%
      mutate(
        diff = abs(k_LDA - v),
        cost = weight * diff
     ) %>%
      pull(cost) %>%
      sum()
  }
}

#' @importFrom dplyr select group_by slice_max
high_weight_pairs <- function(weights, top_n=3) {
  weights %>%
    group_by(k_LDA) %>%
    slice_max(weight, n = top_n)
}

#' @importFrom dplyr mutate row_number select
#' @importFrom tidyr pivot_wider
pairs_to_matrix <- function(weight_pairs) {
  weight_mat <- weight_pairs %>%
    mutate(ix = row_number()) %>%
    select(-weight) %>%
    pivot_wider(names_from = ix, values_from = k_LDA_next) %>%
    ungroup() %>%
    select(-k_LDA) %>%
    t()
  rownames(weight_mat) <- NULL
  weight_mat
}

matrix_paths <- function(weight_mat) {
  paths <- lapply(weight_mat[, 1], identity)
  for (j in seq_len(ncol(weight_mat))) {
    extended <- list()
    for (elem in paths) {
      for (i in seq_len(nrow(weight_mat))) {
        extended <- append(extended, list(c(elem, weight_mat[i, j])))
      }
    }
    paths <- extended
  }
  paths
}

filter_paths <- function(paths) {
  candidates <- list()
  for (i in seq_along(paths)) {
    if (max(table(paths[[i]])) == 1) {
      candidates <- append(candidates, paths[i])
    }
  }
  candidates <- append(candidates, list(seq_along(paths[[1]])))
  do.call(rbind, candidates)
}

candidate_perms <- function(weights, top_n=3, max_perms=1e6) {
  all_pi <- high_weight_pairs(weights, top_n) %>%
    pairs_to_matrix() %>%
    matrix_paths() %>%
    filter_paths()
  if (nrow(all_pi) > max_perms) {
    all_pi <- all_pi[sample(nrow(all_pi), max_perms), ]
  }

  all_pi
}

#' Permutation Optimization
#'
#' This tries all the permutations and picks one with the lowest cost.
#' @export
optimize_permutation <- function(weights, top_n=3) {
  cw <- cost_fun(weights)
  target <- unique(weights$k_LDA_next)
  all_pi <- candidate_perms(weights, top_n)

  pi <- data.frame(v = target, pi_v = all_pi[1, ])
  costs <- vector(length = nrow(all_pi))
  for (i in seq_len(nrow(all_pi))) {
    pi$pi_v <- all_pi[i, ]
    costs[i] <- cw(pi)
  }

  setNames(target, all_pi[which.min(costs), ])
}

#' @importFrom magrittr %>%
#' @importFrom dplyr select
reorder_topics <- function(weights, models, top_n=3) {
  sources <- unique(weights$m)
  for (i in seq_along(sources)) {
    ix <- which(weights$m == sources[i])
    pi_star <- weights[ix, ] %>%
      select(k_LDA, k_LDA_next, weight) %>%
      optimize_permutation(top_n)

    # reorder k_LDA_next
    weights[ix, ]$k_LDA_next <- reorder_helper(weights, ix, pi_star)

    # reorder the next m's k_LDA, to match the new k_LDA_next
    if (i < length(sources)) {
      ix <- which(weights$m == sources[i + 1])
      weights[ix, ]$k_LDA <- reorder_helper(weights, ix, pi_star, "k_LDA")

      pi_ <- pi_star[sort(names(pi_star))]
      models[[i + 1]]$beta <- models[[i + 1]]$beta[pi_, ]
      models[[i + 1]]$gamma <- models[[i + 1]]$gamma[, pi_]
    }
  }

  list(weights = weights, models = models)
}

reorder_helper <- function(weights, ix, pi_star, type = "k_LDA_next") {
  if (type == "k_LDA_next") {
    return(pi_star[as.character(weights[ix, ]$k_LDA_next)])
  }
  pi_star[as.character(weights[ix, ]$k_LDA)]
}
