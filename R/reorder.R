
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

perms <- function(x) {
  if (length(x) == 1) {
    return(x)
  }

  res <- matrix(nrow = 0, ncol = length(x))
  for (i in seq_along(x)) {
    res <- rbind(res, cbind(x[i], Recall(x[-i])))
  }
  res
}

#' Naive Optimization
#'
#' This just tries all the permutations and picks one with the lowest cost.
#' @export
optimize_permutation <- function(weights) {
  cw <- cost_fun(weights)
  target <- unique(weights$k_LDA_next)
  all_pi <- perms(target)

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
reorder_topics <- function(weights) {
  sources <- unique(weights$m)
  for (i in seq_along(sources)) {
    ix <- which(weights$m == sources[i])
    pi_star <- weights[ix, ] %>%
      select(k_LDA, k_LDA_next, weight) %>%
      optimize_permutation()

    # reorder k_LDA_next
    weights[ix, ]$k_LDA_next <- reorder_helper(weights, ix, pi_star)

    # reorder the next m's k_LDA, to match the new k_LDA_next
    if (i < length(sources)) {
      ix <- which(weights$m == sources[i + 1])
      weights[ix, ]$k_LDA <- reorder_helper(weights, ix, pi_star, "k_LDA")
    }
  }

  weights
}

reorder_helper <- function(weights, ix, pi_star, type = "k_LDA_next") {
  if (type == "k_LDA_next") {
    return(pi_star[as.character(weights[ix, ]$k_LDA_next)])
  }
  pi_star[as.character(weights[ix, ]$k_LDA)]
}
