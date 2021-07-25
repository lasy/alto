#' @importFrom dplyr group_by ungroup mutate select rename filter bind_rows
topic_ordering <- function(weights) {
  permuted <- weights %>%
    select(m_next, k_LDA_init, k_LDA_next) %>%
    rename(k_LDA = k_LDA_next, m = m_next)

  root <- weights %>%
    filter(!(m %in% weights$m_next)) %>%
    select(m, k_LDA) %>%
    mutate(k_LDA_init = k_LDA)

  bind_rows(root, permuted) %>%
    distinct() %>%
    arrange(m, k_LDA) %>%
    split(.$m) %>%
    map(~ pull(., k_LDA_init))
}

#' @importFrom magrittr %>%
#' @importFrom dplyr filter arrange pull
reorder_models <- function(models, perms) {
  result <- list()
  for (i in seq_along(models)) {
    m_ <- names(models)[i]
    pi <- perms[[m_]]
    result[[m_]] <- list(
      beta = models[[m_]]$beta[pi, , drop = F],
      gamma = models[[m_]]$gamma[, pi, drop = F]
    )
  }

  result
}

#' @importFrom dplyr filter group_by ungroup mutate summarize arrange
#' @importFrom magrittr %>%
forward_ordering <- function(weights) {
  models <- weights$m_next %>% unique() %>% sort()

  for (model in models) {
    this_trans_weights <-
      weights %>%
      filter(m_next == model) %>%
      mutate(force = norm_weight * k_LDA)
    k_LDA_next_order <-
      this_trans_weights %>%
      group_by(k_LDA_next) %>%
      summarize(force = sum(force), .groups = "drop") %>%
      arrange(force) %>%
      mutate(new_k_LDA_next = row_number()) %>%
      arrange(k_LDA_next)

    weights <-
      weights %>%
      mutate(
        k_LDA_next =
          ifelse(m_next == model,
                 k_LDA_next_order$new_k_LDA_next[k_LDA_next],
                 k_LDA_next),
        k_LDA =
          ifelse(m == model,
                 k_LDA_next_order$new_k_LDA_next[k_LDA],
                 k_LDA)
      )
  }
  weights
}

#' @importFrom dplyr filter group_by ungroup mutate summarize arrange
#' @importFrom magrittr %>%
backward_ordering <- function(weights) {
  models <- weights$m %>% unique() %>% sort()

  for (model in rev(models)) {
    this_trans_weights <-
      weights %>%
      filter(m == model) %>%
      mutate(force = fw_weight * k_LDA)
    k_LDA_order <-
      this_trans_weights %>%
      group_by(k_LDA) %>%
      summarize(force = sum(force), .groups = "drop") %>%
      arrange(force) %>%
      mutate(new_k_LDA = row_number()) %>%
      arrange(k_LDA)

    weights <-
      weights %>%
      mutate(
        k_LDA_next =
          ifelse(m_next == model,
                 k_LDA_order$new_k_LDA[k_LDA_next],
                 k_LDA_next),
        k_LDA =
          ifelse(m == model,
                 k_LDA_order$new_k_LDA[k_LDA],
                 k_LDA)
      )
  }
  weights
}
