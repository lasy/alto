#' @importFrom dplyr group_by ungroup mutate
reorder_topics <- function(weights, models) {
  weights <-
    weights %>%
    group_by(m, k_LDA) %>%
    mutate(fw_weight = weight / sum(weight)) %>%
    ungroup()
  weights_fw <- forward_ordering(weights)
  weights_bw <- backward_ordering(weights_fw)
  list(weights = weights_bw, models = models)
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
