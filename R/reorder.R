#' @importFrom dplyr group_by ungroup mutate select rename filter bind_rows
topic_ordering <- function(weights) {
  weights <- weights %>%
    group_by(m, k_LDA) %>%
    mutate(
      k_LDA_init = k_LDA_next,
      fw_weight = weight / sum(weight)
    ) %>%
    ungroup()

  weights_bw <- weights %>%
    forward_ordering() %>%
    backward_ordering()

  permuted <- weights_bw %>%
    select(m_next, k_LDA_init, k_LDA_next) %>%
    rename(k_LDA = k_LDA_next, m = m_next)

  root <- weights_bw %>%
    filter(!(m %in% weights_bw$m_next)) %>%
    select(m, k_LDA) %>%
    mutate(k_LDA_init = k_LDA)

  unique(bind_rows(root, permuted))
}

#' @importFrom magrittr %>%
#' @importFrom dplyr left_join select rename
reorder_weights <- function(weights, perms) {
  weights %>%
    # reorder the k_LDA's
    left_join(
      perms %>% rename(k_LDA_final = k_LDA),
      by = c("k_LDA" = "k_LDA_init", "m" = "m")
    ) %>%
    select(-k_LDA) %>%
    rename(k_LDA = k_LDA_final) %>%
    # reorder the k_LDA_next's
    left_join(
      perms %>% rename(k_LDA_next_final = k_LDA),
      by = c("k_LDA_next" = "k_LDA_init", "m_next" = "m")
    ) %>%
    select(-k_LDA_next) %>%
    rename(k_LDA_next = k_LDA_next_final) %>%
    select(m, m_next, k_LDA, k_LDA_next, everything())
}

#' @importFrom magrittr %>%
#' @importFrom dplyr filter arrange pull
reorder_models <- function(models, perms) {
  result <- list()
  for (i in seq_along(models)) {
    m_ <- names(models)[i]
    pi <- perms %>%
      filter(m == m_) %>%
      arrange(k_LDA_init) %>%
      pull(k_LDA)

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
