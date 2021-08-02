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

    this_model_weights = get_pre_and_post_weights(weights, model)

    k_order <-
      this_model_weights %>%
      mutate(force = 0.7 * bw_weight * k_prev + 0.3 * fw_weight * k_next) %>%
      group_by(k) %>%
      summarize(gravity_center = sum(force), .groups = "drop") %>%
      arrange(gravity_center) %>%
      mutate(new_k = row_number()) %>%
      arrange(k)

    weights <-
      weights %>%
      mutate(
        k_LDA_next =
          ifelse(m_next == model,
                 k_order$new_k[k_LDA_next],
                 k_LDA_next),
        k_LDA =
          ifelse(m == model,
                 k_order$new_k[k_LDA],
                 k_LDA)
      )
  }
  weights
}


#' @importFrom dplyr filter group_by ungroup mutate summarize arrange
#' @importFrom magrittr %>%
backward_ordering <- function(weights) {
  models <- weights$m %>% unique() %>% sort()

  for (model in rev(models[-1])) {

    this_model_weights = get_pre_and_post_weights(weights, model)

    k_order <-
      this_model_weights %>%
      mutate(force = 0.3 * bw_weight * k_prev + 0.7 * fw_weight * k_next) %>%
      group_by(k) %>%
      summarize(gravity_center = sum(force), .groups = "drop") %>%
      arrange(gravity_center) %>%
      mutate(new_k = row_number()) %>%
      arrange(k)

    weights <-
      weights %>%
      mutate(
        k_LDA_next =
          ifelse(m_next == model,
                 k_order$new_k[k_LDA_next],
                 k_LDA_next),
        k_LDA =
          ifelse(m == model,
                 k_order$new_k[k_LDA],
                 k_LDA)
      )
  }
  weights
}



get_pre_and_post_weights = function(weights, model){

  pre_weights <-
    weights %>%
    filter(m_next == model) %>%
    dplyr::rename(k_prev = k_LDA, k = k_LDA_next, bw_weight = norm_weight) %>%
    select(k_prev, k, bw_weight)

  post_weights <-
    weights %>%
    filter(m == model) %>%
    dplyr::rename(k = k_LDA, k_next = k_LDA_next) %>%
    select(k, k_next, fw_weight)

  if (nrow(post_weights) > 0) {
    this_model_weights <-
      left_join(
        pre_weights,
        post_weights,
        by = "k"
      )
  } else {
    this_model_weights <-
      pre_weights %>%
      mutate(k_next = 0,
             fw_weight = 0)
  }
  this_model_weights
}



#' @importFrom dplyr filter group_by ungroup mutate summarize arrange
#' @importFrom magrittr %>%
forward_ordering_v1 <- function(weights) {
  models <- weights$m_next %>% unique() %>% sort()

  for (model in models) {
    this_trans_weights <-
      weights %>%
      filter(m_next == model) %>%
      mutate(force = norm_weight * k_LDA) # norm_weight
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
backward_ordering_v1 <- function(weights) {
  models <- weights$m %>% unique() %>% sort()

  for (model in rev(models)) {
    this_trans_weights <-
      weights %>%
      filter(m == model) %>%
      mutate(force = fw_weight * k_LDA_next) # fw_weight
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
