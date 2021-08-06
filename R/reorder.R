#' @importFrom dplyr group_by ungroup mutate select rename filter bind_rows
#'   distinct
topic_ordering <- function(weights) {
  permuted <- weights %>%
    select(m_next, k_init, k_next) %>%
    rename(k = k_next, m = m_next)

  root <- weights %>%
    filter(!(m %in% weights$m_next)) %>%
    select(m, k) %>%
    mutate(k_init = k)

  bind_rows(root, permuted) %>%
    distinct() %>%
    arrange(m, k) %>%
    split(.$m) %>%
    map(~ pull(., k_init))
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

#' @importFrom magrittr %>%
#' @importFrom dplyr tibble left_join select rename everything
reorder_topics <- function(topics, perms){
  perms <- perms_as_tibble(perms)
  topics <-
    topics %>%
    left_join(perms, by = c("m", "k")) %>%
    select(-k) %>%
    rename(k = new_k) %>%
    arrange(m, k) %>%
    select(m, k, everything())
  topics
}

#' @importFrom magrittr %>%
#' @importFrom dplyr tibble left_join select rename everything
reorder_weights <- function(weights, perms) {
  perms <- perms_as_tibble(perms)
  weights <-
    weights %>%
    left_join(perms, by = c("m", "k")) %>%
    select(-k) %>%
    rename(k = new_k) %>%
    left_join(perms %>% rename(m_next = m, k_next = k), by = c("m_next", "k_next")) %>%
    select(-k_next) %>%
    rename(k_next = new_k) %>%
    arrange(m, m_next, k, k_next) %>%
    select(m, m_next, k, k_next, everything())
}

#' @importFrom purrr map_dfr
#' @importFrom dplyr mutate
perms_as_tibble <- function(perms) {
  map_dfr(
    .x = names(perms),
    .f = function(model){
      tibble(m = model,
             k = seq_along(perms[[model]]),
             new_k = order(perms[[model]]))
    }
  ) %>%
    mutate(m = m %>% factor(., levels = names(perms)))
}

#' @importFrom dplyr filter group_by ungroup mutate summarize arrange
#' @importFrom magrittr %>%
forward_ordering <- function(weights) {
  models <- weights$m_next %>% unique() %>% sort()

  for (model in models) {

    this_model_weights = pre_post_weights(weights, model)
    k_order <-
      this_model_weights %>%
      mutate(force = 0.7 * bw_weight * k_prev + 0 * fw_weight * k_next) %>%
      group_by(k, topic_weight) %>%
      summarize(gravity_center = sum(force), .groups = "drop") %>%
      arrange(gravity_center, topic_weight) %>%
      mutate(new_k = row_number()) %>%
      arrange(k)

    weights <-
      weights %>%
      mutate(
        k_next =
          ifelse(m_next == model,
                 k_order$new_k[k_next],
                 k_next),
        k =
          ifelse(m == model,
                 k_order$new_k[k],
                 k)
      )
  }
  weights
}

#' @importFrom dplyr filter group_by ungroup mutate summarize arrange
#' @importFrom magrittr %>%
backward_ordering <- function(weights) {
  models <- weights$m %>% unique() %>% sort()

  for (model in rev(models[-1])) {

    this_model_weights = pre_post_weights(weights, model)

    k_order <-
      this_model_weights %>%
      mutate(force = 0 * bw_weight * k_prev + 0.7 * fw_weight * k_next) %>%
      group_by(k, topic_weight) %>%
      summarize(gravity_center = sum(force), .groups = "drop") %>%
      arrange(gravity_center, topic_weight) %>%
      mutate(new_k = row_number()) %>%
      arrange(k)

    weights <-
      weights %>%
      mutate(
        k_next =
          ifelse(m_next == model,
                 k_order$new_k[k_next],
                 k_next),
        k =
          ifelse(m == model,
                 k_order$new_k[k],
                 k)
      )
  }
  weights
}



pre_post_weights <- function(weights, model){
  pre_weights <-
    weights %>%
    filter(m_next == model) %>%
    dplyr::rename(k_prev = k, k = k_next) %>%
    select(k_prev, k, bw_weight)

  post_weights <-
    weights %>%
    filter(m == model) %>%
    dplyr::rename(k = k, k_next = k_next) %>%
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

  topic_weights <-
    weights %>%
    filter(m == model) %>%
    group_by(k) %>%
    summarize(topic_weight = sum(weight), .groups = "drop") %>%
    dplyr::rename(k = k)

  this_model_weights <-
    this_model_weights %>%
    left_join(topic_weights, by = "k") %>%
    select(k_prev, k, k_next, bw_weight, fw_weight, topic_weight)

  this_model_weights
}
