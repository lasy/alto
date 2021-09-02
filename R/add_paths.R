

#' @importFrom dplyr last bind_rows rename mutate filter group_by arrange
#' slice_max left_join select
add_paths <- function(aligned_topics, weight_fun, ...) {
  model_names <- names(models(aligned_topics))

  # initializing paths
  paths <- topics(aligned_topics) %>%
    filter(m == last(model_names)) %>%
    mutate(path = k) %>%
    select(m, k, path)

  w <- weights(aligned_topics)
  for (model in rev(model_names)[-1]) {
    paths_m <- w %>%
      filter(m == model) %>%
      mutate(match_weight = 0.5 * fw_weight + 0.5 * bw_weight) %>%
      group_by(k) %>%
      slice_max(match_weight) %>%
      left_join(paths, by = c("k_next" = "k", "m_next" = "m")) %>%
      select(m, k, path)
    paths <- bind_rows(paths, paths_m)
  }

  paths <- paths %>%
    arrange(m, k) %>%
    mutate(path = factor(path, levels = sort(unique(path))))

  aligned_topics@topics <- aligned_topics@topics %>%
    left_join(paths, by = c("m", "k"))

  aligned_topics
}
