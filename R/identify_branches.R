

#' @importFrom dplyr last bind_rows rename mutate filter group_by arrange
#' slice_head left_join select
add_branches <- function(aligned_topics, weight_fun, ...) {
  model_names <- names(aligned_topics@models)

  # initializing branches
  branches <- aligned_topics@topics %>%
    filter(m == last(model_names)) %>%
    mutate(branch = k) %>%
    select(m, k, branch)

  w <- aligned_topics@weights
  for (model in rev(model_names)[-1]) {
    branches_m <- w %>%
      filter(m == model) %>%
      mutate(match_weight = 0.5 * fw_weight + 0.5 * bw_weight) %>%
      group_by(k) %>%
      arrange(k, -match_weight) %>%
      slice_head(n = 1) %>%
      left_join(.,
                rename(branches, k_next = k, m_next = m),
                by = c("m_next", "k_next")) %>%
      select(m, k, branch)
    branches <- bind_rows(branches, branches_m)
  }

  branches <- branches %>%
    arrange(m, k) %>%
    mutate(branch = factor(branch, levels = sort(unique(branch))))

  aligned_topics@topics <- aligned_topics@topics %>%
    left_join(branches, by = c("m", "k"))

  aligned_topics
}
