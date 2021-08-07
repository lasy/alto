
#' @importFrom magrittr %>%
add_summaries <-  function(aligned_topics) {
  aligned_topics %>%
    add_measure(topic_robustness) %>%
    add_measure(topic_refinement)
}

#' @importFrom purrr map_dfr
#' @importFrom dplyr left_join
#' @importFrom magrittr %>%
add_measure <- function(aligned_topics, fi=topic_robustness) {
  all_topics <- topics(aligned_topics)
  measure <- map_dfr(
    .x = seq_len(nrow(all_topics)),
    .f = ~ fi(all_topics$m[.], all_topics$k[.], aligned_topics)
  )

  aligned_topics@topics <- all_topics %>%
    left_join(measure, by = c("m", "k"))
  aligned_topics
}

#' @importFrom dplyr filter select rowwise left_join select rename group_by
#'  summarize
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
topic_robustness <- function(model, topic, aligned_topics) {
  all_topics <- topics(aligned_topics)
  topic_branch <- all_topics %>%
    filter(m == model, k == topic) %>%
    pull(branch)

  topic_peers <- weights(aligned_topics) %>%
    left_join(
      all_topics %>%
        select(m, k, branch),
      by = c("m", "k")) %>%
    left_join(
      all_topics %>%
        select(m, k, branch) %>%
        rename(m_next = m, k_next = k, branch_next = branch),
      by = c("m_next", "k_next")
    ) %>%
    filter(
      ((m == model) & (k == topic)) | ((m_next == model) & (k_next == topic)),
      branch == branch_next
    )

  if (nrow(topic_peers) > 0) {
    ans <- topic_peers %>%
      rowwise() %>%
      mutate(r_weight = min(fw_weight, bw_weight)) %>%
      group_by(m, m_next) %>%
      summarize(r_weight = sum(r_weight), .groups = "drop") %>%
      summarize(robustness = mean(r_weight)) %>%
      mutate(m = model, k = topic) %>%
      select(m, k, robustness)
  } else {
    ans <- tibble(m = model, k = topic, robustness = 0)
  }

  ans
}

#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate group_by summarize
#' @importFrom magrittr %>%
topic_refinement <- function(model, topic, aligned_topics) {
  topic_descendants <- aligned_topics@weights %>%
    filter(m == model, k == topic)

  if (nrow(topic_descendants) > 0) {
    ans <- topic_descendants %>%
      mutate(r = fw_weight * bw_weight) %>%
      group_by(m, k, m_next) %>%
      summarize(r = sum(r), .groups = "drop") %>%
      group_by(m, k) %>%
      summarize(refinement = mean(r), .groups = "drop")
  } else {
    ans <- tibble(m = model, k = topic, refinement = 1)
  }

  ans
}
