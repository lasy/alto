
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate ungroup
add_summaries <- function(aligned_topics) {
  aligned_topics <- aligned_topics %>%
      add_measure(topic_coherence) %>%
      add_measure(topic_refinement)
  aligned_topics@topics <- aligned_topics@topics %>%
      group_by(m) %>%
      mutate(refinement = refinement * length(unique(k))) %>%
      ungroup() %>%
      remove_summaries()

  aligned_topics
}

#' Removes the refinement scores from the last set of models
#' @importFrom dplyr mutate across if_else
#' @keywords internal
remove_summaries <- function(topics) {
  topics %>%
    mutate(across(
      refinement, # coherence:
      ~ if_else(m == last(levels(m)), NA_real_, .)
    ))
}

#' @importFrom purrr map_dfr
#' @importFrom dplyr left_join
#' @importFrom magrittr %>%
add_measure <- function(aligned_topics, fi=topic_coherence) {
  all_topics <- topics(aligned_topics)
  measure <- map_dfr(
    .x = seq_len(nrow(all_topics)),
    .f = ~ fi(all_topics$m[.], all_topics$k[.], aligned_topics)
  )

  aligned_topics@topics <- all_topics %>%
    left_join(measure, by = c("m", "k"))
  aligned_topics
}

#' Computes the coherence score of a topic
#'
#' The coherence score of a topic reflects how similar this topic is
#' to the other topics of the same path.
#'
#' @param model The index of the model containing the topic whose coherence we
#'   want
#' @param topic The index of the topic whose coherence we want, within the
#'   current model
#' @param aligned_topics An alignment object on which the coherence scores
#'   should be computed.
#'
#' @importFrom dplyr filter select rowwise left_join select rename group_by
#'  summarize
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
#' @keywords internal
topic_coherence <- function(model, topic, aligned_topics) {
  all_topics <- topics(aligned_topics)
  topic_path <- all_topics %>%
    filter(m == model, k == topic) %>%
    pull(path)

  topic_peers <- weights(aligned_topics) %>%
    left_join(
      all_topics %>%
        select(m, k, path),
      by = c("m", "k")) %>%
    left_join(
      all_topics %>%
        select(m, k, path) %>%
        rename(m_next = m, k_next = k, path_next = path),
      by = c("m_next", "k_next")
    ) %>%
    filter(
      ((m == model) & (k == topic)) | ((m_next == model) & (k_next == topic)),
      path == path_next
    )

  if (nrow(topic_peers) > 0) {
    ans <- topic_peers %>%
      rowwise() %>%
      mutate(r_weight = min(fw_weight, bw_weight)) %>%
      group_by(m, m_next) %>%
      summarize(r_weight = sum(r_weight), .groups = "drop") %>%
      summarize(coherence = mean(r_weight)) %>%
      mutate(m = model, k = topic) %>%
      select(m, k, coherence)
  } else {
    ans <- tibble(m = model, k = topic, coherence = 0)
  }

  ans
}




#' Computes the refinement score of a topic
#'
#' The refinement score of a topic reflects the average weight
#' that each of this topic's children attributes to that topic.
#' A topic with a high refinement score is a topic for which
#' all of the subsequent topics connected by some forward weight to
#' this topic have high backward weights to that topic.
#'
#' @param model The index of the model containing the topic whose coherence we
#'   want
#' @param topic The index of the topic whose coherence we want, within the
#'   current model
#' @param aligned_topics An alignment object on which the coherence scores
#'   should be computed.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter mutate group_by summarize
#' @importFrom magrittr %>%
#' @keywords internal
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
