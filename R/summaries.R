
#' @importFrom magrittr %>%
add_summaries <-  function(aligned_topics) {
  aligned_topics %>%
    add_measure(topic_coherence) %>%
    add_measure(topic_refinement)
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
#' to the other topics of the same branch (same key topic).
#' It is computed as
#'
#' $$c(v_{k,m}) = \frac{1}{|M'|} \sum_{m' \in M'} \sum_{k' \in K'_{m'}}
#' \min\left(\tilde{w}_{\text{in}}\left(v_{k,m}, v_{k',m'}\right),
#' \tilde{w}_{\text{out}}\left(v_{k,m}, v_{k',m'}\right) \right)$$
#'
#' where where $M'$ is the set of models with
#' topics belonging to the same branch as $v_{k,m}$ and
#' $K'_{m'}$ is the set of topics of that branch in model $m'$.
#'
#' @importFrom dplyr filter select rowwise left_join select rename group_by
#'  summarize
#' @importFrom magrittr %>%
#' @importFrom tibble tibble
topic_coherence <- function(model, topic, aligned_topics) {
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
#' It is computed as
#'
#' $$r\left(v\right) =
#' \frac{1}{L-l}\sum_{l'=l+1}^L \sum_{v_{l'}^\prime \in V_{l'}}
#'  \tilde{w}_{\text{out}}\left(v, v_{l'}^\prime\right)
#'  \tilde{w}_{\text{in}}\left(v, v_{l'}^\prime\right)$$
#'
#' where $\tilde{w}_{\text{in}}$ and $\tilde{w}_{\text{out}}$
#' are the normalized weights (backward and forward weights respectively).
#'
#'
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
