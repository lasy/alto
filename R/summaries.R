add_summaries <-  function(aligned_topics) {
  aligned_topics <- add_robustness(aligned_topics)
  aligned_topics <- add_refinement(aligned_topics)
}


add_robustness <- function(aligned_topics) {

  robustness <-
    map_dfr(
      .x = 1:nrow(aligned_topics@topics),
      .f = function(i){
        compute_robustness_of_topic(model = aligned_topics@topics$m[i],
                                    topic = aligned_topics@topics$k[i],
                                    aligned_topics = aligned_topics)
      }
    )

  aligned_topics@topics <-
    aligned_topics@topics %>%
    left_join(robustness, by = c("m","k"))

  aligned_topics
}


compute_robustness_of_topic <- function(model, topic, aligned_topics) {

  topic_branch <-
    aligned_topics@topics %>%
    filter(m == model, k == topic) %>%
    select(branch) %>%
    unlist()

  this_topic_peers <-
    aligned_topics@weights %>%
    left_join(aligned_topics@topics %>%  select(m, k, branch), by = c("m", "k")) %>%
    left_join(aligned_topics@topics %>%
                select(m, k, branch) %>%
                rename(m_next = m, k_next = k, branch_next = branch),
              by = c("m_next", "k_next")) %>%
    filter(
      ((m == model) & (k == topic)) |
        ((m_next == model) & (k_next == topic)),
      branch == branch_next
    )

  if(nrow(this_topic_peers) > 0){
    ans <-
      this_topic_peers %>%
      rowwise() %>%
      mutate(r_weight = min(fw_weight, bw_weight)) %>%
      group_by(m, m_next) %>%
      summarize(r_weight = sum(r_weight), .groups = "drop") %>%
      summarize(robustness = mean(r_weight)) %>%
      mutate(m = model, k = topic) %>%
      select(m, k, robustness)

  } else{
    ans <-
      tibble(m = model, k = topic, robustness = 0)
  }
  ans
}




add_refinement <- function(aligned_topics) {

  refinement <-
    map_dfr(
      .x = 1:nrow(aligned_topics@topics),
      .f = function(i){
        compute_refinement_of_topic(model = aligned_topics@topics$m[i],
                                    topic = aligned_topics@topics$k[i],
                                    aligned_topics = aligned_topics)
      }
    )

  aligned_topics@topics <-
    aligned_topics@topics %>%
    left_join(refinement, by = c("m","k"))

  aligned_topics
}




compute_refinement_of_topic <- function(model, topic, aligned_topics) {

  this_topic_descendants <-
    aligned_topics@weights %>%
    filter(m == model, k == topic)

  if(nrow(this_topic_descendants) > 0){
    ans <-
      this_topic_descendants %>%
      mutate(r = fw_weight * bw_weight) %>%
      group_by(m, k, m_next) %>%
      summarize(r = sum(r), .groups = "drop") %>%
      group_by(m, k) %>%
      summarize(refinement = mean(r), .groups = "drop")
  } else{
    ans <-
      tibble(m = model, k = topic, refinement = 1)
  }
  ans
}


