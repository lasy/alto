

compute_refinement_scores <- function(weights) {

  models <- levels(weights$m)
  scores <-
    weights %>%
    dplyr::filter(m_next == rev(models)[1]) %>%
    dplyr::select(m_next, k_LDA_next) %>%
    dplyr::rename(m = m_next, k_LDA = k_LDA_next) %>%
    dplyr::distinct() %>%
    dplyr::mutate(refinement_score = 1)

  for (model in rev(models)[-1]) {
    s <-
      weights %>%
      dplyr::filter(m == model) %>%
      dplyr::left_join(
        .,
        scores %>% dplyr::rename(m_next = m, k_LDA_next = k_LDA),
        by = c("m_next", "k_LDA_next")
        ) %>%
      dplyr::group_by(m, k_LDA) %>%
      dplyr::mutate(fw_norm_weight = weight / sum(weight)) %>%
      dplyr::summarise(
        refinement_score =
          sum(fw_norm_weight * norm_weight * refinement_score),
        .groups = "drop"
      )
    scores <- bind_rows(scores, s)
  }
  scores %>%
    dplyr::arrange(m, k_LDA)
}
