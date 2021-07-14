
identify_branches <- function(weights){
  models <- levels(weights$m)
  branches <-
    weights %>%
    dplyr::filter(m_next == rev(models)[1]) %>%
    dplyr::select(m_next, k_LDA_next) %>%
    dplyr::distinct() %>%
    dplyr::rename(m = m_next, k_LDA = k_LDA_next) %>%
    dplyr::mutate(branch = k_LDA)

  for (model in rev(models)[-1]) {
    b <-
      weights %>%
      dplyr::filter(m == model) %>%
      dplyr::left_join(
        .,
        branches %>% dplyr::rename(m_next = m, k_LDA_next = k_LDA),
        by = c("m_next", "k_LDA_next")
      ) %>%
      dplyr::arrange(m, k_LDA, -weight) %>%
      dplyr::group_by(m, k_LDA) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(m, k_LDA, branch)
    branches <- dplyr::bind_rows(branches, b)
  }
  branches %>%
    dplyr::arrange(m, k_LDA)

}
