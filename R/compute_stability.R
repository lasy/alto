


compute_stability_along_branches <- function(weights) {

  branches <-
    identify_branches(weights)

  weights_with_branches <-
    weights %>%
    dplyr::left_join(
      .,
      branches,
      by = c("m", "k_LDA")
    ) %>%
    dplyr::left_join(
      .,
      branches %>%
        dplyr::rename(k_LDA_next = k_LDA, m_next = m, branch_next = branch),
      by = c("m_next", "k_LDA_next")
    )

  weights_along_branches <-
    weights_with_branches %>%
    dplyr::filter(branch == branch_next) %>%
    dplyr::group_by(m, m_next, branch, branch_next, k_LDA_next) %>%
    dplyr::summarize(norm_weight = sum(norm_weight), .groups = "drop") %>%
    dplyr::group_by(m, m_next, branch, branch_next) %>%
    dplyr::summarize(norm_weight = mean(norm_weight), .groups = "drop")

  stability <-
    branches %>%
    dplyr::select(m, branch) %>%
    dplyr::arrange(branch, m) %>%
    dplyr::group_by(branch) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(stability = 1) %>%
    dplyr::arrange(m)

  for(model in levels(weights$m)){

    s <-
      weights_along_branches %>%
      dplyr::filter(m == model) %>%
      dplyr::left_join(., stability %>%  filter(m == model), by = c("m", "branch")) %>%
      dplyr::mutate(stability = stability * norm_weight) %>%
      dplyr::select(m_next, branch, stability) %>%
      dplyr::rename(m = m_next)

    stability <-  dplyr::bind_rows(stability, s)


  }

  stability %>% dplyr::arrange(branch, m)
}


