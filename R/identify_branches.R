#' Identifies topic branches along the topic alignment graph
#'
#' [Description]
#'
#' @param weights (required) \code{data.frame} with the alignment weights
#' (the @weight field from an \code{alignment} object)
#'
#' @seealso align_topics
#' @return a \code{data.frame}
#' identifying the branch of each topic in each model.
#' @export
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
