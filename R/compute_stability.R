#' Computes stability score along branches
#'
#' Stability is defined as the product of weights along a branch. Therefore, if
#' weights are concentrated on specific descendants, then a branch is stable.
#' This metric is useful for identifying topics that are recovered in models
#' across a range of values of K (they are "stable" to changes in the K
#' parameter).
#'
#' @param weights (required) \code{data.frame} with the alignment weights
#' (the @weight field from an \code{alignment} object)
#'
#' @seealso align_topics align_branches
#' @return a \code{data.frame} with the stability score of each branch along
#' k's.
#' @examples
#' library(purrr)
#' data <- rmultinom(10, 20, rep(0.1, 20))
#' lda_params <- setNames(map(1:5, ~ list(k = .)), 1:5)
#' lda_models <- run_lda_models(data, lda_params)
#' alignment <- align_topics(lda_models)
#' compute_stability_along_branches(weights(alignment))
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join rename filter group_by summarize select arrange
#' slice_head ungroup mutate bind_rows
#' @export
compute_stability_along_branches <- function(weights) {
  branches <-
    identify_branches(weights)

  weights_with_branches <-
    weights %>%
    left_join(
      .,
      branches,
      by = c("m", "k_LDA")
    ) %>%
    left_join(
      .,
      branches %>%
        rename(k_LDA_next = k_LDA, m_next = m, branch_next = branch),
      by = c("m_next", "k_LDA_next")
    )

  weights_along_branches <-
    weights_with_branches %>%
    filter(branch == branch_next) %>%
    group_by(m, m_next, branch, branch_next, k_LDA_next) %>%
    summarize(norm_weight = sum(norm_weight), .groups = "drop") %>%
    group_by(m, m_next, branch, branch_next) %>%
    summarize(norm_weight = mean(norm_weight), .groups = "drop")

  stability <-
    branches %>%
    select(m, branch) %>%
    arrange(branch, m) %>%
    group_by(branch) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    mutate(stability = 1) %>%
    arrange(m)

  for (model in levels(weights$m)) {

    s <-
      weights_along_branches %>%
      filter(m == model) %>%
      left_join(., stability %>%  filter(m == model), by = c("m", "branch")) %>%
      mutate(stability = stability * norm_weight) %>%
      select(m_next, branch, stability) %>%
      rename(m = m_next)

    stability <-  bind_rows(stability, s)
  }

  stability %>% arrange(branch, m)
}
