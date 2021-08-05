#' Computes the refinement score of each topic in each model
#'
#' This function computes a refinement score for each topic in an alignment. The
#' refinement score is defined recursively from the leaves. All leaves are given
#' scores of 1. To find the score of an intermediate node, a weighted average is
#' taken of the refinement scores for all descendant nodes, with weights coming
#' from the normalized alignment weights.
#'
#' @param weights (required) \code{data.frame} with the alignment weights
#' (the @weight field from an \code{alignment} object)
#'
#' @seealso align_topics align_branches
#' @return a \code{data.frame}
#' with the refinement score of each topic in each model.
#'
#' @examples
#' library(purrr)
#' data <- rmultinom(10, 20, rep(0.1, 20))
#' lda_params <- setNames(map(1:5, ~ list(k = .)), 1:5)
#' lda_models <- run_lda_models(data, lda_params)
#' alignment <- align_topics(lda_models)
#' compute_refinement_scores(weights(alignment))
#'
#' @importFrom dplyr filter select rename distinct mutate rename left_join
#' group_by summarise arrange bind_rows arrange rename
#' @importFrom magrittr %>%
#' @export
compute_refinement_scores <- function(weights) {

  models <- levels(weights$m)
  scores <- weights %>%
    filter(m_next == rev(models)[1]) %>%
    select(m_next, k_LDA_next) %>%
    rename(m = m_next, k_LDA = k_LDA_next) %>%
    distinct() %>%
    mutate(refinement_score = 1)

  for (model in rev(models)[-1]) {
    s <- weights %>%
      filter(m == model) %>%
      left_join(
        .,
        scores %>% rename(m_next = m, k_LDA_next = k_LDA),
        by = c("m_next", "k_LDA_next")
        ) %>%
      group_by(m, k_LDA) %>%
      mutate(fw_norm_weight = weight / sum(weight)) %>%
      summarise(
        refinement_score =
          sum(fw_norm_weight * norm_weight * refinement_score),
        .groups = "drop"
      )
    scores <- bind_rows(scores, s)
  }
  scores %>%
    arrange(m, k_LDA)
}




compute_refinement_scores_v2 <- function(aligned_topics) {

  topics <-
    bind_rows(
      aligned_topics@weights %>%
        select(m, k_LDA),
      aligned_topics@weights %>%
        select(m_next, k_LDA_next) %>%
        rename(m = m_next, k_LDA = k_LDA_next)

    ) %>%
    distinct() %>%
    arrange(m, k_LDA)


  all_weigh

map_dfr(
  .x = 1:nrow(topics),
  .f = function(i){
    r =

    tibble(m = topics$m[i], k = topics$k_LDA[i], refinement_score = 1)
  }
)



  models <- levels(weights$m)
  scores <- weights %>%
    filter(m_next == rev(models)[1]) %>%
    select(m_next, k_LDA_next) %>%
    rename(m = m_next, k_LDA = k_LDA_next) %>%
    distinct() %>%
    mutate(refinement_score = 1)

  for (model in rev(models)[-1]) {
    s <- weights %>%
      filter(m == model) %>%
      left_join(
        .,
        scores %>% rename(m_next = m, k_LDA_next = k_LDA),
        by = c("m_next", "k_LDA_next")
      ) %>%
      group_by(m, k_LDA) %>%
      mutate(fw_norm_weight = weight / sum(weight)) %>%
      summarise(
        refinement_score =
          sum(fw_norm_weight * norm_weight * refinement_score),
        .groups = "drop"
      )
    scores <- bind_rows(scores, s)
  }
  scores %>%
    arrange(m, k_LDA)
}

