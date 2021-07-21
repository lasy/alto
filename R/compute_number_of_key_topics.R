#' Computes the number of key topics
#'
#' This function computes the number of topics recursively in a backwards
#' fashion. At the final level of the alignment, all topics are considered key
#' topics. At intermediate levels, a topic is considered key if there is an edge
#' leaving it such that the linked topic received most of its weight from this
#' parent edge.
#'
#' Given the weights from an \code{alignment} object, this function computes the
#' number of key topics at each level in the alignment. The result is a
#' \code{data.frame} mapping each level to a number from 1 to the number of
#' current levels.
#'
#' @param weights (required) \code{data.frame} with the alignment weights
#' (the @weight field from an \code{alignment} object).
#' @param plot (optional, default = FALSE) whether to visualize the number of
#' key topics.
#'
#' @seealso align_topics
#' @return a \code{data.frame} with the number of key topic for each model. The
#' \code{n_topics} column shows the total possible number of topics available
#' for that model while \code{n_key_topics} gives the number that passed the key
#' topics criterion.
#'
#' @examples
#' library(purrr)
#' data <- rmultinom(10, 20, rep(0.1, 20))
#' lda_params <- setNames(map(1:5, ~ list(k = .)), 1:5)
#' lda_models <- run_lda_models(data, lda_params)
#' alignment <- align_topics(lda_models)
#' compute_number_of_key_topics(weights(alignment), plot = TRUE)
#'
#' @importFrom ggplot2 ggplot geom_line geom_point aes guides theme_minimal
#' scale_x_continuous scale_y_continuous labs %+%
#' @importFrom dplyr group_by summarise n n_distinct
#' @importFrom magrittr %>%
#' @export
compute_number_of_key_topics <- function(weights, plot = FALSE) {

  branches <- identify_branches(weights)
  n_key_topics <- branches %>%
    group_by(m) %>%
    summarise(
      n_key_topics = n_distinct(branch),
      n_topics = n(),
      .groups = "drop"
    )

  if (plot) {
    g <- ggplot(n_key_topics, aes(x = n_topics)) +
      geom_line(aes(y = n_topics), col = "gray80", size = 3) +
      geom_line(aes(y = n_key_topics)) +
      geom_point(aes(y = n_key_topics, col = n_key_topics < n_topics),
                          size = 3) +
      guides(col = "none") +
      theme_minimal() +
      scale_x_continuous(
        breaks = n_key_topics$n_topics, labels = n_key_topics$m,
        minor_breaks = NULL) +
      scale_y_continuous(
        breaks = n_key_topics$n_topics,
        minor_breaks = NULL) +
        labs(x = "models", y = "# of key topics")

    print(g)
  }

  n_key_topics
}
