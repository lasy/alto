



compute_number_of_key_topics <- function(weights, plot = FALSE){

  branches <-
    identify_branches(weights)

  n_key_topics <-
    branches %>%
    dplyr::group_by(m) %>%
    dplyr::summarise(
      n_key_topics = length(unique(branch)),
      n_topics = n(),
      .groups = "drop"
    )

  if (plot) {
    g <-
      ggplot2::ggplot(n_key_topics,aes(x = n_topics) ) +
      ggplot2::geom_line(aes(y = n_topics), col = "gray80", size = 3) +
      ggplot2::geom_line(aes(y = n_key_topics)) +
      ggplot2::geom_point(aes(y = n_key_topics, col = n_key_topics < n_topics),
                          size = 3) +
      ggplot2::guides(col = "none") +
      ggplot2::theme_minimal() +
      ggplot2::scale_x_continuous(
        breaks = n_key_topics$n_topics, labels = n_key_topics$m,
        minor_breaks = NULL) +
      ggplot2::scale_y_continuous(
        breaks = n_key_topics$n_topics,
        minor_breaks = NULL) +
      ggplot2::xlab("models") + ggplot2::ylab("# of key topics")

    print(g)
  }

  n_key_topics
}
