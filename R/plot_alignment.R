#' Visualize alignments between topics of distinct LDA models
#'
#' [Description]
#'
#' @param aligned_topics (required)
#' @param add_leaves (optional, default = TRUE) whether topic composition of
#' last model should be displayed. @param min_beta (optional, default = 0.025)
#' if \code{add_leaves} is \code{TRUE}, this option specifies the minimum beta
#' value (i.e. proportion in topic) for a feature to be displayed in the leaves.
#' @param n_features (optional) alternative to \code{min_beta}. if
#' \code{add_leaves} is \code{TRUE}, this option specifies the minimum beta
#' value (i.e. proportion in topic) for a feature to be displayed in the leaves.
#' @param add_feature_labels (optional, default = TRUE) if \code{add_leaves} is
#' \code{TRUE}, this option specifies if the name of the features should be
#' displayed. @param reverse_x_axis (optional, default = TRUE) specifies whether
#' the x-axis (models) should be reversed.
#'
#' @seealso align_topics
#' @return a \code{ggplot} object (?) .
#' @export
plot_alignment <- function(
  x,
  add_leaves = TRUE,
  min_beta = 0.025,
  n_features = NULL,
  add_feature_labels = TRUE,
  reverse_x_axis = FALSE,
  rect_gap = 0.2
) {
  .check_input(x)
  layouts <- .compute_layout(x@weights, rect_gap)
  .plot_from_layout(layouts$rect, layouts$ribbon, rect_gap)
}

#' @importFrom ggplot2 ggplot geom_ribbon aes %+% scale_x_continuous geom_rect
#'   theme
.plot_from_layout <- function(rect, ribbon, rect_gap) {
  ms <- unique(rect$m_num)
  ggplot() +
    geom_ribbon(
      data = ribbon,
      aes(
        x = m_num,
        ymin = ymin,
        ymax = ymax,
        group = id,
        fill = as.factor(k_LDA)
      ),
      alpha = 0.4
    ) +
    geom_rect(
      data = rect,
      aes(
        xmin = m_num - rect_gap,
        xmax = m_num + rect_gap,
        ymin = ymin,
        ymax = ymax,
        fill = as.factor(k_LDA)
      )
    ) +
    scale_x_continuous(breaks = ms, labels = ms) +
    theme(legend.position = "none")
}

#' @importFrom magrittr %>%
#' @importFrom stringr str_c
#' @importFrom dplyr bind_rows group_by arrange summarise mutate rename select
.compute_layout <- function(weights, rect_gap = 0.2) {
  final_topic <- weights %>%
    filter(m_next == tail(levels(m_next), 1)) %>%
    select(-m, -k_LDA) %>%
    rename(m = m_next, k_LDA = k_LDA_next)
  layout_rect <- bind_rows(
    topic_layout(weights),
    topic_layout(final_topic)
  )

  # compute flows out and into rectangles (input to geom_ribbon)
  r_out <- ribbon_layout(weights, c("m", "k_LDA"), rect_gap)
  r_in <- ribbon_layout(weights, c("m_next", "k_LDA_next"), -rect_gap) %>%
    select(-m) %>%
    rename(m = m_next)

  list(rect = layout_rect, ribbon = bind_rows(r_out, r_in))
}

ribbon_layout <- function(weights, v = c("m", "k_LDA"), rect_gap = 0.1) {
  weights %>%
    group_by(across(v[1])) %>%
    arrange(across(v)) %>%
    mutate(
      ymax = .data[[v[2]]] / (max(.data[[v[2]]] + 1)) + cumsum(weight),
      ymin = ymax - weight,
      id = str_c(m, m_next, k_LDA, k_LDA_next),
      m_num = match(.data[[v[1]]], levels(.data[[v[1]]])) + rect_gap
    )
}

#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise mutate
topic_layout <- function(weights) {
  weights %>%
    group_by(m, k_LDA) %>%
    summarise(topic_weight = sum(weight)) %>%
    group_by(m) %>%
    mutate(
      m_num = match(m, levels(m)),
      ymax = k_LDA / (max(k_LDA) + 1) + cumsum(topic_weight),
      ymin = ymax - topic_weight
    )
}

.check_input <- function(aligned_topics) {
  stopifnot(class(aligned_topics) == "alignment")
}

#' Plot Method for Alignment Class
#' @import methods
#' @export
setMethod("plot", c(x = "alignment", y = "missing"), plot_alignment)
