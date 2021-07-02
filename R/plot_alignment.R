#' Visualize alignments between topics of distinct LDA models
#'
#' [Description]
#'
#' @param aligned_topics (required)
#' @param add_leaves (optional, default = TRUE) whether topic composition of
#' last model should be displayed.
#' @param reverse_x_axis (optional, default = TRUE) specifies whether
#' the x-axis (models) should be reversed.
#'
#' @seealso align_topics
#' @return a \code{ggplot} object (?) .
#' @export
plot_alignment <- function(
  x,
  reverse_x_axis = FALSE,
  rect_gap = 0.2,
  color_by = "branch"
) {
  # inputs
  .check_input(x)
  color_by <- color_by[1]
  color_by <- match.arg(color_by, c("topic","branch","refinement","stability"))
  # layout and viz
  layouts <- .compute_layout(x@weights, rect_gap)
  .plot_from_layout(x@weights, layouts$rect, layouts$ribbon, rect_gap, color_by)
}

#' @importFrom ggplot2 ggplot geom_ribbon aes %+% scale_x_continuous geom_rect
#' theme guides scale_fill_gradient
#' @importFrom dplyr mutate left_join
.plot_from_layout <- function(weights, rect, ribbon, rect_gap, color_by) {

  rect = .add_topic_col(rect, weights, color_by)
  ribbon = .add_topic_col(ribbon, weights, color_by)

  ms <- unique(rect$m_num)
  g <-
    ggplot() +
    geom_ribbon(
      data = ribbon,
      aes(
        x = m_num,
        ymin = ymin,
        ymax = ymax,
        group = id,
        fill = topic_col
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
        fill = topic_col
      )
    ) +
    scale_x_continuous(breaks = ms, labels = ms) +
    theme(legend.position = "bottom")

  # replace choices below by a better color scheme...
  if(color_by %in% c("refinement", "stability"))
    g <- g + scale_fill_gradient(color_by,
                                 low = "brown1",
                                 high = "cornflowerblue",
                                 limits = c(0,1))
  else
    g <- g + guides(fill = "none")

  g
}



.add_topic_col <- function(x, weights, color_by){

  if (color_by == "topic"){
    x <- x %>% mutate(topic_col = factor(k_LDA))
  }
  if (color_by == "branch"){
    branches <- identify_branches(weights)
    x <-
      x %>%
      left_join(., branches, by = c("m", "k_LDA")) %>%
      mutate(topic_col = factor(branch))

  }
  if (color_by == "refinement") {
    refinement_score <- compute_refinement_scores(weights)
    x <-
      x %>%
      left_join(., refinement_score, by = c("m", "k_LDA")) %>%
      mutate(topic_col = refinement_score)

  }
  if (color_by == "stability") {
    stability <- compute_stability_along_branches(weights)
    branches <- identify_branches(weights)
    x <-
      x %>%
      left_join(., branches, by = c("m", "k_LDA")) %>%
      left_join(., stability, by = c("m", "branch")) %>%
      mutate(topic_col = stability)

  }
  x
}





#' @importFrom magrittr %>%
#' @importFrom stringr str_c
#' @importFrom dplyr bind_rows group_by arrange summarise mutate rename select
.compute_layout <- function(weights, rect_gap = 0.2) {

  final_topic <-
    weights %>%
    filter(m_next == tail(levels(m_next), 1)) %>%
    select(-m, -k_LDA) %>%
    rename(m = m_next, k_LDA = k_LDA_next)
  layout_rect <-
    bind_rows(
      topic_layout(weights),
      topic_layout(final_topic)
    )

  # compute flows out and into rectangles (input to geom_ribbon)
  r_out <- ribbon_out(weights, rect_gap)
  r_in <- ribbon_in(weights, -rect_gap) #%>%
  # select(-m) %>%
  # rename(m = m_next)

  list(rect = layout_rect, ribbon = bind_rows(r_out, r_in))
}

ribbon_out <- function(weights, rect_gap = 0.1) {
  weights %>%
    group_by(m) %>%
    arrange(k_LDA, k_LDA_next) %>%
    mutate(
      ymax = k_LDA / (max(k_LDA + 1)) + cumsum(weight),
      ymin = ymax - weight,
      id = str_c(m, m_next, k_LDA, k_LDA_next),
      m_num = match(m, levels(m)) + rect_gap
    )
}

ribbon_in<- function(weights, rect_gap = 0.1) {
  weights %>%
    group_by(m_next) %>%
    arrange(k_LDA_next, k_LDA) %>%
    mutate(
      ymax = k_LDA_next / (max(k_LDA_next + 1)) + cumsum(weight),
      ymin = ymax - weight,
      id = str_c(m, m_next, k_LDA, k_LDA_next),
      m_num = match(m_next, levels(m_next)) + rect_gap
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

#' Plot Words Heatmap
#'
#' @param min_beta (optional, default = 0.025) if \code{add_leaves} is
#' \code{TRUE}, this option specifies the minimum beta value (i.e. proportion in
#' topic) for a feature to be displayed in the leaves.
#' @param n_features (optional) alternative to \code{min_beta}. if
#' \code{add_leaves} is \code{TRUE}, this option specifies the minimum beta
#' value (i.e. proportion in topic) for a feature to be displayed in the leaves.
#' @param add_feature_labels (optional, default = TRUE) if \code{add_leaves} is
#' \code{TRUE}, this option specifies if the name of the features should be
#' displayed.
#' @importFrom magrittr %>%
#' @importFrom dplyr select starts_with
#' @importFrom superheat superheat
#' @export
plot_beta <- function(x, models = "all", min_beta = 0.025, n_features = NULL,
                      add_feature_labels = TRUE) {
  p <- plot_beta_layout(x, models, min_beta, n_features)
  superheat(
    p$betas %>% select(starts_with("X")),
    membership.rows = p$betas$m,
    pretty.order.cols = TRUE,
    yr = p$weights$weight,
    yr.plot.type = "bar",
    yr.obs.col = p$weights$col,
    grid.vline = FALSE,
    heat.pal = c("#f6eff7", "#bdc9e1", "#67a9cf", "#1c9099", "#016c59")
  )
}

#' @importFrom purrr map map_dfr
#' @importFrom magrittr %>%
#' @importFrom dplyr select row_number mutate n left_join
#' @importFrom scales hue_pal
plot_beta_layout <- function(x, models = "all", min_beta = 0.025,
                             n_features = NULL, cols = NULL) {
  betas <- models(x) %>%
    map_dfr(~ data.frame(exp(.$beta)), .id = "m")

  topic_weights <- models(x) %>%
    map(~ colSums(.$gamma)) %>%
    map(~ data.frame(weight = .)) %>%
    map_dfr(~ mutate(., k_LDA  = row_number()), .id = "m")

  if (is.null(cols)) {
    cols <- data.frame(k_LDA = unique(topic_weights$k_LDA)) %>%
      mutate(col = hue_pal()(n()))
  }

  list(betas = betas, weights = left_join(topic_weights, cols))
}


#' Plot Method for Alignment Class
#' @import methods
#' @export
setMethod("plot", c(x = "alignment", y = "missing"), plot_alignment)
