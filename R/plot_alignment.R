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
  color_by <- match.arg(color_by, c("topic", "branch", "refinement", "stability"))
  # layout and viz
  layouts <- .compute_layout(x@weights, rect_gap)
  .plot_from_layout(x@weights, layouts$rect, layouts$ribbon, rect_gap, color_by)
}

#' @importFrom ggplot2 ggplot geom_ribbon aes %+% scale_x_continuous geom_rect
#' theme guides scale_fill_gradient scale_fill_discrete
#' @importFrom dplyr mutate left_join
.plot_from_layout <- function(weights, rect, ribbon, rect_gap, color_by) {

  rect <- .add_topic_col(rect, weights, color_by)
  ribbon <- .add_topic_col(ribbon, weights, color_by)

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
  if (color_by %in% c("refinement", "stability"))
    g <- g + scale_fill_gradient(str_c(color_by, "(log10)"),
                                 low = "brown1",
                                 high = "cornflowerblue",
                                 limits = c(min(rect$topic_col), 0))
  else
    g <- g +
    scale_fill_discrete(limits = levels(rect$topic_col)) +
    guides(fill = "none")

  g
}


.add_topic_col <- function(x, weights, color_by){

  if (color_by == "topic") {
    x <- x %>% mutate(topic_col = factor(k_LDA))
  }
  if (color_by == "branch"){
    branches <- identify_branches(weights) %>% arrange(branch)
    x <-
      x %>%
      left_join(., branches, by = c("m", "k_LDA")) %>%
      mutate(
        topic_col =
          factor(branch,
                 levels = branches$branch %>% unique() %>% sort())
      ) %>%
      arrange(m, k_LDA)

  }
  if (color_by == "refinement") {
    refinement_score <- compute_refinement_scores(weights)
    x <-
      x %>%
      left_join(., refinement_score, by = c("m", "k_LDA")) %>%
      mutate(topic_col = refinement_score %>% log10())

  }
  if (color_by == "stability") {
    stability <- compute_stability_along_branches(weights)
    branches <- identify_branches(weights)
    x <-
      x %>%
      left_join(., branches, by = c("m", "k_LDA")) %>%
      left_join(., stability, by = c("m", "branch")) %>%
      mutate(topic_col = stability %>% log10())

  }
  x
}

#' @importFrom magrittr %>%
#' @importFrom stringr str_c
#' @importFrom dplyr bind_rows group_by arrange summarise mutate rename select
#' @importFrom utils tail
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
  r_in <- ribbon_in(weights, -rect_gap)
  list(rect = layout_rect, ribbon = bind_rows(r_out, r_in))
}

ribbon_out <- function(weights, rect_gap = 0.1) {
  weights %>%
    group_by(m) %>%
    arrange(k_LDA, k_LDA_next) %>%
    mutate(
      ymax = k_LDA / (max(k_LDA + 1)) + cumsum(weight),
      ymin = ymax - weight,
      id = str_c(m, m_next,"_", k_LDA, "-",k_LDA_next),
      m_num = match(m, levels(m)) + rect_gap
    )
}

ribbon_in <- function(weights, rect_gap = 0.1) {
  weights %>%
    group_by(m_next) %>%
    arrange(k_LDA_next, k_LDA) %>%
    mutate(
      ymax = k_LDA_next / (max(k_LDA_next + 1)) + cumsum(weight),
      ymin = ymax - weight,
      id = str_c(m, m_next, "_", k_LDA, "-", k_LDA_next),
      m_num = match(m_next, levels(m_next)) + rect_gap
    )
}

#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise mutate
topic_layout <- function(weights) {
  weights %>%
    group_by(m, k_LDA) %>%
    summarise(topic_weight = sum(weight), .groups = "rowwise") %>%
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

#' Plot Topics Heatmap
#'
#' This function plots the \eqn{\beta_{kd}^{m}} topic parameters across models
#' \eqn{m}, topics \eqn{k}, and dimensions \eqn{d}. It takes as input a raw
#' alignment object and then returns a heatmap from the superheatmap package.
#' The shade of each cell corresponds to the value \eqn{\beta_{kd}^m} for the
#' model in panel \eqn{m}, topic in row \eqn{k}, and dimension in column
#' \eqn{d}. The plot can be restricted to only a subset of models by using the
#' \code{models} argument, which may be either a vector of model names or
#' numeric indices into the list of models. The dimensions can be filtered by
#' using the \code{n_features} or \code{min_beta} arguments -- by default, only
#' dimensions with at least one topic satisfying \eqn{\beta_{kd}^m > 0.025} are
#' displayed.
#'
#' @param models Which models to display in the heatmap? Defaults to
#' \code{"all"}, meaning that all models are shown. If given \code{"last"}, only
#' the last model in the models list will be plotted. If given a vector of
#' characters, it will plot only models whose names in the original models list
#' match. Similarly, if given a list of integers, only the models lying at those
#' indices in the original model list will be visualized.
#' @param min_beta (optional, default = 0.025) if \code{add_leaves} is
#' \code{TRUE}, this option specifies the minimum beta value (i.e. proportion in
#' topic) for a feature to be displayed in the leaves.
#' @param n_features (optional) alternative to \code{min_beta}. if
#' \code{add_leaves} is \code{TRUE}, this option specifies the minimum beta
#' value (i.e. proportion in topic) for a feature to be displayed in the leaves.
#'
#' @examples
#' library(purrr)
#' data <- rmultinom(10, 20, rep(0.1, 20))
#' lda_params <- setNames(map(1:5, ~ list(k = .)), 1:5)
#' lda_models <- run_lda_models(data, lda_params)
#' alignment <- align_topics(lda_models)
#' plot_beta(alignment)
#' plot_beta(alignment, min_beta = 0)
#' plot_beta(alignment, models = c(3, 4))
#' plot_beta(alignment, models = "last")
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select starts_with
#' @importFrom superheat superheat
#' @export
plot_beta <- function(x, models = "all", min_beta = 0.025, n_features = NULL,
                      ...) {
  p <- plot_beta_layout(x, models, min_beta, n_features)
  layout_args <- list(
    X = p$betas %>% select(-m),
    membership.rows = p$betas$m,
    yr = p$weights$topic_weight,
    yr.obs.col = p$weights$col
  )
  style_args <- superheat_defaults(...)
  do.call(superheat::superheat, c(layout_args, style_args))
}


plot_beta_lsy <- function(x, models = "all", min_beta = 0.001, n_features = NULL, beta_aes = c("size","alpha")){
  beta_aes = beta_aes[1]
  beta_aes = match.arg(beta_aes, choices = c("size","alpha"))

  p <- plot_beta_layout(x, models, min_beta, n_features)
  beta <- format_beta(p)


  if (beta_aes == "size") {
    g <- ggplot(beta %>% filter(b > min_beta),
                aes(x = k_LDA %>% factor(., levels = 1:100), y = w,
                    col = col, size = b)) +
      geom_point() +
      guides(col = "none", size = "none") +
      scale_color_identity() +
      scale_size(range = c(0,5), limits = c(0,1))


  } else{
    g <- ggplot(beta %>% filter(b > min_beta),
                aes(x = k_LDA %>% factor(., levels = 1:100), y = w,
                    fill = col, alpha = b)) +
      geom_tile() +
      guides(fill = "none", alpha = "none") +
      scale_fill_identity() +
      scale_alpha(range = c(0,1), limits = c(0,1))
  }

  g +
    facet_grid(. ~ m, scales = "free", space = "free") +
    theme_bw() +
    xlab("") + ylab("") +
    theme(
      panel.spacing.x = unit(0,"pt"),
      strip.text.y = element_text(angle = 0, hjust = 0, color = "black"))
}


#' @importFrom dplyr select group_by mutate ungroup left_join arrange filter slice_head
#' @importFrom tidyr pivot_longer
format_beta <-  function(p) {
  beta <-
    p$betas %>%
    group_by(m) %>%
    mutate(k_LDA = row_number()) %>%
    ungroup() %>%
    left_join(p$weights %>% select(m, k_LDA, topic_col, col), by = c("m","k_LDA")) %>%
    pivot_longer(
      -c(m,k_LDA, col, topic_col),
      names_to = "w",
      values_to = "b"
    )

  w_order <-
    beta %>%
    arrange(m) %>%
    filter(m == m[1]) %>%
    arrange(w, -b) %>%
    group_by(w) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    arrange(k_LDA)

  beta <-
    beta %>%
    mutate(w = w %>% factor(., levels = w_order$w %>%  rev()),
           m = m %>% factor(., levels = rev(levels(m))))
  beta
}



#' Style Defaults for plot_beta
#' @importFrom utils modifyList
superheat_defaults <- function(...) {
  plot_defaults <- list(
    bottom.label.text.size = 4,
    grid.vline = FALSE,
    heat.pal = c("#f6eff7", "#bdc9e1", "#67a9cf", "#1c9099", "#016c59"),
    left.label.text.size = 5,
    legend.text.size = 6,
    pretty.order.cols = TRUE,
    yr.axis.name = "Topic Weight",
    yr.plot.type = "bar"
  )
  modifyList(plot_defaults, list(...))
}

#' Filter to words of interest
#' @param min_beta Words with less than this beta in all topics are ignored
#' @param n_features Maximum number of features to show
#' @importFrom dplyr select bind_cols
trim_betas <- function(betas, min_beta = -1, n_features = NULL) {
  if (min_beta == 0 & is.null(n_features)) {
    return(betas)
  }

  beta_tilde <- select(betas, -m)
  beta_tilde <- beta_tilde[, colMeans(beta_tilde) > min_beta]
  if (ncol(beta_tilde) == 0) {
    warning("min_beta removed all columns. Try a smaller min_beta?
             Returning without filtering.\n")
    beta_tilde <- select(betas, -m)
  }

  discrepancies <- apply(beta_tilde, 2, discrepancy)
  ix <- order(discrepancies, decreasing = TRUE)
  beta_tilde <- beta_tilde[, ix[seq_len(min(n_features, ncol(beta_tilde)))]]
  bind_cols(m = betas$m, beta_tilde)
}

#' @importFrom purrr map map_dfr
#' @importFrom magrittr %>%
#' @importFrom dplyr select row_number mutate n left_join
#' @importFrom scales hue_pal
plot_beta_layout <- function(x, subset = "all", min_beta = 0, n_features = NULL,
                             cols = NULL) {
  # subset to only the models of interest
  model_params <- models(x)
  if (length(subset) == 1 && subset == "last") {
    model_params <- model_params[n_models(x)]
  } else if (length(subset) == 1 && subset == "all") {
    model_params <- model_params
  } else {
    model_params <- model_params[subset]
  }

  # filter betas to those that pass thresholds
  betas <- model_params %>%
    map_dfr(~ data.frame(exp(.$beta)), .id = "m") %>%
    trim_betas(min_beta, n_features) %>%
    mutate(m = factor(m, levels = rev(names(model_params))))

  # extract topic weights for the side plot
  final_topic <-
    weights(x) %>%
    filter(m_next == tail(levels(m_next), 1)) %>%
    select(-m, -k_LDA) %>%
    rename(m = m_next, k_LDA = k_LDA_next)
  topic_weights <-
    bind_rows(
      topic_layout(weights(x)),
      topic_layout(final_topic)
    ) %>%
    filter(m %in% betas$m) %>%
    .add_topic_col(weights(x), "branch") %>%
    mutate(col = hue_pal()(nlevels(topic_col))[as.integer(topic_col)])

  list(betas = betas, weights = topic_weights)
}

#' Equation (3) from https://doi.org/10.1371/journal.pgen.1006599
kl_div <- function(p1, p2) {
  p1 * log(p1 / p2) + (p2 - p1)
}

#' Pairwise KL divergences
kl_mat <- function(p) {
  K <- matrix(0, nrow = length(p), ncol = length(p))
  for (i in seq_along(p)) {
    for (j in seq_len(i - 1)) {
      K[i, j] <- kl_div(p[i], p[j])
    }
  }
  K
}

#' Discrepancy for ordering words in plot_betas
#'
#' This implements equation (4) from
#' https://doi.org/10.1371/journal.pgen.1006599
discrepancy <- function(p, lambda = 1e-7) {
  p <- (p + lambda) / sum(p + lambda) # Laplace smoothing
  K <- kl_mat(p)
  max(K)
}

#' Plot Method for Alignment Class
#' @import methods
#' @export
setMethod("plot", c(x = "alignment", y = "missing"), plot_alignment)
