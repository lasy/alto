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
  color_by <- match.arg(color_by, c("topic", "branch", "refinement", "robustness"))

  # layout and viz
  layouts <- .compute_layout(x, rect_gap)
  .plot_from_layout(x, layouts, rect_gap, color_by)
}

#' @importFrom ggplot2 ggplot geom_ribbon aes %+% scale_x_continuous geom_rect
#' theme guides scale_fill_gradient scale_fill_discrete
#' @importFrom dplyr mutate left_join
.plot_from_layout <- function(aligned_topics, layouts, rect_gap, color_by) {

  rect <- .add_topic_col(layouts$rect, aligned_topics, color_by)
  ribbon <- .add_topic_col(layouts$ribbon, aligned_topics, color_by)

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
  if (color_by %in% c("refinement", "robustness")) {
    g <- g +
      scale_fill_gradient(
        color_by, low = "brown1", high = "cornflowerblue", limits = c(0, 1)
      )
  } else {
    g <- g +
    scale_fill_discrete(limits = levels(rect$topic_col)) +
    guides(fill = "none")
  }

  g
}


#' @importFrom dplyr mutate all_of
#' @importFrom magrittr %>%
.add_topic_col <- function(df, x, color_by) {
  df %>%
    mutate(topic = factor(k)) %>%
    left_join(topics(x), by = c("m", "k")) %>%
    rename(topic_col = !!color_by)
}

#' @importFrom magrittr %>%
#' @importFrom stringr str_c
#' @importFrom dplyr bind_rows group_by arrange summarise mutate rename select
#' @importFrom utils tail
.compute_layout <- function(aligned_topics, rect_gap = 0.2) {
  layout_rect <-
    aligned_topics@topics %>%
    select(m, k, prop) %>%
    group_by(m) %>%
    mutate(
      m_num = match(m, levels(m)),
      ymax = k / (max(k) + 1) + cumsum(prop),
      ymin = ymax - prop
    )

  # compute flows out and into rectangles (input to geom_ribbon)
  weights <- consecutive_weights(aligned_topics)
  r_out <- ribbon_out(weights, rect_gap)
  r_in <- ribbon_in(weights, -rect_gap)
  list(rect = layout_rect, ribbon = bind_rows(r_out, r_in))
}

ribbon_out <- function(weights, rect_gap = 0.1) {
  weights %>%
    group_by(m) %>%
    arrange(m, k, k_next) %>%
    mutate(
      ymax = k / (max(k + 1)) + cumsum(weight),
      ymin = ymax - weight,
      id = str_c(m, m_next, "_", k, "-", k_next),
      m_num = match(m, levels(m)) + rect_gap
    )
}

ribbon_in <- function(weights, rect_gap = 0.1) {
  weights %>%
    group_by(m_next) %>%
    arrange(m, k_next, k) %>%
    mutate(
      ymax = k_next / (max(k_next + 1)) + cumsum(weight),
      ymin = ymax - weight,
      id = str_c(m, m_next, "_", k, "-", k_next),
      m_num = match(m_next, levels(m_next)) + rect_gap
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
#' @importFrom dplyr select starts_with filter
#' @importFrom superheat superheat
#' @importFrom ggplot2 ggplot aes scale_color_identity geom_point geom_tile
#' guides scale_fill_identity scale_size scale_alpha facet_grid labs theme_bw
#' theme element_text
#' @importFrom grid unit
#' @export
plot_beta <- function(x, models = "all", min_beta = 0.001, n_features = NULL,
                      beta_aes = "size", color_by = "branch") {
    beta_aes <- match.arg(beta_aes, choices = c("size", "alpha"))
    color_by <- match.arg(color_by, choices = c("topic", "branch", "refinement", "robustness"))
    beta <- plot_beta_layout(x, models, min_beta, n_features, color_by) %>%
      format_beta() %>%
      filter(b > min_beta)

    g <- ggplot(beta, aes(x = factor(k, levels = 1:100), y = w, col = col)) +
      guides(col = "none", size = "none")

    if (beta_aes == "size") {
      g <- g +
        geom_point(aes(size = b)) +
        scale_color_identity() +
        scale_size(range = c(0, 5), limits = c(0, 1))
    } else {
      g <- g +
        geom_tile(aes(alpha = b)) +
        scale_fill_identity() +
        scale_alpha(range = c(0, 1), limits = c(0, 1))
    }

    g +
      facet_grid(. ~ m, scales = "free", space = "free") +
      labs(x = "", y = "") +
      theme_bw() +
      theme(
        panel.spacing.x = unit(0, "pt"),
        strip.text.y = element_text(angle = 0, hjust = 0, color = "black")
      )
  }


#' @importFrom purrr map map_dfr
#' @importFrom magrittr %>%
#' @importFrom dplyr select row_number mutate n left_join
#' @importFrom scales hue_pal
plot_beta_layout <- function(x, subset = "all", min_beta = 0, n_features = NULL,
                             color_by = "branch") {
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
    set_colnames(c("m", colnames(model_params[[1]]$beta))) %>%
    trim_betas(min_beta, n_features) %>%
    mutate(m = factor(m, levels = rev(names(model_params))))

  # associate topics with the variable to shade in by
  topic_weights <- topics(x) %>%
    filter(m %in% betas$m) %>%
    mutate(topic = factor(k)) %>%
    rename(topic_col = !!color_by)

  if (color_by %in% c("topic", "branch")) {
    topic_weights <- topic_weights %>%
      mutate(col = hue_pal()(nlevels(topic_col))[as.integer(topic_col)])
  } else {
    topic_weights <- topic_weights %>%
      mutate(col = colorRampPalette(colors = c("brown1", "cornflowerblue"))(11)[round(topic_col,1)*10+1])
  }

  list(betas = betas, weights = topic_weights)
}

#' @importFrom dplyr select group_by mutate ungroup left_join arrange filter
#'  slice_head slice_min
#' @importFrom tidyr pivot_longer
format_beta <-  function(p) {
  beta <- p$betas %>%
    group_by(m) %>%
    mutate(k = row_number()) %>%
    ungroup() %>%
    left_join(p$weights %>% select(m, k, topic_col, col), by = c("m", "k")) %>%
    pivot_longer(
      -c(m, k, col, topic_col),
      names_to = "w",
      values_to = "b"
    )

  w_order <- beta %>%
    slice_min(m) %>%
    arrange(w, -b) %>%
    group_by(w) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    arrange(k)

  beta %>%
    mutate(
      w = factor(w, levels = w_order$w %>%  rev()),
      m = factor(m, levels = rev(levels(m)))
    )
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
