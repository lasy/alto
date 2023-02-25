#' Visualize alignments between topics of distinct LDA models
#'
#' This function creates flow diagrams of alignment weight across collections of
#' topics. It is the default plot method associated with alignment objects. The
#' x-axis indexes models from those with few to those with many topics. Each
#' rectangle matches a topic; its height is the weight associated with that
#' topic from across all samples. The width of edges between topics corresponds
#' of the alignment weight between that pair of topics. By default, topics are
#' shaded according to their path. Other topic measures can be
#' provided by modifying the `color_by` argument.
#'
#' @param x (required) An alignment class object resulting from
#' \code{align_topics}.
#' @param rect_gap (optional) A float describing how much vertical space to put
#' between topics within the same model. The units correspond to topic masses.
#' Defaults to 0.2.
#' @param color_by (optional) What should the color of topics and weights
#' encode? Defaults to 'path'. Other possible arguments are 'coherence',
#' 'refinement', or 'topic'.
#' @param model_name_repair_fun (optional) How should names be repaired before plotting?
#' @param label_topics (optional, default = \code{FALSE}) A \code{logical}
#' specifying if topics should be labeled with the \code{"color_by"} information.
#' @param add_leaves (optional, default = \code{FALSE}) A \code{logical}
#' specifying if the topic composition of leave-topics should be printed.
#' @param leaves_text_size (optional, default = \code{10}) specifies the font
#' size of leaves annotations in \code{pt} if \code{add_leaves} is \code{TRUE}.
#' @param n_features_in_leaves (optional, default = 3) specifies the maximum
#' number of features that should be included in the leaves annotations
#' if \code{add_leaves} is \code{TRUE}.
#' @param min_feature_prop (optional, default = 0.1) specifies the minimum
#' proportion of a feature in a topic for that feature to be included in
#' the leaves annotations if \code{add_leaves} is \code{TRUE}.
#' @param top_n_edges (optional, \code{integer}, default = \code{NULL}) specifies
#'  the number of edges that should be drawn between the topics of subsequent models.
#'  The \code{top_n_edges} with the highest weights are drawn. If \code{NULL}
#'  (default), all edges are drawn.
#' @seealso align_topics
#' @return A \code{ggplot2} object describing the alignment weights across
#' models.
#' @export
plot_alignment <- function(
  x,
  rect_gap = 0.2,
  color_by = "path",
  model_name_repair_fun = paste0,
  label_topics = FALSE,
  add_leaves = FALSE,
  leaves_text_size = 10,
  n_features_in_leaves = 3,
  min_feature_prop = 0.1,
  top_n_edges = NULL
) {

  # inputs
  .check_input(x)
  color_by <- match.arg(color_by, c("topic", "topic_label", "path", "refinement", "coherence"))
  stopifnot(is.null(top_n_edges) || ((top_n_edges > 0) & (round(top_n_edges) == top_n_edges)))

  # layout and viz
  layouts <- .compute_layout(x, rect_gap)
  if (add_leaves) {
    leaves <- .get_leaves_layout(x, layouts$rect, n_features_in_leaves, min_feature_prop)
  } else {
    leaves <- data.frame()
  }
  .plot_from_layout(
    x, layouts, rect_gap, color_by,
    model_name_repair_fun = model_name_repair_fun, label_topics = label_topics,
    leaves = leaves, leaves_text_size = leaves_text_size,
    top_n_edges = top_n_edges
  )
}

#' @importFrom ggplot2 ggplot geom_ribbon aes %+% scale_x_continuous geom_rect geom_text
#' theme guides scale_fill_gradient scale_fill_discrete element_blank labs
#' @importFrom dplyr mutate left_join
.plot_from_layout <- function(aligned_topics, layouts, rect_gap, color_by, model_name_repair_fun = paste0, label_topics = FALSE, leaves = data.frame(), leaves_text_size = 10, top_n_edges = NULL) {

  rect <- .add_topic_col(layouts$rect, aligned_topics, color_by)
  ribbon <- .add_topic_col(layouts$ribbon, aligned_topics, color_by)

  if (!is.null(top_n_edges)) {
    top_n_edges <- top_n_edges %>% round()
    ribbon <-
      ribbon %>%
      arrange(m, k, -weight) %>%
      group_by(m, k) %>%
      slice_head(n = 2*top_n_edges)
  }

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
    scale_x_continuous(breaks = ms, labels = levels(rect$m)[ms] %>% model_name_repair_fun , minor_breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    theme(legend.position = "bottom",
          axis.text.y = element_blank()) +
    labs(x = "models", y = "")

  # replace choices below by a better color scheme...
  if (color_by %in% c("refinement", "coherence")) {
    g <- g +
      scale_fill_gradient(
        color_by, low = "brown1", high = "cornflowerblue"
      )
    rect <-
      rect %>%
      mutate(topic_col = topic_col %>% round(., 2))
  } else {
    g <- g +
      scale_fill_discrete(limits = levels(rect$topic_col), na.value = "transparent") +
      guides(fill = "none")
  }

  if (label_topics) {
    g <-
      g +
      geom_text(
        data = rect,
        aes(
          label = topic_col,
          x = m_num,
          y = (ymax + ymin)/2
        ),
        col = "black"
      )
  }

  if (nrow(leaves) > 0) {
    if (color_by != "coherence") {
      leaves <- .add_topic_col(leaves, aligned_topics, color_by = color_by)
    } else {
      leaves <- leaves %>% mutate(topic_col = NA_real_)
    }

    g <-
      g +
      geom_segment(
        data = leaves,
        aes(x = x_start, xend = x_end,
            y = y_start, yend = y_end,
            col = topic_col),
        linetype = 2
      ) +
      geom_text(
        data = leaves,
        aes(x = x_end, y = y_end,
            label = leave_label,
            col = topic_col),
        hjust = 0, nudge_x = 0.2,
        size = leaves_text_size/.pt,
        lineheight = 2.5/.pt
      ) +
      guides(col = "none") +
      expand_limits(
        x = max(leaves$x_end + leaves_text_size * leaves$max_w_length/120)
      )
  }

  g
}


#' @importFrom dplyr mutate all_of
#' @importFrom magrittr %>%
.add_topic_col <- function(df, x, color_by) {
  df %>%
    mutate(topic = factor(k)) %>%
    left_join(topics(x), by = c("m", "k")) %>%
    mutate(topic_label = k_label) %>%
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

#' @importFrom magrittr %>%
#' @importFrom dplyr group_by arrange mutate filter slice_head summarize
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_length
.get_leaves_layout <- function(x, rects, n_features_in_leaves = 3, min_feature_prop = NULL){

  betas_last_m <-
    x@models[[n_models(x)]]$beta %>%
    exp() %>%
    as.data.frame() %>%
    mutate(k = row_number()) %>%
    pivot_longer(
      cols = -k,
      names_to = "feature",
      values_to = "p"
    )

  if (is.null(min_feature_prop))
    min_feature_prop = 1/length(unique(betas_last_m$feature))

  betas_last_m <-
    betas_last_m %>%
    filter(p >= min_feature_prop) %>%
    group_by(k) %>%
    arrange(k, -p) %>%
    slice_head(n = n_features_in_leaves)

  leaves <-
    betas_last_m %>%
    group_by(k) %>%
    summarize(
      max_w_length = max(str_length(feature)),
      leave_label =
        str_c("[",round(p, 2) %>% format(., nsmall = 2),"] ", feature) %>%
        str_c(., collapse = "\n"),
      .groups = "drop"
    ) %>%
    left_join(., rects %>% filter(m == last(levels(m))), by = "k") %>%
    mutate(
      m = names(x@models) %>% last(),
      x_start = m_num + 0.2,
      x_end = m_num + 1.2,
      gap  = 1/(n()+1),
      equal_prop = 1/n(),
      y_start = (ymax + ymin)/2,
      y_end = cumsum(equal_prop + gap)-equal_prop/2

    ) %>%
    select(m, k, leave_label, x_start, x_end, y_start, y_end, max_w_length)
  leaves


}


#' Plot Topics Heatmap
#'
#' This function plots the \eqn{\beta_{kd}^{m}} topic parameters across models
#' \eqn{m}, topics \eqn{k}, and dimensions \eqn{d}. It takes as input a raw
#' alignment object and then returns a circle heatmap. The size of each circle
#' corresponds to the value \eqn{\beta_{kd}^m} for the model in panel \eqn{m},
#' topic in column \eqn{k}, and dimension in row \eqn{d}. The plot can be
#' restricted to only a subset of models by using the \code{models} argument,
#' which may be either a vector of model names or numeric indices into the list
#' of models. The dimensions can be filtered by using the \code{n_features} or
#' \code{min_beta} arguments -- by default, only dimensions with at least one
#' topic satisfying \eqn{\beta_{kd}^m > 0.025} are displayed.
#'
#' @param x (required) An alignment class object resulting from
#' \code{align_topics}.
#' @param models Which models to display in the heatmap? Defaults to
#' \code{"all"}, meaning that all models are shown. If given \code{"last"}, only
#' the last model in the models list will be plotted. If given a vector of
#' characters, it will plot only models whose names in the original models list
#' match. Similarly, if given a list of integers, only the models lying at those
#' indices in the original model list will be visualized.
#' @param filter_by (optional, default = \code{"beta"}) a character specifying
#' if the data (beta matrices) should be filtered by the average \code{"beta"}
#' across topics or by the \code{"distinctiveness"} of the features.
#' @param x_axis (optional, default = \code{"index"}) a character specifying
#' if the x-axis should display topic indices (\code{"index"}) such that they
#' match the alignment plot order or topic names (\code{"label"}).
#' @param threshold (optional, default = 0.001)
#' Words (features) with less than this average beta or
#' distinctiveness across all topics are ignored
#' @param n_features (optional) alternative to \code{threshold}. The maximum
#' number of words (features) to display along rows of the plot.
#' @param beta_aes Should word probabilities within a topic be encoded using
#' circle size (\code{"size"}) or opacity (\code{"alpha"}) ? Defaults to
#' \code{"size"}.
#' @param color_by (optional) What should the color of topics and weights
#' encode? Defaults to 'path'. Other possible arguments are 'coherence',
#' 'refinement', or 'topic'.
#' @return A ggplot2 object describing the word probabilities associated with
#' each topic across models of interest.
#'
#' @examples
#' library(purrr)
#' data <- rmultinom(10, 20, rep(0.1, 20))
#' lda_params <- setNames(map(1:5, ~ list(k = .)), 1:5)
#' lda_models <- run_lda_models(data, lda_params)
#' alignment <- align_topics(lda_models)
#' plot_beta(alignment)
#' plot_beta(alignment, models = c(3, 4))
#' plot_beta(alignment, models = "last")
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select filter
#' @importFrom ggplot2 ggplot aes scale_color_identity geom_point geom_tile
#' guides scale_fill_identity scale_size scale_alpha facet_grid labs theme_bw
#' theme element_text element_rect
#' @importFrom grid unit
#' @export
plot_beta <- function(x, models = "all",
                      filter_by = "beta",
                      x_axis = "label",
                      threshold = 0.001, n_features = NULL,
                      beta_aes = "size", color_by = "path") {

  filter_by <- match.arg(filter_by, choices = c("beta", "distinctiveness"))
  beta_aes <- match.arg(beta_aes, choices = c("size", "alpha"))
  color_by <- match.arg(color_by, choices = c("topic", "path", "refinement", "coherence"))
  x_axis <- match.arg(x_axis, choices = c("label","index"))

  beta <-
    plot_beta_layout(x, models, filter_by, threshold, n_features, color_by) %>%
    format_beta(., x_axis = x_axis)

  # we further trim beta to improve the visualization
  # by removing the betas that are 1 order of magnitude lower
  # than what would be obtained by distributing the betas within the selected words
  beta <-
    beta %>%
    filter(b > max(threshold, 1/length(unique(beta$w))/10))

  g <-
    ggplot(beta, aes(x = x, y = w)) + # col
    guides(size = "none") # col = "none",

  if (beta_aes == "size") {
    g <- g +
      geom_point(aes(size = b, col = topic_col)) +
      #scale_color_identity() +
      scale_size(range = c(0, 5), limits = c(0, 1))
  } else {
    g <- g +
      geom_tile(aes(alpha = b, fill = topic_col)) +
      #scale_fill_identity() +
      scale_alpha(range = c(0, 1), limits = c(0, 1))
  }

  if (color_by %in% c("topic", "path")) {
    g <- g +
      ggplot2::scale_color_discrete(color_by) +
      ggplot2::scale_fill_discrete(color_by)
      # mutate(col = hue_pal()(nlevels(topic_col))[as.integer(topic_col)])
  } else {
    max_score <- ifelse(color_by == "refinement", n_models(x), 1)
    g <- g +
      ggplot2::scale_color_gradient(color_by, low = "brown1", high = "cornflowerblue", limits = c(0, max_score)) +
      ggplot2::scale_fill_gradient(color_by, low = "brown1", high = "cornflowerblue", limits = c(0, max_score))
      # mutate(col = colorRampPalette(colors = c("brown1", "cornflowerblue"))(11)[round(topic_col,1)*10+1])
  }

  g +
    facet_grid(. ~ m, scales = "free", space = "free") +
    labs(x = "", y = "") +
    # theme_bw() +
    theme(
      panel.border = ggplot2::element_rect(color = "black", fill = NA),
      panel.spacing.x = unit(0, "pt"),
      #strip.text.y = element_text(angle = 0, hjust = 0, color = "black"),
      strip.background = ggplot2::element_rect(color = "black")
    )
}


#' @importFrom purrr map map_dfr
#' @importFrom magrittr %>%
#' @importFrom dplyr select row_number mutate n left_join
#' @importFrom scales hue_pal
plot_beta_layout <- function(x, subset = "all",
                             filter_by = "beta",
                             threshold = 0, n_features = NULL,
                             color_by = "path") {
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
  betas <-
    model_params %>%
    map_dfr(~ as.data.frame(exp(.$beta)), .id = "m") %>%
    trim_betas(filter_by, threshold, n_features) %>%
    mutate(m = factor(m, levels = rev(names(model_params))))

  # associate topics with the variable to shade in by
  topic_weights <-
    topics(x) %>%
    filter(m %in% betas$m) %>%
    mutate(topic = factor(k)) %>%
    rename(topic_col = !!color_by)

  # if (color_by %in% c("topic", "path")) {
  #   topic_weights <- topic_weights %>%
  #     mutate(col = hue_pal()(nlevels(topic_col))[as.integer(topic_col)])
  # } else {
  #   topic_weights <- topic_weights %>%
  #     mutate(col = colorRampPalette(colors = c("brown1", "cornflowerblue"))(11)[round(topic_col,1)*10+1])
  # }

  list(betas = betas, weights = topic_weights)
}

#' @importFrom dplyr select group_by mutate ungroup left_join arrange filter
#'  slice_head slice_min
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
format_beta <-  function(p, x_axis = "label") {
  beta <-
    p$betas %>%
    group_by(m) %>%
    mutate(k = row_number()) %>%
    ungroup() %>%
    left_join(p$weights %>% select(m, k, k_label, topic_col), by = c("m", "k")) %>% #, col
    pivot_longer(
      -c(m, k, k_label, topic_col), # , col
      names_to = "w",
      values_to = "b"
    )

  if (x_axis == "label") {
    beta$x <- beta$k_label
  } else {
    beta$x <- beta$k %>% factor()
  }


  w_order <-
    beta %>%
    slice_min(m) %>%
    arrange(w, -b) %>%
    group_by(w) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    arrange(x)

  beta %>%
    mutate(
      w = factor(w, levels = w_order$w %>%  rev()),
      m = factor(m, levels = rev(levels(m)))
    )
}

#' Filter to words of interest
#' @param betas A \code{data.frame} whose rows correspond to topics and whose
#' columns are \code{m}, the model ID, and the words for which estimates are
#' made.
#' @param filter_by a character specifying if the data (\code{betas}) should
#' be filtered by the average \code{"beta"} across topics or
#' by the \code{"distinctiveness"} of the features.
#' @param threshold Words (features) with less than this average beta or
#' distinctiveness across all topics are ignored
#' @param n_features Maximum number of features to show
#' @importFrom dplyr select bind_cols
trim_betas <- function(betas, filter_by, threshold = 0, n_features = NULL) {
  if (threshold == 0 & is.null(n_features)) {
    return(betas)
  }

  beta_tilde <- select(betas, -m)
  filter_by <- match.arg(filter_by, choices = c("beta", "distinctiveness"))

  if (filter_by == "beta") {
    filtering_quantity <- colMeans(beta_tilde)
  }else{
    filtering_quantity <- apply(beta_tilde, 2, discrepancy)
  }

  if (!is.null(n_features)){
    ix <- order(filtering_quantity, decreasing = TRUE)
    ix <- ix[seq_len(min(n_features,ncol(beta_tilde)))]
  }else{
    ix <- which(filtering_quantity > threshold)
  }

  beta_tilde <- beta_tilde[, ix]
  if (ncol(beta_tilde) == 0) {
    warning("threshold removed all columns. Try a smaller threshold?
             Returning without filtering.\n")
    beta_tilde <- select(betas, -m)
  }

  bind_cols(m = betas$m, beta_tilde)
}


#' KL Divergence of Categoricals
#' @param p1 The first probability in KL(p1 || p2)
#' @param p2 The second probability in KL(p1 || p2)
#' @return The KL divergence between pairs of discrete probability categorical
#' vectors.
#' Equation (3) from https://doi.org/10.1371/journal.pgen.1006599
#' @keywords internal
kl_div <- function(p1, p2) {
  p1 * log(p1 / p2) + (p2 - p1)
}

#' Pairwise KL divergences
#'
#' @param p A matrix whose rows correspond to discrete probability vectors.
#' @return K A matrix whose \code{ij} entry is the KL divergence between rows
#' \code{i} and \code{j} of \code{p}.
#' @keywords  internal
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
#' @param p the probability vector on which to compute the discrepancy measure.
#' @param lambda The degree of Laplace smoothing to apply to the topics. This
#'  prevents the divergence from exploding. Defaults to 1e-7.
#' This implements equation (4) from
#' https://doi.org/10.1371/journal.pgen.1006599
#' @keywords internal
discrepancy <- function(p, lambda = 1e-7) {
  p <- (p + lambda) / sum(p + lambda) # Laplace smoothing
  K <- kl_mat(p)
  max(K)
}

#' Plot Method for Alignment Class
#' @param x (required) An alignment class object resulting from
#' \code{align_topics}.
#' @param rect_gap (optional) A float describing how much vertical space to put
#' between topics within the same model. The units correspond to topic masses.
#' Defaults to 0.2.
#' @param color_by (optional) What should the color of topics and paths
#' encode? Defaults to 'path'. Other possible arguments are 'coherence',
#' 'refinement', or 'topic'.
#' @param model_name_repair_fun How should names be repaired before plotting?
#' @seealso align_topics
#' @return A \code{ggplot2} object describing the alignment weights across
#' models.
#' @import methods
#' @seealso plot_alignment
#' @export
setMethod("plot", c(x = "alignment", y = "missing"), plot_alignment)
