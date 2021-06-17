#' Visualize alignments between topics of distinct LDA models
#'
#' [Description]
#'
#' @param aligned_topics (required)
#' @param add_leaves (optional, default = TRUE) whether topic composition of last model should be displayed.
#' @param min_beta (optional, default = 0.025) if \code{add_leaves} is \code{TRUE}, this option specifies the minimum beta value (i.e. proportion in topic) for a feature to be displayed in the leaves.
#' @param n_features (optional) alternative to \code{min_beta}. if \code{add_leaves} is \code{TRUE}, this option specifies the minimum beta value (i.e. proportion in topic) for a feature to be displayed in the leaves.
#' @param add_feature_labels (optional, default = TRUE) if \code{add_leaves} is \code{TRUE}, this option specifies if the name of the features should be displayed.
#' @param reverse_x_axis (optional, default = TRUE) specifies whether the x-axis (models) should be reversed.
#'
#' @seealso align_topics
#' @return a \code{ggplot} object (?) .
#' @export

plot_alignment <- function(
  aligned_topics,
  add_leaves = TRUE,
  min_beta = 0.025,
  n_features = NULL,
  add_feature_labels = TRUE,
  reverse_x_axis = FALSE
) {

  # 1. CHECKS


  # 2. COMPUTE LAYOUT

  # 3. CREATE VIZ

  # 4. Return ggplot
}
