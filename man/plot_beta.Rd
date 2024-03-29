% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_alignment.R
\name{plot_beta}
\alias{plot_beta}
\title{Plot Topics Heatmap}
\usage{
plot_beta(
  x,
  models = "all",
  filter_by = "beta",
  x_axis = "label",
  threshold = 0.001,
  n_features = NULL,
  beta_aes = "size",
  color_by = "path"
)
}
\arguments{
\item{x}{(required) An alignment class object resulting from
\code{align_topics}.}

\item{models}{Which models to display in the heatmap? Defaults to
\code{"all"}, meaning that all models are shown. If given \code{"last"}, only
the last model in the models list will be plotted. If given a vector of
characters, it will plot only models whose names in the original models list
match. Similarly, if given a list of integers, only the models lying at those
indices in the original model list will be visualized.}

\item{filter_by}{(optional, default = \code{"beta"}) a character specifying
if the data (beta matrices) should be filtered by the average \code{"beta"}
across topics or by the \code{"distinctiveness"} of the features.}

\item{x_axis}{(optional, default = \code{"index"}) a character specifying
if the x-axis should display topic indices (\code{"index"}) such that they
match the alignment plot order or topic names (\code{"label"}).}

\item{threshold}{(optional, default = 0.001)
Words (features) with less than this average beta or
distinctiveness across all topics are ignored}

\item{n_features}{(optional) alternative to \code{threshold}. The maximum
number of words (features) to display along rows of the plot.}

\item{beta_aes}{Should word probabilities within a topic be encoded using
circle size (\code{"size"}) or opacity (\code{"alpha"}) ? Defaults to
\code{"size"}.}

\item{color_by}{(optional) What should the color of topics and weights
encode? Defaults to 'path'. Other possible arguments are 'coherence',
'refinement', or 'topic'.}
}
\value{
A ggplot2 object describing the word probabilities associated with
each topic across models of interest.
}
\description{
This function plots the \eqn{\beta_{kd}^{m}} topic parameters across models
\eqn{m}, topics \eqn{k}, and dimensions \eqn{d}. It takes as input a raw
alignment object and then returns a circle heatmap. The size of each circle
corresponds to the value \eqn{\beta_{kd}^m} for the model in panel \eqn{m},
topic in column \eqn{k}, and dimension in row \eqn{d}. The plot can be
restricted to only a subset of models by using the \code{models} argument,
which may be either a vector of model names or numeric indices into the list
of models. The dimensions can be filtered by using the \code{n_features} or
\code{min_beta} arguments -- by default, only dimensions with at least one
topic satisfying \eqn{\beta_{kd}^m > 0.025} are displayed.
}
\examples{
library(purrr)
data <- rmultinom(10, 20, rep(0.1, 20))
lda_params <- setNames(map(1:5, ~ list(k = .)), 1:5)
lda_models <- run_lda_models(data, lda_params)
alignment <- align_topics(lda_models)
plot_beta(alignment)
plot_beta(alignment, models = c(3, 4))
plot_beta(alignment, models = "last")

}
