% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_lda_models.R
\name{.set_lda_seed}
\alias{.set_lda_seed}
\title{Set LDAControl seed}
\usage{
.set_lda_seed(params_list, seed)
}
\description{
The LDA model has its own internal seed (it does not use the global R seed).
This helper can be used to control that internal seed within
`run_lda_models()`.
}
\keyword{internal}
