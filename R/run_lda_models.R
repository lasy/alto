#' Run LDA models for specified values of hyper-parameters or for different environments.
#'
#' [Description]
#'
#' @param data (required) a \code{matrix} or \code{data.frame} containing the counts (integers) of each feature (e.g. words) and each sample (or document). Each row is a sample, each column is a feature.
#' @param varying (required) a \code{character} specifying the varying element across models. Currently supported values for \code{varying} are (i) \code{K}, the number of topics, (ii) \code{alpha}, the prior on the number of topics per document, and (iii) \code{environment}, the environment in which samples have been collected.
#' @param values (required)
#' @param controls (optional)
#' @param reset (optional, default = \code{FALSE})
#'
#' @return a list of LDA models (see package \code{topicmodels}).
#' @export

run_lda_models = function(
  data,
  varying,
  values,
  controls,
  reset = FALSE
){

  # 1. CHECKS
  # check data (format and values)
  # check varying arguments
  varying = match.arg(varying, c("K","alpha","environment"))
  # check argument values
  if(varying == "K"){
    # values should be positive integer (allow 1?)
  }
  if(varying == "alpha"){
    # values should be positive doubles
  }
  if(varying == "environment"){
    # values should be a vector (of integer/characters/does not matter?) with a length equal to the number of rows in data
  }


  # 2. RUNNING MODELS




}
