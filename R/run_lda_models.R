#' Run LDA models for specified values of hyper-parameters or for different environments.
#'
#' [Description]
#'
#' @param data (required) a \code{matrix} or \code{data.frame} containing the counts (integers) of each feature (e.g. words) and each sample (or document). Each row is a sample, each column is a feature.
#' @param lda_params_list (required) a list specifying the parameter for each models that needs to be ran. Currently, supported parameters are "K" (the number of topic), "method" ("VEM" or "Gibbs"), and any of the possible arguments of \code{LDAcontrol}. See \code{topicmodels::LDA} for details and below for examples.
#' @param reset (optional, default = \code{FALSE})
#'
#' @return a list of LDA models (see package \code{topicmodels}). (or a \code{lda_models} object ? (which would be a list of 1. a list of model; 2. some metadata about the alignement))
#' @export

run_lda_models = function(
  data,
  lda_varying_params_lists,
  lda_fixed_params_list,
  dir,
  reset = FALSE
){


#   = list(model1 = list(K = 2),
#          model2 = list(K = 3))
#      list(model1 = list(environment = c(1,4,7,100)),
#           model2 = list(environment = c(2,3,5,6,99))

  # 1. CHECKS
  # check data (format and values)
  # check lda_varying_params_lists
  # check lda_fixed_params_list



  # 2. RUNNING MODELS


  # 3. Return results


}
