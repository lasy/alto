#' Align topics from distinct LDA models
#'
#' [Description]
#'
#' @param model_list (required) a \code{lda_models} object. See \code{run_lda_models} for details.
#' @param models_to_compare (optional) either a character indicating if topics should be aligned between \code{consecutive} or \code{all} models, or a list of model pairs between which topics should be aligned.
#'
#' @return a \code{data.frame} (? or some specific object) providing the weights between every pair of topics of each model pairs in the input edgelist (\code{models_to_compare}). ? Do we also return the lda_models with ordered topics?
#' @export

align_topics = function(
  model_list,
  models_to_compare , # = "consecutive" // "all" // list(pair1 = c("model1", "model3"), pair2 = c("model1", "model3")),
  order_constrain
){

  # 1. CHECKS
  # check the model list
  # check the edgelist (models_to_compare)


  # 2. ALIGN TOPICS
  # ? gammas // betas ?

  # 3. ORDER TOPICS

  # 4. Return results
}
