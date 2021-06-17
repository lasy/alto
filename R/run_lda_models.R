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
#' @examples
#' set.seed(1)
#' data = matrix(sample(0:1000, size = 24), 4, 6)
#' lda_varying_params_lists = list(K2 = list(K = 2), K3 = list(K = 3))
#' lda_models = run_lda_models(data = data, lda_varying_params_lists = lda_varying_params_lists, dir = "test_lda_models/")

run_lda_models = function(
  data,
  lda_varying_params_lists,
  lda_fixed_params_list = list(),
  dir = NULL,
  reset = FALSE
){

  # 1. CHECKS
  # check data (format and values)
  data = .check_data(data = data)
  # check lda_varying_params_lists and lda_fixed_params_list
  param_lists =
    .check_params(
      lda_varying_params_lists = lda_varying_params_lists,
      lda_fixed_params_list = lda_fixed_params_list
      )
  #   = list(model1 = list(K = 2),
  #          model2 = list(K = 3))
  #      list(model1 = list(environment = c(1,4,7,100)),
  #           model2 = list(environment = c(2,3,5,6,99))

  if(is.null(dir)){
    delete_dir = TRUE
    dir = paste0("lda_models_",as.integer(Sys.time()),"/")
    while(dir.exists(dir)) dir = paste0("lda_models_",as.integer(Sys.time())+sample(1:10^9,1),"/")
  }
  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)


  # 2. RUNNING and SAVING MODELS
  lda_files = list.files(dir)
  if(reset |  (length(lda_files) != length(param_lists))){
    done =
      purrr::map(
        .x = names(param_lists),
        .f = function(m){
          param_list = param_lists[[m]]
          lda_model =
            topicmodels::LDA(
              x = data, k = param_list$K, method = param_list$method,
              control = param_list$control
            )
          save(lda_model, file = paste0(dir, m,".Rdata"))
        }
      )
  }

  # 3. retrive models
  lda_models =
    purrr::map(
      .x = names(param_lists),
      .f = function(m){
        load(file = paste0(dir, m,".Rdata"))
        lda_model
      }
    )
  names(lda_models) = names(param_lists)

  # 4. Return results
  lda_models
}


.check_data = function(data){
  data = as.matrix(data)
  if(any(as.integer(data) != data)) stop("'data' must only contain integer counts\n")
  if(any(data < 0)) stop("'data' must only contain positive integer counts\n")
  if(nrow(data) < 2) stop("'data' must have at least two rows\n")
  if(ncol(data) < 2) stop("'data' must have at least two columns\n")
  data
}



.check_params = function(lda_varying_params_lists,lda_fixed_params_list){
  # for each element of the lda_varying_params_lists, we check the varying params and we add the fixed params to the varying params list.
  # the end result is a list of lists.
  # each sub-list has the element K, method and control

  param_lists =
    purrr::map(
      .x = lda_varying_params_lists,
      .f = function(lda_varying_params_list){
        l = list(K = NULL, method = NULL, control = NULL)

        if("K" %in% names(lda_fixed_params_list)) l$K = lda_fixed_params_list$K
        if("K" %in% names(lda_varying_params_list)) l$K = lda_varying_params_list$K
        if(is.null(l$K)){warning("no value was provided for 'K'. Using default value 'K = 5'."); l$K = 5}

        if("method" %in% names(lda_fixed_params_list)) l$method = lda_fixed_params_list$method
        if("method" %in% names(lda_varying_params_list)) l$method = lda_varying_params_list$method
        if(is.null(l$method)){warning("no value was provided for 'method'. Using default value 'VEM'."); l$method = "VEM"}

        if("control" %in% names(lda_fixed_params_list)) l$control = lda_fixed_params_list$control
        if("control" %in% names(lda_varying_params_list)) l$control = lda_varying_params_list$control

        l
      }
    )

  param_lists
}
