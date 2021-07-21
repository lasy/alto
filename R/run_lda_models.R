#' Run LDA models for specified values of hyper-parameters.
#'
#' This function fits a collection of lda models to a dataset, fitting one model
#' for each hyperparameter setting specified by the
#' \code{lda_varying_params_list} argument. Its output can be directly used by
#' \code{align_topics}.
#'
#' @param data (required) a \code{matrix}, \code{data.frame} or
#' \code{slam::simple_triplet_matrix} containing the counts (integers) of each
#' feature (e.g. words) and each sample (or document). If data is provided as
#' \code{matrix} or \code{data.frame}, each row is a sample, each column is a
#' feature.
#' @param lda_varying_params_lists (required) a \code{list} specifying the
#' parameter for each models that needs to be ran. Currently, supported
#' parameters are "k" (the number of topic), "method" ("VEM" or "Gibbs"), and
#' "control", a list of type \code{LDAcontrol}. See \code{topicmodels::LDA} for
#' details and below for examples.
#' @param lda_fixed_params_list (optional) a \code{list} specifying the
#' parameters common to all models to be fitted. Values provided by
#' \code{lda_fixed_params_list} are overwritten by those provided by
#' \code{lda_varying_params_lists}.
#' @param dir (optional) a \code{character} specifying the directory in which
#' individual LDA models should be stored. If not specified, individual LDA
#' models are not stored. This option is especially useful for data exploration
#' as it allows to save execution time if one wishes to add models to an
#' existing model list. (see examples)
#' @param reset (optional, default = \code{FALSE})
#'
#' @return a list of LDA models (see package \code{topicmodels}).
#' ? or a \code{lda_models} object which would be a list of
#' 1. a list of model;
#' 2. some metadata about the alignement
#' @importFrom purrr map
#' @importFrom topicmodels LDA
#' @export
#' @examples
#' set.seed(1)
#' data = matrix(sample(0:1000, size = 24), 4, 6)
#' lda_varying_params_lists = list(K2 = list(k = 2), K3 = list(k = 3))
#' lda_models =
#'    run_lda_models(
#'       data = data,
#'       lda_varying_params_lists = lda_varying_params_lists,
#'       dir = "test_lda_models/"
#'       )
#'
#' additional_lda_varying_params_list =
#'    list(K4 = list(k = 4))
#'  updated_lda_models =
#'    run_lda_models(
#'       data = data,
#'       lda_varying_params_lists =
#'         append(
#'            lda_varying_params_lists,
#'            additional_lda_varying_params_list),
#'       dir = "test_lda_models/"
#'       )
#'
#' # because we specified the "dir" option, it only runs LDA for k = 4
#' unlink("test_lda_models/", recursive = TRUE)

run_lda_models <-
  function(
    data,
    lda_varying_params_lists,
    lda_fixed_params_list = list(),
    dir = NULL,
    reset = FALSE
  ) {

    # 1. CHECKS
    # check data (format and values)
    data <- .check_data(data = data)
    # check lda_varying_params_lists and lda_fixed_params_list
    param_lists <-
      .check_params(
        lda_varying_params_lists = lda_varying_params_lists,
        lda_fixed_params_list = lda_fixed_params_list
      )

    delete_dir <- FALSE
    if (is.null(dir)) {
      delete_dir <- TRUE
      dir <- paste0("lda_models_", as.integer(Sys.time()), "/")
      while (dir.exists(dir))
        dir <- paste0("lda_models_",
                      as.integer(Sys.time()) + sample(1:10^9, 1),
                      "/")
    }
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)


    # 2. RUNNING and SAVING MODELS
    lda_files <- list.files(dir)
    if (reset |  (length(lda_files) != length(param_lists))) {
      done <-
        map(
          .x = names(param_lists),
          .f = function(m) {
            param_list <- param_lists[[m]]
            if (param_list$k == 1) {
              lda_model <- list(
                gamma = matrix(1, nrow = nrow(data), ncol = 1),
                beta = log(matrix(colSums(data) / sum(data), nrow = 1))
              )
            } else {
              tm <- LDA(
                      x = data,
                      k = param_list$k,
                      method = param_list$method,
                      control = param_list$control
                    )
              lda_model <- list(gamma = tm@gamma, beta = tm@beta)
            }

            save(lda_model, file = paste0(dir, m, ".Rdata"))
          }
        )
    }

    # 3. retrive models
    lda_models <-
      purrr::map(
        .x = names(param_lists),
        .f = function(m) {
          load(file = paste0(dir, m, ".Rdata"))
          lda_model
        }
      )
    names(lda_models) <- names(param_lists)

    # 4. Return results
    if (delete_dir) base::unlink(dir, recursive = TRUE)
    lda_models
  }


.check_data <- function(data) {
  data <- as.matrix(data)
  if (any(as.integer(data) != data))
    stop("'data' must only contain integer counts\n")
  if (any(data < 0))
    stop("'data' must only contain positive integer counts\n")
  if (nrow(data) < 2)
    stop("'data' must have at least two rows\n")
  if (ncol(data) < 2)
    stop("'data' must have at least two columns\n")
  data
}


#' @importFrom purrr map
.check_params <- function(lda_varying_params_lists, lda_fixed_params_list) {
  # for each element of the lda_varying_params_lists,
  # we check the varying params and
  # we add the fixed params to the varying params list.
  # the end result is a list of lists.
  # each sub-list has the element k, method and control
  defaults <- list(k = 5, method = "VEM", "control" = NULL)
  map(
     .x = lda_varying_params_lists,
     .f = function(hyper) {
        params_list <- modifyList(lda_fixed_params_list, hyper)
        if (is.null(params_list$k)) {
          message("No value was provided for 'k'. Using default value of '5'.")
        }
        if (is.null(params_list$method)) {
          message("Using default value 'VEM' for 'method' LDA parameter.")
        }

        modifyList(defaults, params_list)
    }
  )
}
