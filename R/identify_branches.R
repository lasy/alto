#' Identifies topic branches along the topic alignment graph
#'
#' [Description]
#'
#' @param weights (required) \code{data.frame} with the alignment weights
#' (the @weight field from an \code{alignment} object)
#'
#' @seealso align_topics
#' @return a \code{data.frame}
#' identifying the branch of each topic in each model.
#' @export
identify_branches <- function(weights){
  models <- levels(weights$m)
  branches <-
    weights %>%
    dplyr::filter(m_next == rev(models)[1]) %>%
    dplyr::select(m_next, k_LDA_next) %>%
    dplyr::distinct() %>%
    dplyr::rename(m = m_next, k_LDA = k_LDA_next) %>%
    dplyr::mutate(branch = k_LDA)

  for (model in rev(models)[-1]) {
    b <-
      weights %>%
      dplyr::filter(m == model) %>%
      dplyr::left_join(
        .,
        branches %>% dplyr::rename(m_next = m, k_LDA_next = k_LDA),
        by = c("m_next", "k_LDA_next")
      ) %>%
      dplyr::arrange(m, k_LDA, -weight) %>%
      dplyr::group_by(m, k_LDA) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(m, k_LDA, branch)
    branches <- dplyr::bind_rows(branches, b)
  }
  branches %>%
    dplyr::arrange(m, k_LDA)

}





#'
identify_branches_v3 <- function(aligned_topics){

  x = aligned_topics
  models <- names(x@models)

  last_model <- last(models)
  n_topics_in_last_model <- nrow(x@models[[length(x@models)]]$beta)

  branches <-
    expand_grid(
      m = last_model,
      k_LDA = 1:n_topics_in_last_model
    ) %>%
    mutate(branch = k_LDA)

  for (model in models[-length(models)]) {
    #cat(model, "\n")
    w <- align_graph(
      data.frame(from = model, to = last_model),
      list(x@models[[model]]$gamma,
           x@models[[last_model]]$gamma) %>%
        set_names(c(model, last_model)),
      list(x@models[[model]]$beta,
           x@models[[last_model]]$beta) %>%
        set_names(c(model, last_model)),
      product_weights
    )
    this_model_branches <-
      w %>%
      group_by(k_LDA) %>%
      arrange(k_LDA, -fw_weight) %>%
      slice_head(n = 1) %>%
      left_join(.,
                branches %>% dplyr::rename(k_LDA_next = k_LDA, m_next = m),
                by = c("m_next", "k_LDA_next")) %>%
      select(m, k_LDA, branch)
    branches = bind_rows(branches, this_model_branches)
  }

  branches %>%
    dplyr::mutate(m = m %>%  factor(., levels = models)) %>%
    dplyr::arrange(m, k_LDA)

}


#'
identify_branches_v4 <- function(aligned_topics){

  x = aligned_topics
  models <- names(x@models)

  last_model <- last(models)
  n_topics_in_last_model <- nrow(x@models[[length(x@models)]]$beta)

  branches <-
    expand_grid(
      m = last_model,
      k_LDA = 1:n_topics_in_last_model
    ) %>%
    mutate(branch = k_LDA)



  all_weights <-  align_topics(models = x@models, comparisons = "all", method = "transport")

  for (model in rev(models)[-1]) {
    #cat(model, "\n")
    w <- all_weights@weights %>%
      filter(m == model)
    this_model_branches <-
      w %>%
      group_by(k_LDA) %>%
      arrange(k_LDA, -fw_weight) %>%
      slice_head(n = 1) %>%
      left_join(.,
                branches %>% dplyr::rename(k_LDA_next = k_LDA, m_next = m),
                by = c("m_next", "k_LDA_next")) %>%
      select(m, k_LDA, branch)
    branches = bind_rows(branches, this_model_branches)
  }

  branches %>%
    dplyr::mutate(m = m %>%  factor(., levels = models)) %>%
    dplyr::arrange(m, k_LDA)

}


#'
#' #' @importFrom dplyr filter select distinct group_by ungroup mutate arrange slice_head left_join bind_rows
#' identify_branches_v2 <- function(weights){
#'   refinement = compute_refinement_scores(weights)
#'   models = weights$m %>% levels()
#'
#'   branches =
#'     weights %>%
#'     filter(m == models[1]) %>%
#'     select(m, k_LDA) %>%
#'     distinct() %>%
#'     mutate(branch = row_number())
#'
#'   for(model in models[-1]){
#'     this_branches <-
#'       weights %>%
#'       filter(m_next == model) %>%
#'       group_by(k_LDA_next) %>%
#'       arrange(k_LDA_next, -norm_weight) %>%
#'       slice_head(n = 1) %>%
#'       left_join(
#'         .,
#'         refinement,
#'         by = c("m", "k_LDA")) %>%
#'       left_join(
#'         .,
#'         refinement %>%
#'           dplyr::rename(
#'             m_next = m,
#'             k_LDA_next = k_LDA,
#'             refinement_score_next = refinement_score
#'           ),
#'         by = c("m_next", "k_LDA_next")
#'       ) %>%
#'       left_join(., branches, by = c("m", "k_LDA")) %>%
#'       arrange(branch, -fw_weight) %>%
#'       group_by(branch) %>%
#'       mutate(b = row_number()) %>%
#'       ungroup() %>%
#'       mutate(
#'         new_branch = (b > 1) & (refinement_score_next > refinement_score)
#'       ) %>%
#'       arrange(new_branch, branch) %>%
#'       mutate(branch_next = ifelse(new_branch, max(branch) + cumsum(new_branch), branch)) %>%
#'       select(m_next, k_LDA_next, branch_next) %>%
#'       dplyr::rename(m = m_next, k_LDA = k_LDA_next, branch = branch_next)
#'
#'     branches <-
#'       bind_rows(branches,
#'                 this_branches)
#'
#'   }
#'
#'   branches
#'
#' }


