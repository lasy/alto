

#'
add_branches <- function(aligned_topics, weight_fun, ...){

  model_names <- names(aligned_topics@models)

  # initializing branches
  branches <-
    aligned_topics@topics %>%
    filter(m == last(model_names)) %>%
    mutate(branch = k) %>%
    select(m, k, branch)

  all_weights <- aligned_topics@weights

  for (model in rev(model_names)[-1] ) { # c("k12","k11","k10","k9")
    w <- all_weights %>%
      filter(m == model)
    this_model_branches <-
      w %>%
      mutate(match_weight = 0.5 * fw_weight + 0.5 * bw_weight) %>%
      group_by(k) %>%
      arrange(k, -match_weight) %>%
      slice_head(n = 1) %>%
      left_join(.,
                branches %>% dplyr::rename(k_next = k, m_next = m),
                by = c("m_next", "k_next")) %>%
      select(m, k, branch)
    branches = bind_rows(branches, this_model_branches)
  }

  branches <-
    branches %>%
    arrange(m, k) %>%
    mutate(
      branch = branch %>%  factor(., levels = sort(unique(branch)))
    )

  aligned_topics@topics <-
    aligned_topics@topics %>%
    left_join(branches, by = c("m","k"))

  aligned_topics
}






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
identify_branches_v1 <- function(weights){
  models <- levels(weights$m)
  branches <-
    weights %>%
    dplyr::filter(m_next == rev(models)[1]) %>%
    dplyr::select(m_next, k_next) %>%
    dplyr::distinct() %>%
    dplyr::rename(m = m_next, k = k_next) %>%
    dplyr::mutate(branch = k)

  for (model in rev(models)[-1]) {
    b <-
      weights %>%
      dplyr::filter(m == model) %>%
      dplyr::left_join(
        .,
        branches %>% dplyr::rename(m_next = m, k_next = k),
        by = c("m_next", "k_next")
      ) %>%
      dplyr::arrange(m, k, -weight) %>%
      dplyr::group_by(m, k) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup() %>%
      dplyr::select(m, k, branch)
    branches <- dplyr::bind_rows(branches, b)
  }
  branches %>%
    dplyr::arrange(m, k)

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
#'     select(m, k) %>%
#'     distinct() %>%
#'     mutate(branch = row_number())
#'
#'   for(model in models[-1]){
#'     this_branches <-
#'       weights %>%
#'       filter(m_next == model) %>%
#'       group_by(k_next) %>%
#'       arrange(k_next, -norm_weight) %>%
#'       slice_head(n = 1) %>%
#'       left_join(
#'         .,
#'         refinement,
#'         by = c("m", "k")) %>%
#'       left_join(
#'         .,
#'         refinement %>%
#'           dplyr::rename(
#'             m_next = m,
#'             k_next = k,
#'             refinement_score_next = refinement_score
#'           ),
#'         by = c("m_next", "k_next")
#'       ) %>%
#'       left_join(., branches, by = c("m", "k")) %>%
#'       arrange(branch, -fw_weight) %>%
#'       group_by(branch) %>%
#'       mutate(b = row_number()) %>%
#'       ungroup() %>%
#'       mutate(
#'         new_branch = (b > 1) & (refinement_score_next > refinement_score)
#'       ) %>%
#'       arrange(new_branch, branch) %>%
#'       mutate(branch_next = ifelse(new_branch, max(branch) + cumsum(new_branch), branch)) %>%
#'       select(m_next, k_next, branch_next) %>%
#'       dplyr::rename(m = m_next, k = k_next, branch = branch_next)
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
#'
#'
#'
#'
# identify_branches_v3 <- function(aligned_topics){
#
#   x = aligned_topics
#   models <- names(x@models)
#
#   last_model <- last(models)
#   n_topics_in_last_model <- nrow(x@models[[length(x@models)]]$beta)
#
#   branches <-
#     expand_grid(
#       m = last_model,
#       k = 1:n_topics_in_last_model
#     ) %>%
#     mutate(branch = k)
#
#   for (model in models[-length(models)]) {
#     #cat(model, "\n")
#     w <- align_graph(
#       data.frame(from = model, to = last_model),
#       list(x@models[[model]]$gamma,
#            x@models[[last_model]]$gamma) %>%
#         set_names(c(model, last_model)),
#       list(x@models[[model]]$beta,
#            x@models[[last_model]]$beta) %>%
#         set_names(c(model, last_model)),
#       product_weights
#     )
#     this_model_branches <-
#       w %>%
#       group_by(k) %>%
#       arrange(k, -fw_weight) %>%
#       slice_head(n = 1) %>%
#       left_join(.,
#                 branches %>% dplyr::rename(k_next = k, m_next = m),
#                 by = c("m_next", "k_next")) %>%
#       select(m, k, branch)
#     branches = bind_rows(branches, this_model_branches)
#   }
#
#   branches %>%
#     dplyr::mutate(m = m %>%  factor(., levels = models)) %>%
#     dplyr::arrange(m, k)
#
# }


