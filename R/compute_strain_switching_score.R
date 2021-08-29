#' *DRAFT* Computes strain switching scores for topic pairs
#'
#' This function takes an alignment and a model identifier and returns
#' a \code{data.frame} with the strain switching scores for each pair
#' of topics in that model.
#'
#' The strain switching score for a given pair of topic (k1, k2) in model K
#' is currently computed as follow:
#' - the path of each topic is identified
#' - the models K1 and K2 at which each path first appears are identified
#' - the parent model K' is selected as the "latest" model between K1, K2
#' and Kmin, where Kmin is the model that is \code{n_ancestry_level} ahead
#' of K
#' - for each topic k' in K', the strain switching score for the triplet
#' (k', k1, k2) is computed as r(k')  w_in(k',k1)  w_in(k', k2)
#' - the topic k' which maximizes that score for the pair (k1, k2) is returned
#' together with the score and K'.
#'
#'
#' @param alignment (required) an \code{alignment} object
#' @param model (required) a \code{character} or \code{integer} specifying
#' the model for which strain switching scores should be computed.
#' @param n_ancestry_levels (required) an \code{integer} specifying the
#' maximum number of levels between the target \code{model} and the model
#' hosting the parent topic.
#' @return A \code{data.frame} providing the strain switching scores
#' for each pair of topic in the target \code{model}.
#'
#' @examples
#' library(purrr)
#' data <- rmultinom(10, 20, rep(0.1, 20))
#' lda_params <- setNames(map(1:5, ~ list(k = .)), 1:5)
#' lda_models <- run_lda_models(data, lda_params)
#'
#' alignment <- align_topics(lda_models, method = "transport")
#' plot(alignment)
#'
#' compute_strain_switching_scores_of_model(
#'    alignment,
#'    model = 5,
#'    n_ancestry_levels = 2
#' )
#'
#' @importFrom purrr map_dfr
#' @importFrom dplyr filter select as_tibble
#' @importFrom magrittr %>% set_colnames
#' @export
compute_strain_switching_scores_of_model <-
  function(alignment, model, n_ancestry_levels) {
    topic_pairs <-
      combn(
        x = alignment@topics %>%
          dplyr::filter(m == model) %>%
          dplyr::select("k") %>%
          unlist(),
        m = 2) %>%
      t() %>%
      magrittr::set_colnames(c("k1", "k2")) %>%
      as_tibble()
    sss <-
      purrr::map_dfr(
        .x = 1:nrow(topic_pairs),
        .f = function(i) {
          compute_sss_topic_pair(
            alignment = alignment,
            model = model,
            n_ancestry_levels = n_ancestry_levels,
            k1 = topic_pairs$k1[i],
            k2 = topic_pairs$k2[i]
          )
        }
      )
    sss
  }


#' @importFrom dplyr filter select left_join rename
compute_sss_topic_pair <-
  function(alignment, model, n_ancestry_levels, k1, k2) {

    models <- levels(alignment@topics$m)
    j <- which(models == model)

    branches <-
      alignment@topics %>%
      filter(m %in% model, k %in% c(k1, k2)) %>%
      select(branch) %>%
      unlist()

    prev_model <-
      alignment@topics %>%
      filter(branch %in% branches) %>%
      arrange(branch, m) %>%
      group_by(branch) %>%
      slice_head() %>%
      ungroup() %>%
      arrange(m) %>%
      tail(1) %>%
      select(m) %>%
      unlist()

    i <- which(models == prev_model)
    prev_model <- models[max(2, j - n_ancestry_levels, i - 1)]

    alignment@weights %>%
      filter(
        m %in% prev_model,
        m_next == model,
        k_next %in% c(k1, k2)) %>%
      left_join(
        ., alignment@topics %>% select(m, k, refinement), by = c("m", "k")) %>%
      left_join(
        .,
        alignment@topics %>%
          select(m, k, coherence) %>%
          rename(m_next = m, k_next = k),
        by = c("m_next", "k_next")) %>%
      group_by(m, k, refinement) %>%
      summarize(sss = prod(bw_weight), #  * coherence
                .groups = "drop")  %>%
      mutate(sss = refinement * sss) %>%
      arrange(-sss) %>%
      head(1) %>%
      rename(parent_m = m, parent_k = k) %>%
      mutate(k1 = k1, k2 = k2, m = model) %>%
      select(m, k1, k2, parent_m, parent_k, sss)
  }
