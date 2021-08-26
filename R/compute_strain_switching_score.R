compute_strain_switching_scores_of_model <-
  function(alignment, model = 5, n_ancestry_levels = 3) {
    topic_pairs <-
      combn(
        x = alignment@topics %>%
          dplyr::filter(m == model) %>%
          dplyr::select("k") %>%
          unlist(),
        m = 2) %>%
      t() %>%
      magrittr::set_colnames(c("k1","k2")) %>%
      as_tibble()
    sss <-
      purrr::map_dfr(
        .x = 1:nrow(topic_pairs),
        .f = function(i){
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

    models = levels(alignment@topics$m)
    j = which(models == model)
    # prev_models = models[max(1,(j-n_ancestry_levels)):(j-1)]

    branches =
      alignment@topics %>%
      filter(m %in% model, k %in% c(k1,k2)) %>%
      select(branch) %>%
      unlist()

    prev_model =
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

    i = which(models == prev_model)
    prev_model = models[max(2, j-n_ancestry_levels, i-1)]

    alignment@weights %>%
      filter(
        m %in% prev_model,
        m_next == model,
        k_next %in% c(k1,k2)) %>%
      left_join(
        ., alignment@topics %>% select(m, k, refinement), by = c("m","k")) %>%
      left_join(
        .,
        alignment@topics %>%
          select(m, k, coherence) %>%
          rename(m_next = m, k_next = k),
        by = c("m_next","k_next")) %>%
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

