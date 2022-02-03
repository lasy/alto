#' Tests for including original topic names in alignment output
#'
#' There are situations where we might manually change the topic labels
#' associated with topic model output. These tests verify that those labels can
#' be retrieved from the @topics slot of an alignment object. Note that at the
#' moment it's only possible to rename the $beta in original topic models --
#' renaming $gamma leads to errors for unrelated reasons.

library(alto)
library(dplyr)

# some helper functions that are needed
simulate_models <- function(M = 4, n = 10, D = 4) {
  model <- list()
  for (m in seq_len(M)) {
      model[[m]] <- list(
        gamma = matrix(runif(n * m), n, m),
        beta = matrix(runif(D * m), m, D)
      )

      model[[m]]$gamma <- model[[m]]$gamma / rowSums(model[[m]]$gamma)
      model[[m]]$beta <- model[[m]]$beta / rowSums(model[[m]]$beta)
  }
  names(model) <- paste0("K", seq_len(M))
  model
}

models <- simulate_models()
rownames(models[[4]]$beta) <- paste0("topic_", 1:4)
alignment1 <- align_topics(models, method = "product")
alignment2 <- align_topics(models, method = "transport")

test_that(
  "beta topic names are included", {
  expect_true(
    "topic_1" %in% (alignment1@topics %>%
      filter(m == "K4") %>%
      pull(k_label))
  )
  expect_true(
    "topic_1" %in% (alignment2@topics %>%
      filter(m == "K4") %>%
      pull(k_label))
    )
})
