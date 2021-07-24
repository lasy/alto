
library("alto")

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

ordering <- data.frame(
  m = c("K1", "K2", "K2", "K3", "K3", "K3", "K4", "K4", "K4", "K4"),
  k_LDA_init = c(1, 1:2, 1:3, 1:4),
  k_LDA = c(1, 1, 2, 1, 3, 2, 2, 3, 4, 1)
)

models <- simulate_models()
reordered <- reorder_models(models, ordering)

test_that(
  "Permutes parameters at levels 3 and 4 of a simulated model.", {
  expect_equal(reordered[["K1"]]$gamma, models[["K1"]]$gamma)
  expect_equal(reordered[["K1"]]$beta, models[["K1"]]$beta)
  expect_equal(reordered[["K2"]]$gamma, models[["K2"]]$gamma)
  expect_equal(reordered[["K2"]]$beta, models[["K2"]]$beta)
  expect_equal(reordered[["K3"]]$gamma, models[["K3"]]$gamma[, c(1, 3, 2)])
  expect_equal(reordered[["K3"]]$beta, models[["K3"]]$beta[c(1, 3, 2), ])
  expect_equal(reordered[["K4"]]$gamma, models[["K4"]]$gamma[, c(2, 3, 4, 1)])
  expect_equal(reordered[["K4"]]$beta, models[["K4"]]$beta[c(2, 3, 4, 1), ])
})

test_weights <- data.frame(
  m = c(rep("K1", 2), rep("K2", 6), rep("K3", 12)),
  m_next = c(rep("K2", 2), rep("K3", 6), rep("K4", 12)),
  k_LDA = c(1, 1, rep(1:2, 3), rep(1:3, 4)),
  k_LDA_next = c(1:2, rep(1:3, 2), rep(1:4, 3)),
  weight = runif(20),
  norm_weight = runif(20)
)

reordered <- reorder_weights(test_weights, ordering)

test_that(
  "Permutes k_LDA's at levels 3 and 4 of artificial weights.", {
  expect_equal(reordered$k_LDA[1:8], test_weights$k_LDA[1:8])
  expect_equal(reordered$k_LDA_next[1:2], test_weights$k_LDA_next[1:2])
  expect_equal(reordered$k_LDA[9:20], rep(c(1, 3, 2), 4))
  expect_equal(reordered$k_LDA_next[3:8], rep(c(1, 3, 2), 2))
  expect_equal(reordered$k_LDA_next[9:20], rep(c(2, 3, 4, 1), 3))
})

test_that(
  "Does not alter the order of the underlying weights", {
  expect_equal(reordered$weight, test_weights$weight)
  expect_equal(reordered$norm_weight, test_weights$norm_weight)
})
