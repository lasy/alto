
library(alto)

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
  m = c("K1", "K2", "K2", "K3", "K3", "K3", "K4", "K4", "K4", "K4"),
  k_LDA_init = c(1, 1:2, 1:3, 1:4),
  k_LDA = c(1, 1, 2, 1, 3, 2, 2, 3, 4, 1)
)

ordering <- list(
  "K1" = 1,
  "K2" = 1:2,
  "K3" = c(1, 3, 2),
  "K4" = c(2, 3, 4, 1)
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
    k_LDA_init = c(1:2, rep(1:3, 2), rep(1:4, 3)),
    k_LDA_next = c(2:1, rep(1:3, 2), rep(c(4, 3, 2, 1), 3)),
    weight = runif(20),
    norm_weight = runif(20)
)

perms <- topic_ordering(test_weights)

test_that(
  "k_init -> new K permutations are properly picked up", {
  expect_equal(perms[["K2"]], c(2, 1))
  expect_equal(perms[["K3"]], 1:3)
  expect_equal(perms[["K4"]], c(4, 3, 2, 1))
})
