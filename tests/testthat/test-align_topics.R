library(alto)
library(MCMCpack)
library(purrr)
library(dplyr)
set.seed(1234)

simulate_lda <- function(betas, gammas, n0=NULL, lambda=1e4) {
  n <- nrow(gammas)
  if (is.null(n0)) {
    n0 <- rpois(n, lambda)
  }

  x <- matrix(nrow = n, ncol = ncol(betas))
  for (i in seq_len(n)) {
    x[i, ] <- rmultinom(1, n0[i], t(betas) %*% gammas[i, ])
  }
  rownames(x) <- seq_len(n)
  colnames(x) <- seq_len(ncol(betas))
  x
}

K <- 5
V <- 20
N <- 10
n_models <- 7
lambdas <- list(beta = 0.1, gamma = 0.5, count = 1e4)
betas <- rdirichlet(K, rep(lambdas$beta, V))
gammas <- rdirichlet(N, rep(lambdas$gamma, K))
x <- simulate_lda(betas, gammas, lambda = lambdas$count)
lda_params <- map(1:n_models, ~ list(k = .))
names(lda_params) <- str_c("K", 1:n_models)
alignment <- x %>%
  run_lda_models(lda_params, reset = TRUE) %>%
  align_topics(method = "transport")

test_that(
  "Very correlated topics have high alignment weights.", {
  lda_models <- models(alignment)
  for (i in seq_len(n_models - 1)) {
    S <- cor(t(exp(lda_models[[i]]$beta)), t(exp(lda_models[[i + 1]]$beta)))
    max_s <- which(S == max(S), arr.ind = TRUE)
    weight <- weights(alignment) %>%
      filter(
        m == names(lda_models)[i],
        k_LDA == max_s[1],
        k_LDA_next == max_s[2]
      )
    expect_gt(weight$norm_weight[1], 0.8)
  }
})

test_that(
  "High alignment weights correspond to correlated topics.", {
  lda_models <- models(alignment)
  for (i in seq_len(n_models - 1)) {
    if (i == 1) next

    ks <- weights(alignment) %>%
      filter(m == names(lda_models)[i]) %>%
      slice_max(norm_weight)
    topics_cor <- cor(lda_models[[i]]$gamma, lda_models[[i + 1]]$gamma)
    expect_gt(topics_cor[ks$k_LDA[1], ks$k_LDA_next[1]], 0.8)
  }
})
