test_that("the engine agrees with an independent reference implementation", {
  skip_on_cran()
  d <- sw_design(clusters_per_sequence = c(5, 5, 5, 5),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.15, treatment_or = 1.6, icc = 0.05,
                      n_per_cluster_period = 40)
  v <- validate_engine(d, a, nsim = 150, seed = 11)
  expect_true(v$agrees)
})

test_that("the default analysis omits the redundant sequence term", {
  # Sequence is a cluster-level attribute already absorbed by the cluster
  # random intercept, so it is not in the default model.
  expect_false(formals(fit_stepwedge_model)$adjust_sequence)
  # The legacy entry point keeps its documented 0.1.0 model, sequence and all.
  d <- sw_design(clusters_per_sequence = c(4, 4, 4, 4),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.05,
                      n_per_cluster_period = 20)
  sim <- simulate_swcrt(d, a, seed = 1)
  with_seq <- fit_stepwedge_model(sim, adjust_sequence = TRUE)
  expect_true(any(grepl("sequence", rownames(with_seq$coefficients))))
  without <- fit_stepwedge_model(sim)
  expect_false(any(grepl("sequence", rownames(without$coefficients))))
})
