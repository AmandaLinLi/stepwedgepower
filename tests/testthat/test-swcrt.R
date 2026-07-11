test_that("sw_design builds from crossover periods with baseline periods", {
  d <- sw_design(clusters_per_sequence = c(10, 10, 10, 10),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  expect_s3_class(d, "sw_design")
  expect_equal(dim(d$treatment), c(4, 5))
  expect_equal(d$n_clusters, 40)
  # period 1 is all-baseline
  expect_true(all(d$treatment[, 1] == 0))
})

test_that("sw_design accepts an explicit treatment matrix and derives crossovers", {
  m <- matrix(c(0, 0, 1, 1, 1,
                0, 0, 0, 1, 1,
                0, 0, 0, 0, 1), nrow = 3, byrow = TRUE)
  d <- sw_design(clusters_per_sequence = c(8, 10, 12), treatment = m)
  expect_equal(unname(d$crossover_period), c(3, 4, 5))
  expect_equal(d$n_clusters, 30)
})

test_that("sw_assumptions validates and resolves icc/cluster_sd", {
  a <- sw_assumptions(baseline_prob = 0.05, treatment_or = 1.5, icc = 0.05)
  expect_s3_class(a, "sw_assumptions")
  expect_equal(a$treatment_effect, log(1.5), tolerance = 1e-8)
  expect_error(
    sw_assumptions(baseline_prob = 0.05, treatment_or = 1.5,
                   icc = 0.05, cluster_sd = 0.9),
    "disagree"
  )
})

test_that("simulate_swcrt returns generic aggregated columns", {
  d <- sw_design(clusters_per_sequence = c(5, 5, 5, 5),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.03,
                      n_per_cluster_period = 20)
  sim <- simulate_swcrt(d, a, seed = 1)
  expect_true(all(c("cluster_id", "sequence", "period", "intervention",
                    "n", "events") %in% names(sim)))
  expect_equal(nrow(sim), 20 * 5)
})

test_that("period effects and matrix sample sizes are honoured", {
  d <- sw_design(clusters_per_sequence = c(3, 3), crossover_period = c(2, 3),
                 n_periods = 3)
  ss <- matrix(c(10, 20, 30, 40, 50, 60), nrow = 2, byrow = TRUE)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 1.5, icc = 0.02,
                      period_effects = c(0, 0.2, 0.4),
                      n_per_cluster_period = ss)
  sim <- simulate_swcrt(d, a, seed = 1)
  # each sequence-period should match the ss matrix
  expect_equal(unique(sim$n[sim$sequence_idx == 1 & sim$period == 1]), 10L)
  expect_equal(unique(sim$n[sim$sequence_idx == 2 & sim$period == 3]), 60L)
})

test_that("fit_stepwedge_model accepts a custom formula", {
  d <- sw_design(clusters_per_sequence = c(6, 6, 6, 6),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.03,
                      n_per_cluster_period = 30)
  sim <- simulate_swcrt(d, a, seed = 2)
  res <- fit_stepwedge_model(
    sim,
    formula = cbind(events, n - events) ~ intervention + factor(period) +
      (1 | cluster_id)
  )
  expect_true(is.numeric(res$p_value))
})

test_that("power_swcrt returns a rich sw_power object", {
  d <- sw_design(clusters_per_sequence = c(6, 6, 6, 6),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2.5, icc = 0.03,
                      n_per_cluster_period = 25)
  pw <- power_swcrt(d, a, nsim = 5, seed = 1)
  expect_s3_class(pw, "sw_power")
  expect_true(all(c("power", "mcse", "conf.int", "failure_rate",
                    "estimated_effects", "estimated_ses",
                    "convergence_status", "singular_fit",
                    "coverage", "bias") %in% names(pw)))
  expect_output(print(pw), "sw_power")
})

test_that("power_swcrt uses an exact interval that survives the boundary", {
  d <- sw_design(clusters_per_sequence = c(10, 10, 10, 10),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 6,
                      cluster_sd = 0.3, n_per_cluster_period = 40)
  pw <- power_swcrt(d, a, nsim = 20, seed = 5)
  expect_equal(pw$power, 1)
  # A Wald interval would collapse to exactly [1, 1]; the exact one must not.
  expect_lt(pw$conf.int[1], 1)
  expect_gt(pw$conf.int[1], 0.5)
  expect_equal(pw$conf.int[2], 1)
})

test_that("legacy estimate_power inherits the exact interval", {
  pw <- estimate_power(
    n_simulations = 20, treatment_or = 6,
    n_clusters_per_sequence = c(10, 10, 10, 10),
    baseline_probs = rep(0.1, 4), cluster_sd = 0.3,
    n_per_cluster_period = 40, seed = 5
  )
  expect_equal(pw$power, 1)
  expect_lt(pw$conf.int[1], 1)
})
