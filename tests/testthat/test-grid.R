test_that("power_grid runs the full cartesian product", {
  d <- sw_design(clusters_per_sequence = c(4, 4, 4, 4),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.05,
                      n_per_cluster_period = 20)
  g <- power_grid(d, a, vary = list(treatment_or = c(1.5, 2.5),
                                    icc = c(0.02, 0.10)),
                  nsim = 5, seed = 1)
  expect_s3_class(g, "sw_power_grid")
  expect_equal(nrow(g), 4)
  expect_true(all(c("power", "mcse", "conf_low", "conf_high") %in% names(g)))
  expect_length(attr(g, "runs"), 4)
})

test_that("power_grid can vary the number of clusters", {
  d <- sw_design(clusters_per_sequence = c(4, 4, 4, 4),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.05,
                      n_per_cluster_period = 20)
  g <- power_grid(
    d, a,
    vary = list(clusters_per_sequence = list(c(3, 3, 3, 3), c(6, 6, 6, 6))),
    nsim = 5, seed = 1
  )
  expect_equal(nrow(g), 2)
  runs <- attr(g, "runs")
  expect_equal(runs[[1]]$design$n_clusters, 12)
  expect_equal(runs[[2]]$design$n_clusters, 24)
})

test_that("power_grid rejects unknown parameters", {
  d <- sw_design(clusters_per_sequence = c(4, 4), crossover_period = c(2, 3),
                 n_periods = 3)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.05)
  expect_error(power_grid(d, a, vary = list(nonsense = 1:2), nsim = 2),
               "unknown parameter")
})

test_that("results are reproducible regardless of core count", {
  d <- sw_design(clusters_per_sequence = c(4, 4, 4, 4),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.05,
                      n_per_cluster_period = 20)
  serial <- power_swcrt(d, a, nsim = 6, seed = 42, n_cores = 1)
  # Two cores must reproduce the serial run exactly; a power estimate that
  # moves when you add cores is not usable.
  parallel_run <- power_swcrt(d, a, nsim = 6, seed = 42, n_cores = 2)
  expect_equal(serial$p_values, parallel_run$p_values)
  expect_equal(serial$power, parallel_run$power)
})

test_that("the same seed gives the same answer twice", {
  d <- sw_design(clusters_per_sequence = c(4, 4, 4, 4),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.05,
                      n_per_cluster_period = 20)
  expect_equal(
    power_swcrt(d, a, nsim = 5, seed = 7)$p_values,
    power_swcrt(d, a, nsim = 5, seed = 7)$p_values
  )
})
