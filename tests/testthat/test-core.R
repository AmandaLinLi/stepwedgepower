test_that("prepare_physician_data filters rows", {
  dat <- read_example_physician_data()
  out <- prepare_physician_data(dat, min_patients = 100, max_patients = 5000)
  expect_true(all(out$n_total_pat >= 100))
  expect_true(all(out$n_total_pat < 5000))
})

test_that("summaries return expected columns", {
  dat <- read_example_physician_data()
  out <- summarize_by_specialty(dat, vars = c("n_total_pat"))
  expect_true(all(c("variable", "specialty", "median") %in% names(out)))
})

test_that("simulation returns data with required columns", {
  sim_dat <- simulate_stepwedge_trial(seed = 1)
  expect_true(all(c("PID", "step", "treat", "n_patients", "n_positive") %in% names(sim_dat)))
})

test_that("simulation returns generic columns and legacy columns agree", {
  sim <- simulate_stepwedge_trial(seed = 1)
  generic <- c("cluster_id", "sequence", "sequence_idx", "period",
               "intervention", "n", "events")
  legacy <- c("PID", "specialty", "specialty_idx", "step", "treat",
              "n_patients", "n_positive")
  expect_true(all(generic %in% names(sim)))
  expect_true(all(legacy %in% names(sim)))
  expect_identical(sim$events, sim$n_positive)
  expect_identical(sim$intervention, sim$treat)
  expect_identical(sim$period, sim$step)
})

test_that("icc conversions round-trip", {
  sd <- icc_to_cluster_sd(0.05)
  expect_equal(cluster_sd_to_icc(sd), 0.05, tolerance = 1e-8)
  expect_error(icc_to_cluster_sd(1), "must be in")
})

test_that("legacy arguments warn but still work", {
  expect_warning(
    sim <- simulate_stepwedge_trial(tau_provider = 0.5, seed = 1),
    "deprecated"
  )
  expect_true(nrow(sim) > 0)
})

test_that("disagreeing icc and cluster_sd error", {
  expect_error(
    simulate_stepwedge_trial(icc = 0.05, cluster_sd = 0.9),
    "disagree"
  )
})

test_that("estimate_power reports mcse and confidence interval", {
  pw <- estimate_power(
    n_simulations = 5, treatment_or = 3,
    n_clusters_per_sequence = c(6, 6, 6, 6),
    baseline_probs = rep(0.1, 4), cluster_sd = 0.3,
    n_per_cluster_period = 20, seed = 1
  )
  expect_true(all(c("power", "mcse", "conf.int") %in% names(pw)))
  expect_length(pw$conf.int, 2)
  expect_true(is.na(pw$power) || (pw$power >= 0 && pw$power <= 1))
})
