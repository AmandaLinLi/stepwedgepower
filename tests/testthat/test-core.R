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

test_that("generic simulation returns required columns", {
  sim_dat <- simulate_stepwedge_trial(seed = 1)
  expect_true(all(c("cluster_id", "sequence", "period", "intervention", "n", "events") %in% names(sim_dat)))
  expect_true(all(c("PID", "step", "treat", "n_patients", "n_positive") %in% names(sim_dat)))
})

test_that("ICC conversion round trips", {
  icc <- c(0.01, 0.05, 0.20)
  expect_equal(cluster_sd_to_icc(icc_to_cluster_sd(icc)), icc, tolerance = 1e-10)
})

test_that("invalid ICC is rejected", {
  expect_error(icc_to_cluster_sd(0), "strictly between")
  expect_error(icc_to_cluster_sd(1), "strictly between")
})

test_that("deprecation warnings are emitted once, not once per simulation", {
  count <- 0L
  withCallingHandlers(
    invisible(estimate_power(
      n_simulations = 5, tau_provider = 0.4,
      n_clusters_per_sequence = c(4, 4, 4, 4),
      baseline_probs = rep(0.1, 4), n_per_cluster_period = 20, seed = 1
    )),
    warning = function(w) {
      if (grepl("deprecated", conditionMessage(w))) count <<- count + 1L
      invokeRestart("muffleWarning")
    }
  )
  expect_equal(count, 1L)
})

test_that("supplying a canonical and a deprecated argument together is an error", {
  expect_error(
    simulate_stepwedge_trial(treatment_or = 1.5, effect_size_or = 3.0),
    "only one of"
  )
  expect_error(
    simulate_stepwedge_trial(cluster_sd = 0.5, icc = 0.05),
    "not both"
  )
})

test_that("deprecated aliases still drive the simulation", {
  sim <- suppressWarnings(simulate_stepwedge_trial(
    n_providers_per_specialty = c(2, 2, 2),
    specialty_names = c("A", "B", "C"),
    base_probs = rep(0.1, 3),
    pts_per_step = 10, seed = 1
  ))
  expect_equal(sort(unique(sim$sequence)), c("A", "B", "C"))
  expect_equal(nrow(sim), 6 * 4)  # 6 clusters x (3 sequences + 1) periods
  expect_true(all(sim$n == 10))
})

test_that("exact Monte Carlo interval does not collapse at the boundary", {
  pw <- estimate_power(
    n_simulations = 20, treatment_or = 6,
    n_clusters_per_sequence = c(10, 10, 10, 10),
    baseline_probs = rep(0.1, 4), cluster_sd = 0.3,
    n_per_cluster_period = 40, seed = 5
  )
  expect_equal(pw$power, 1)
  # A Wald interval would give exactly [1, 1]; the exact interval must not.
  expect_lt(pw$conf_low, 1)
  expect_gt(pw$conf_low, 0.5)
  expect_equal(pw$conf_high, 1)
})

test_that("estimate_type1_error exposes explicit arguments", {
  expect_true(all(c("n_simulations", "n_clusters_per_sequence", "icc") %in%
                    names(formals(estimate_type1_error))))
  out <- estimate_type1_error(
    n_simulations = 5, n_clusters_per_sequence = c(6, 6, 6, 6),
    baseline_probs = rep(0.1, 4), icc = 0.05,
    n_per_cluster_period = 20, seed = 3
  )
  expect_s3_class(out, "stepwedge_power")
  expect_equal(out$type1_error, out$power)
})

test_that("analysis accepts generic and legacy column names alike", {
  sim <- simulate_stepwedge_trial(
    n_clusters_per_sequence = c(5, 5, 5, 5),
    baseline_probs = rep(0.1, 4), icc = 0.05, seed = 1
  )
  generic_only <- sim[, c("cluster_id", "sequence_index", "period",
                          "intervention", "n", "events")]
  legacy_only <- sim[, c("PID", "specialty_idx", "step", "treat",
                         "n_patients", "n_positive")]
  expect_true(is.numeric(run_stepwedge_analysis(generic_only)$p_value))
  expect_true(is.numeric(run_stepwedge_analysis(legacy_only)$p_value))
})
