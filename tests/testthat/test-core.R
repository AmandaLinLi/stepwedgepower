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
