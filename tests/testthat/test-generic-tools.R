test_that("generic data tools reproduce the specialty wrappers", {
  dat <- read_example_physician_data()
  generic <- prepare_cluster_data(
    dat,
    groups = c("CARDIOLOGY", "FAMILY MEDICINE", "INTERNAL MEDICINE", "NEUROLOGY"),
    min_size = 100, max_size = 10000,
    group_var = "specialty", size_var = "n_total_pat",
    order_var = "PROV_NAME"
  )
  wrapped <- prepare_physician_data(dat)
  expect_equal(nrow(generic), nrow(wrapped))

  g <- summarize_by_group(dat, group_var = "specialty", vars = "n_total_pat")
  s <- summarize_by_specialty(dat, vars = "n_total_pat")
  expect_equal(g$median, s$median)
  expect_true("group" %in% names(g))
  expect_true("specialty" %in% names(s))
})

test_that("generic rate model reproduces the specialty rate model", {
  dat <- read_example_physician_data()
  generic <- fit_grouped_rate_model(
    dat, successes = "n_lpa_pat", trials = "n_total_pat", group = "specialty"
  )
  wrapped <- fit_specialty_rate_model(
    dat, successes = "n_lpa_pat", trials = "n_total_pat"
  )
  expect_equal(unname(stats::coef(generic)), unname(stats::coef(wrapped)))

  rates <- estimate_group_rates(generic, group = "specialty",
                                approximate_marginal = FALSE)
  expect_true(all(rates$probability > 0 & rates$probability < 1))
  expect_true("group" %in% names(rates))
})
