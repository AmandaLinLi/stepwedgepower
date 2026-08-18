test_that("multistage design represents asynchronous Control-A-A+B rollout", {
  d <- sw_multistage_design(
    clusters_per_sequence = c(3, 4, 5, 6),
    a_start = c(2, 3, 4, 5),
    b_start = c(5, 7, 6, 8),
    n_periods = 8,
    sequence_names = paste0("S", 1:4)
  )
  expect_s3_class(d, "sw_multistage_design")
  expect_true(d$has_b)
  expect_equal(unname(d$state[1, ]), c(0L, 1L, 1L, 1L, 2L, 2L, 2L, 2L))
  expect_equal(d$a_to_b_lag, c(3, 4, 2, 3))
  expect_false(length(unique(d$b_start)) == 1L)
})

test_that("multistage state matrix must pass through A before A+B", {
  bad <- matrix(c(0, 0, 2, 2,
                  0, 1, 1, 2), nrow = 2, byrow = TRUE)
  expect_error(
    sw_multistage_design(c(2, 2), state = bad),
    "A-only period"
  )

  good <- matrix(c(0, 1, 2, 2,
                   0, 0, 1, 2), nrow = 2, byrow = TRUE)
  d <- sw_multistage_design(c(2, 2), state = good)
  expect_equal(d$a_start, c(2, 3))
  expect_equal(d$b_start, c(3, 4))
})

test_that("delayed A and B indicators use exposure time rather than calendar time", {
  d <- sw_multistage_design(
    clusters_per_sequence = c(1, 1),
    a_start = c(2, 3),
    b_start = c(4, 6),
    n_periods = 6,
    sequence_names = c("S1", "S2")
  )
  a <- sw_multistage_assumptions(
    baseline_prob = 0.2,
    treatment_or_a = 1.5,
    incremental_or_b = 1.3,
    delay_a = 1,
    delay_b = 1,
    icc = 0,
    n_per_cluster_period = 20
  )
  sim <- simulate_multistage_swcrt(d, a, seed = 1)
  s1 <- sim[sim$sequence == "S1", ]
  expect_equal(s1$a_effective, c(0L, 0L, 1L, 1L, 1L, 1L))
  expect_equal(s1$b_effective, c(0L, 0L, 0L, 0L, 1L, 1L))
  expect_true(all(c("state", "a_exposure_time", "b_exposure_time",
                    "a_effective", "b_effective") %in% names(sim)))
})

test_that("design audit detects staggered B and fixed-lag confounding", {
  staggered <- sw_multistage_design(
    rep(3, 4), c(2, 3, 4, 5), c(5, 7, 6, 8), 8
  )
  audit <- audit_multistage_design(staggered, delay_a = 1, delay_b = 1)
  expect_true(audit$full_rank)
  expect_true(audit$staggered_b)
  expect_false(audit$constant_a_to_b_lag)
  expect_true(length(audit$a_ab_periods) > 0)

  fixed_lag <- sw_multistage_design(
    rep(3, 4), c(2, 3, 4, 5), c(5, 6, 7, 8), 8
  )
  audit_fixed <- audit_multistage_design(fixed_lag)
  expect_true(audit_fixed$constant_a_to_b_lag)
  expect_true(any(grepl("same duration", audit_fixed$messages)))
})

test_that("multistage model returns component and total contrasts", {
  d <- sw_multistage_design(
    rep(4, 4), c(2, 3, 4, 5), c(5, 7, 6, 8), 8,
    sequence_names = paste0("S", 1:4)
  )
  a <- sw_multistage_assumptions(
    baseline_prob = 0.15,
    treatment_or_a = 2,
    incremental_or_b = 1.7,
    delay_a = 1,
    delay_b = 1,
    icc = 0.03,
    n_per_cluster_period = 50
  )
  fit <- fit_multistage_model(
    simulate_multistage_swcrt(d, a, seed = 2),
    multiplicity = "holm"
  )
  expect_true(all(c("A_vs_control", "AB_vs_A", "AB_vs_control") %in%
                    fit$contrasts$contrast))
  expect_true(all(c("estimate", "std_error", "p_value", "p_adjusted") %in%
                    names(fit$contrasts)))
  expect_type(fit$converged, "logical")
})

test_that("multistage power is reproducible and failure-aware", {
  d <- sw_multistage_design(
    rep(3, 4), c(2, 3, 4, 5), c(5, 7, 6, 8), 8,
    sequence_names = paste0("S", 1:4)
  )
  a <- sw_multistage_assumptions(
    baseline_prob = 0.15,
    treatment_or_a = 1.8,
    incremental_or_b = 1.5,
    delay_a = 1,
    delay_b = 1,
    icc = 0.05,
    n_per_cluster_period = 40
  )
  first <- power_multistage_swcrt(
    d, a, nsim = 3, seed = 11, warn_on_design = FALSE
  )
  second <- power_multistage_swcrt(
    d, a, nsim = 3, seed = 11, warn_on_design = FALSE
  )
  expect_s3_class(first, "sw_multistage_power")
  expect_equal(first$raw_p_values, second$raw_p_values)
  expect_true(all(first$power_table$failure_aware_power <=
                    first$power_table$conditional_power, na.rm = TRUE))
  expect_true(all(c(
    "power_table", "estimation_table", "design_audit",
    "fit_diagnostics", "replicate_diagnostics", "fit_status",
    "hard_glmm_error", "error_messages", "convergence_messages"
  ) %in% names(first)))
  expect_true(all(c(
    "n_hard_glmm_error", "n_nonconverged_fit",
    "n_converged_nonfinite_contrast", "n_singular_fit",
    "n_successful_fit"
  ) %in% names(first$power_table)))
  primary <- first$fit_diagnostics$category != "singular_fit"
  expect_equal(sum(first$fit_diagnostics$count[primary]), first$nsim)
})

test_that("two-state and three-state designs compare under one interface", {
  three <- sw_multistage_design(
    rep(3, 4), c(2, 3, 4, 5), c(5, 7, 6, 8), 8
  )
  two <- sw_multistage_design(
    rep(3, 4), c(2, 3, 4, 5), rep(Inf, 4), 8
  )
  a <- sw_multistage_assumptions(
    baseline_prob = 0.15, treatment_or_a = 1.8,
    incremental_or_b = 1.5, delay_a = 1, delay_b = 1,
    icc = 0.05, n_per_cluster_period = 40
  )
  comparison <- compare_multistage_designs(
    two, three, a, nsim = 2, seed = 7, warn_on_design = FALSE
  )
  expect_s3_class(comparison, "sw_multistage_comparison")
  expect_true(comparison$equal_structural_resources)
  expect_true(all(c("design", "test", "conditional_power") %in%
                    names(comparison$comparison)))
})
