test_that("component design accepts all four states and withdrawals", {
  state <- rbind(
    S1 = c("Control", "A", "A+B", "B"),
    S2 = c("Control", "B", "A+B", "A+B")
  )
  d <- sw_component_design(c(2, 3), state = state)
  expect_s3_class(d, "sw_component_design")
  expect_equal(unname(d$state[1, ]), c(0L, 1L, 3L, 2L))
  expect_true(d$has_a_only)
  expect_true(d$has_b_only)
  expect_true(d$has_ab)
  expect_equal(d$withdrawal_count_a, 1L)
})

test_that("component carryover follows the specified weights", {
  d <- sw_component_design(
    1,
    state = matrix(c(0, 1, 1, 3, 2, 2, 2), nrow = 1)
  )
  a <- sw_component_assumptions(
    baseline_prob = 0.2,
    treatment_or_a = 1.5,
    treatment_or_b = 1.3,
    interaction_or = 1.1,
    carryover_periods_a = 2,
    carryover_weights_a = c(0.5, 0.25),
    icc = 0,
    n_per_cluster_period = 20
  )
  sim <- simulate_component_swcrt(d, a, seed = 1)
  expect_equal(sim$a_effect_weight, c(0, 1, 1, 1, 0.5, 0.25, 0))
  expect_equal(sim$b_effect_weight, c(0, 0, 0, 1, 1, 1, 1))
  expect_equal(sim$ab_effect_weight, c(0, 0, 0, 1, 0.5, 0.25, 0))
})

test_that("restart can reset or resume the wash-in clock", {
  d <- sw_component_design(
    1,
    state = matrix(c(0, 1, 0, 1), nrow = 1)
  )
  reset <- sw_component_assumptions(
    baseline_prob = 0.2, treatment_or_a = 1.5,
    delay_a = 1, restart_rule_a = "reset", icc = 0
  )
  resume <- sw_component_assumptions(
    baseline_prob = 0.2, treatment_or_a = 1.5,
    delay_a = 1, restart_rule_a = "resume", icc = 0
  )
  expect_equal(
    simulate_component_swcrt(d, reset, seed = 1)$a_effect_weight,
    c(0, 0, 0, 0)
  )
  expect_equal(
    simulate_component_swcrt(d, resume, seed = 1)$a_effect_weight,
    c(0, 0, 0, 1)
  )
})

test_that("four-state audit supports the standard contrasts", {
  state <- rbind(
    S1 = c(0, 0, 1, 1, 3, 3, 2, 2),
    S2 = c(0, 0, 0, 1, 1, 3, 3, 2),
    S3 = c(0, 0, 2, 2, 3, 3, 3, 3),
    S4 = c(0, 0, 0, 2, 2, 3, 3, 3),
    S5 = c(0, 0, 1, 1, 1, 3, 3, 3),
    S6 = c(0, 0, 2, 2, 2, 3, 3, 3),
    S7 = c(0, 0, 0, 0, 1, 1, 3, 2),
    S8 = c(0, 0, 0, 0, 2, 2, 3, 1)
  )
  d <- sw_component_design(rep(3, 8), state = state)
  a <- sw_component_assumptions(
    baseline_prob = 0.15, treatment_or_a = 1.4,
    treatment_or_b = 1.3, interaction_or = 1.1,
    carryover_periods_a = 1, carryover_weights_a = 0.5,
    carryover_periods_b = 1, carryover_weights_b = 0.5,
    icc = 0.05
  )
  audit <- audit_component_design(d, a)
  expect_true(audit$full_rank)
  expect_true(audit$all_requested_estimable)
  expect_equal(nrow(audit$estimability), 6)
})

test_that("component model returns all standard contrasts", {
  state <- rbind(
    S1 = c(0, 0, 1, 1, 3, 3, 2, 2),
    S2 = c(0, 0, 0, 1, 1, 3, 3, 2),
    S3 = c(0, 0, 2, 2, 3, 3, 3, 3),
    S4 = c(0, 0, 0, 2, 2, 3, 3, 3),
    S5 = c(0, 0, 1, 1, 1, 3, 3, 3),
    S6 = c(0, 0, 2, 2, 2, 3, 3, 3),
    S7 = c(0, 0, 0, 0, 1, 1, 3, 2),
    S8 = c(0, 0, 0, 0, 2, 2, 3, 1)
  )
  d <- sw_component_design(rep(4, 8), state = state)
  a <- sw_component_assumptions(
    baseline_prob = 0.2, treatment_or_a = 1.8,
    treatment_or_b = 1.7, interaction_or = 1.2,
    icc = 0.03, n_per_cluster_period = 50
  )
  fit <- fit_component_model(
    simulate_component_swcrt(d, a, seed = 3),
    multiplicity = "holm",
    nAGQ = 0
  )
  expect_true(isTRUE(fit$converged))
  expect_true(all(is.finite(fit$contrasts$estimate)))
  expect_true(all(c(
    "A_vs_control", "B_vs_control", "AB_vs_control",
    "AB_vs_A", "AB_vs_B", "interaction"
  ) %in% fit$contrasts$contrast))
  expect_true(all(c("estimate", "std_error", "p_value", "p_adjusted") %in%
                    names(fit$contrasts)))
})

test_that("component power is reproducible and failure-aware", {
  state <- rbind(
    S1 = c(0, 1, 1, 3, 2), S2 = c(0, 2, 2, 3, 1),
    S3 = c(0, 0, 1, 3, 3), S4 = c(0, 0, 2, 3, 3),
    S5 = c(0, 1, 3, 3, 2), S6 = c(0, 2, 3, 3, 1)
  )
  d <- sw_component_design(rep(3, 6), state = state)
  a <- sw_component_assumptions(
    baseline_prob = 0.2, treatment_or_a = 1.8,
    treatment_or_b = 1.6, interaction_or = 1.1,
    icc = 0.03, n_per_cluster_period = 40
  )
  first <- power_component_swcrt(
    d, a, nsim = 2, seed = 12, nAGQ = 0, warn_on_design = FALSE
  )
  second <- power_component_swcrt(
    d, a, nsim = 2, seed = 12, nAGQ = 0, warn_on_design = FALSE
  )
  expect_s3_class(first, "sw_component_power")
  expect_equal(first$raw_p_values, second$raw_p_values)
  expect_true(all(first$power_table$failure_aware_power <=
                    first$power_table$conditional_power, na.rm = TRUE))
  expect_true(all(c(
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

test_that("cumulative designs convert to component coding", {
  old <- sw_multistage_design(
    c(2, 2), a_start = c(2, 3), b_start = c(4, 5), n_periods = 5
  )
  new <- as_component_design(old)
  expect_s3_class(new, "sw_component_design")
  expect_true(any(new$state == 3L))
  expect_false(any(new$state == 2L))
})

test_that("interaction mode and analysis specification are consistent", {
  d <- sw_component_design(
    c(2, 2),
    state = rbind(c(0, 1, 3, 2), c(0, 2, 3, 1))
  )
  no_interaction <- expect_warning(
    sw_component_assumptions(
      baseline_prob = 0.2,
      treatment_or_a = 1.3,
      treatment_or_b = 1.2,
      interaction_or = 1.5,
      interaction_mode = "none",
      icc = 0.05
    ),
    "ignored"
  )
  expect_equal(no_interaction$interaction_effect, 0)
  expect_error(
    power_component_swcrt(
      d, no_interaction, nsim = 1, include_interaction = TRUE,
      warn_on_design = FALSE
    ),
    "include_interaction = FALSE"
  )

  nonzero_interaction <- sw_component_assumptions(
    baseline_prob = 0.2,
    treatment_or_a = 1.3,
    treatment_or_b = 1.2,
    interaction_or = 1.1,
    icc = 0.05
  )
  expect_error(
    power_component_swcrt(
      d, nonzero_interaction, nsim = 1, include_interaction = FALSE,
      warn_on_design = FALSE
    ),
    "nonzero interaction"
  )
})

test_that("incomplete component designs distinguish dashes from Control", {
  d <- sw_incomplete_component_design(
    c(2, 2, 3, 3),
    rbind(
      `Group 1` = c("0", "1", "1+2", "1+2", "-"),
      `Group 2` = c("0", "0", "1", "1+2", "-"),
      `Group 3` = c("-", "0", "1", "1+2", "1+2"),
      `Group 4` = c("-", "0", "0", "1", "1+2")
    )
  )

  expect_s3_class(d, "sw_component_design")
  expect_true(d$is_incomplete)
  expect_equal(sum(d$observed), 16L)
  expect_equal(d$n_observed_cluster_periods, 40L)
  expect_equal(d$n_missing_cluster_periods, 10L)
  expect_equal(unname(d$state[1, ]), c(0L, 1L, 3L, 3L, 3L))
  expect_equal(unname(d$state[3, ]), c(0L, 0L, 1L, 3L, 3L))
  expect_false(d$observed[1, 5])
  expect_false(d$observed[3, 1])
})

test_that("incomplete simulation omits structurally unobserved rows", {
  d <- sw_incomplete_component_design(
    c(2, 2, 3, 3),
    rbind(
      c("0", "1", "1+2", "1+2", "-"),
      c("0", "0", "1", "1+2", "-"),
      c("-", "0", "1", "1+2", "1+2"),
      c("-", "0", "0", "1", "1+2")
    )
  )
  a <- sw_component_assumptions(
    baseline_prob = 0.15,
    treatment_or_a = 1.35,
    treatment_or_b = 1.25,
    interaction_mode = "none",
    icc = 0.05,
    n_per_cluster_period = 25
  )

  observed_only <- simulate_component_swcrt(d, a, seed = 1)
  complete_history <- simulate_component_swcrt(
    d, a, seed = 1, include_unobserved = TRUE
  )

  expect_equal(nrow(observed_only), 40L)
  expect_true(all(observed_only$observed))
  expect_equal(nrow(complete_history), 50L)
  expect_equal(sum(!complete_history$observed), 10L)
  expect_true(all(is.na(complete_history$n[!complete_history$observed])))
  expect_true(all(is.na(complete_history$events[!complete_history$observed])))
})

test_that("resource summary uses observed rather than calendar periods", {
  swd <- sw_incomplete_component_design(
    c(5, 5),
    rbind(c("0", "1", "1+2", "1+2"),
          c("0", "0", "1", "1+2"))
  )
  bswd <- sw_incomplete_component_design(
    c(2, 2, 3, 3),
    rbind(
      c("0", "1", "1+2", "1+2", "-"),
      c("0", "0", "1", "1+2", "-"),
      c("-", "0", "1", "1+2", "1+2"),
      c("-", "0", "0", "1", "1+2")
    )
  )
  a <- sw_component_assumptions(
    baseline_prob = 0.15,
    treatment_or_a = 1.35,
    treatment_or_b = 1.25,
    interaction_mode = "none",
    icc = 0.05,
    n_per_cluster_period = 25
  )

  r_swd <- component_resource_summary(swd, a)
  r_bswd <- component_resource_summary(bswd, a)
  expect_equal(r_swd$calendar_cluster_periods, 40L)
  expect_equal(r_swd$observed_cluster_periods, 40L)
  expect_equal(r_bswd$calendar_cluster_periods, 50L)
  expect_equal(r_bswd$observed_cluster_periods, 40L)
  expect_equal(r_bswd$missing_cluster_periods, 10L)
  expect_equal(r_swd$total_individual_observations, 1000)
  expect_equal(r_bswd$total_individual_observations, 1000)
})

test_that("internal missing periods require an explicit latent schedule", {
  expect_error(
    sw_incomplete_component_design(
      1,
      matrix(c("0", "-", "1"), nrow = 1)
    ),
    "internal missing period"
  )

  d <- sw_incomplete_component_design(
    1,
    matrix(c("0", "-", "1"), nrow = 1),
    latent_state = matrix(c("0", "0", "1"), nrow = 1)
  )
  expect_false(d$observed[1, 2])
  expect_equal(unname(d$state[1, ]), c(0L, 0L, 1L))
})

test_that("incomplete design audit uses observed cells", {
  d <- sw_incomplete_component_design(
    c(2, 2, 3, 3),
    rbind(
      c("0", "1", "1+2", "1+2", "-"),
      c("0", "0", "1", "1+2", "-"),
      c("-", "0", "1", "1+2", "1+2"),
      c("-", "0", "0", "1", "1+2")
    )
  )
  a <- sw_component_assumptions(
    baseline_prob = 0.15,
    treatment_or_a = 1.35,
    treatment_or_b = 1.25,
    interaction_mode = "none",
    icc = 0.05,
    n_per_cluster_period = 25
  )
  audit <- audit_component_design(
    d, a,
    contrasts = c("A_vs_control", "AB_vs_A", "AB_vs_control"),
    include_interaction = FALSE
  )

  expect_true(audit$full_rank)
  expect_true(audit$all_requested_estimable)
  expect_equal(audit$observed_cluster_periods, 40L)
  expect_equal(audit$missing_cluster_periods, 10L)
  expect_equal(audit$observed_sequences_per_period, c(2L, 4L, 4L, 4L, 2L))
})
<<<<<<< HEAD

test_that("fit diagnostic categories are explicit and internally consistent", {
  counts <- .analysis_evaluability_counts(
    finite_result = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    converged = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    hard_error = c(TRUE, FALSE, FALSE, FALSE, FALSE),
    singular = c(NA, FALSE, FALSE, TRUE, FALSE)
  )
  expect_equal(counts$n_hard_glmm_error, 1L)
  expect_equal(counts$n_nonconverged_fit, 1L)
  expect_equal(counts$n_converged_nonfinite_contrast, 1L)
  expect_equal(counts$n_singular_fit, 1L)
  expect_equal(counts$n_successful_fit, 2L)

  replicate_diagnostics <- .make_replicate_diagnostics(
    fit_status = c(
      "hard_glmm_error", "nonconverged_fit",
      "converged_nonfinite_contrast", "successful_fit", "successful_fit"
    ),
    singular = c(NA, FALSE, FALSE, TRUE, FALSE),
    has_warning = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    nonfinite_contrasts = c("A_vs_control", "", "AB_vs_A", "", ""),
    error_message = c("fit failed", NA, NA, NA, NA),
    convergence_message = c(NA, "did not converge", NA, NA, NA),
    optimizer_code = c(NA, "1", "0", "0", "0")
  )
  summary <- .summarize_fit_diagnostics(replicate_diagnostics)
  primary <- summary$category != "singular_fit"
  expect_equal(sum(summary$count[primary]), 5L)
  expect_equal(
    summary$count[summary$category == "successful_fit"],
    2L
  )
  expect_equal(
    summary$count[summary$category == "singular_fit"],
    1L
  )
})

test_that("singularity messages are advisory rather than nonconvergence", {
  messages <- .partition_lme4_messages(c(
    "boundary (singular) fit: see help('isSingular')",
    "Model failed to converge with max|grad| = 0.01"
  ))
  expect_equal(
    messages$advisory,
    "boundary (singular) fit: see help('isSingular')"
  )
  expect_equal(
    messages$convergence,
    "Model failed to converge with max|grad| = 0.01"
  )
})
=======
>>>>>>> origin/main
