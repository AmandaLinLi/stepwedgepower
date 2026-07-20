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
