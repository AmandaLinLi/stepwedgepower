# stepwedgepower 0.4.0: Control, A, B, and A+B demo
#
# Some sequences follow Control -> A -> A+B -> B, so A is withdrawn.
# Other sequences follow Control -> B -> A+B, and one sequence withdraws B.
# Carryover is specified separately for A and B.

library(stepwedgepower)

state <- rbind(
  S1 = c(0, 0, 1, 1, 3, 3, 2, 2, 2, 2),
  S2 = c(0, 0, 0, 1, 1, 3, 3, 2, 2, 2),
  S3 = c(0, 0, 2, 2, 3, 3, 3, 3, 3, 3),
  S4 = c(0, 0, 0, 2, 2, 3, 3, 3, 3, 3),
  S5 = c(0, 0, 1, 1, 1, 3, 3, 3, 3, 3),
  S6 = c(0, 0, 2, 2, 2, 3, 3, 3, 3, 3),
  S7 = c(0, 0, 0, 0, 0, 1, 1, 3, 2, 2),
  S8 = c(0, 0, 0, 0, 0, 2, 2, 3, 1, 1)
)

design <- sw_component_design(
  clusters_per_sequence = rep(5L, 8),
  state = state
)

assumptions <- sw_component_assumptions(
  baseline_prob = 0.15,
  treatment_or_a = 1.35,
  treatment_or_b = 1.25,
  interaction_or = 1.10,
  delay_a = 1L,
  delay_b = 1L,
  carryover_periods_a = 2L,
  carryover_periods_b = 2L,
  carryover_weights_a = c(0.60, 0.30),
  carryover_weights_b = c(0.50, 0.25),
  restart_rule_a = "reset",
  restart_rule_b = "reset",
  interaction_mode = "effective_overlap",
  icc = 0.05,
  period_effects = log(seq(1.00, 1.10, length.out = ncol(state))),
  n_per_cluster_period = 25L
)

# Structural audit under the exact wash-in and carryover rules.
audit <- audit_component_design(design, assumptions)
print(audit)

# One simulated trial and its planned mixed-model analysis.
one_trial <- simulate_component_swcrt(design, assumptions, seed = 123)
fit <- fit_component_model(
  one_trial,
  contrasts = c(
    "A_vs_control", "B_vs_control", "AB_vs_control",
    "AB_vs_A", "AB_vs_B", "interaction"
  ),
  multiplicity = "holm",
  multiplicity_family = c("A_vs_control", "B_vs_control", "interaction")
)
print(fit$contrasts, row.names = FALSE, digits = 4)

# Inspect periods after A withdrawal for the first cluster in sequence S1.
carryover_example <- subset(
  one_trial,
  sequence == "S1" & cluster_id == min(cluster_id[sequence == "S1"]) &
    period >= 5,
  select = c(
    period, state, a_active, b_active,
    a_current_weight, a_carryover_weight, a_effect_weight,
    b_effect_weight, ab_effect_weight
  )
)
print(carryover_example, row.names = FALSE)

# Quick demonstration. Use at least 5,000 simulations and nAGQ = 1 for a
# manuscript analysis, with the Monte Carlo target specified in advance.
power <- power_component_swcrt(
  design = design,
  assumptions = assumptions,
  nsim = 100,
  multiplicity = "holm",
  multiplicity_family = c("A_vs_control", "B_vs_control", "interaction"),
  nAGQ = 0,
  n_cores = 1,
  seed = 2026,
  warn_on_design = FALSE
)
summary(power)

# Global-null calibration uses the same schedule and nuisance assumptions.
# type1 <- type1_component_swcrt(
#   design, assumptions, nsim = 5000,
#   multiplicity = "holm",
#   multiplicity_family = c("A_vs_control", "B_vs_control", "interaction"),
#   nAGQ = 1, n_cores = 4, seed = 22026,
#   warn_on_design = FALSE
# )
# summary(type1)
