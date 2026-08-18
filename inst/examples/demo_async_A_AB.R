# Asynchronous Control -> A -> A+B power comparison
#
# Increase nsim to at least 5,000 for a manuscript analysis, or choose nsim
# from the desired Monte Carlo precision.

library(stepwedgepower)

n_periods <- 12L

three_state <- sw_multistage_design(
  clusters_per_sequence = rep(6L, 8),
  a_start = c(2, 3, 4, 5, 6, 7, 8, 9),
  b_start = c(6, 8, 7, 10, 9, 11, 12, 12),
  n_periods = n_periods,
  sequence_names = paste0("S", 1:8)
)

two_state <- sw_multistage_design(
  clusters_per_sequence = three_state$clusters_per_sequence,
  a_start = three_state$a_start,
  b_start = rep(Inf, three_state$n_sequences),
  n_periods = n_periods,
  sequence_names = three_state$sequence_names
)

assumptions <- sw_multistage_assumptions(
  baseline_prob = 0.15,
  treatment_or_a = 1.40,
  incremental_or_b = 1.30,
  delay_a = 1L,
  delay_b = 1L,
  icc = 0.05,
  period_effects = log(seq(1.00, 1.12, length.out = n_periods)),
  n_per_cluster_period = 30L
)

print(audit_multistage_design(
  three_state,
  delay_a = assumptions$delay_a,
  delay_b = assumptions$delay_b
))

comparison <- compare_multistage_designs(
  design_two = two_state,
  design_three = three_state,
  assumptions_two = assumptions,
  nsim = 200L,
  multiplicity = "holm",
  seed = 2026,
  warn_on_design = FALSE
)
print(comparison)
cat("\nDetailed fit diagnostics by design\n")
print(comparison$fit_diagnostics, row.names = FALSE)

# Global-null calibration check.
type1 <- type1_multistage_swcrt(
  design = three_state,
  assumptions = assumptions,
  nsim = 200L,
  multiplicity = "holm",
  seed = 22026,
  warn_on_design = FALSE
)
print(type1)
cat("\nGlobal-null fit diagnostics\n")
print(type1$fit_diagnostics, row.names = FALSE)
