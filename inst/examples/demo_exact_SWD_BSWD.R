<<<<<<< HEAD
# Reproducible SWD/BSWD demonstration for stepwedgepower 0.4.1
#
# 0 = Control, 1 = intervention A, and 1+2 = A+B.
# A dash is a structurally unobserved clinic-period, not Control.

library(stepwedgepower)

n_sims <- as.integer(Sys.getenv("STEPWEDGE_NSIM", unset = "200"))
n_cores <- as.integer(Sys.getenv("STEPWEDGE_CORES", unset = "1"))
nAGQ_demo <- as.integer(Sys.getenv("STEPWEDGE_NAGQ", unset = "0"))

swd <- sw_batched_design(
  c(5L, 5L),
  rbind(
    `Group 1` = c("0", "1", "1+2", "1+2"),
    `Group 2` = c("0", "0", "1", "1+2")
  ),
  batch = c("Batch 1", "Batch 1")
)

bswd <- sw_batched_design(
  c(2L, 2L, 3L, 3L),
  rbind(
=======
# Exact SWD and BSWD comparison with structurally unobserved periods
#
# Manuscript notation:
#   0   = Control
#   1   = intervention A
#   1+2 = intervention A+B
#   -   = no clinic-period observation (not Control)

if (!requireNamespace("stepwedgepower", quietly = TRUE) ||
    utils::packageVersion("stepwedgepower") < "0.4.0") {
  stop("Install stepwedgepower version 0.4.0 or later.")
}
library(stepwedgepower)

# Use at least 5,000 simulations and nAGQ = 1 for a manuscript analysis.
nsim <- 200L
n_cores <- 1L
nAGQ <- 0L

# (a) SWD design: 4 periods, 10 clinics
swd <- sw_incomplete_component_design(
  clusters_per_sequence = c(5L, 5L),
  state = rbind(
    `Group 1` = c("0", "1", "1+2", "1+2"),
    `Group 2` = c("0", "0", "1", "1+2")
  )
)

# (b) BSWD design: 5 periods, 10 clinics. A dash is omitted from
# simulation outcomes, model fitting, power, and observed-resource totals.
bswd <- sw_incomplete_component_design(
  clusters_per_sequence = c(2L, 2L, 3L, 3L),
  state = rbind(
>>>>>>> origin/main
    `Group 1` = c("0", "1", "1+2", "1+2", "-"),
    `Group 2` = c("0", "0", "1", "1+2", "-"),
    `Group 3` = c("-", "0", "1", "1+2", "1+2"),
    `Group 4` = c("-", "0", "0", "1", "1+2")
<<<<<<< HEAD
  ),
  batch = c("Batch 1", "Batch 1", "Batch 2", "Batch 2")
)

assumptions <- sw_batched_assumptions(
=======
  )
)

cat("\n(a) SWD design\n")
print(swd)
cat("\n(b) BSWD design\n")
print(bswd)

# B is never delivered alone. In an additive model, the B coefficient is the
# incremental A+B versus A effect.
assumptions <- sw_component_assumptions(
>>>>>>> origin/main
  baseline_prob = 0.15,
  treatment_or_a = 1.35,
  treatment_or_b = 1.25,
  interaction_mode = "none",
<<<<<<< HEAD
  icc = 0.05,
  n_per_cluster_period = 25L,
  time_model = "calendar",
  time_effects = log(seq(1.00, 1.08, length.out = 5))
=======
  delay_a = 0L,
  delay_b = 0L,
  icc = 0.05,
  period_effects = NULL,
  n_per_cluster_period = 25L
>>>>>>> origin/main
)

contrasts <- c("A_vs_control", "AB_vs_A", "AB_vs_control")

<<<<<<< HEAD
print(swd)
print(bswd)
print(rbind(
  SWD = component_resource_summary(swd, assumptions),
  BSWD = component_resource_summary(bswd, assumptions)
))

for (model in c("calendar", "time_on_trial", "separate")) {
  print(audit_batched_design(
    bswd, assumptions, model, contrasts,
    include_interaction = FALSE
  ))
}

one_trial <- simulate_batched_swcrt(bswd, assumptions, seed = 123)
one_fit <- fit_batched_model(
  one_trial,
  time_model = "calendar",
=======
cat("\nObserved-resource comparison\n")
resources <- rbind(
  SWD = component_resource_summary(swd, assumptions),
  BSWD = component_resource_summary(bswd, assumptions)
)
print(resources)
stopifnot(
  resources["SWD", "observed_cluster_periods"] == 40L,
  resources["BSWD", "observed_cluster_periods"] == 40L,
  resources["SWD", "total_individual_observations"] == 1000,
  resources["BSWD", "total_individual_observations"] == 1000
)

cat("\nDesign audits\n")
print(audit_component_design(
  swd, assumptions,
  contrasts = contrasts,
  include_interaction = FALSE
))
print(audit_component_design(
  bswd, assumptions,
  contrasts = contrasts,
  include_interaction = FALSE
))

# One BSWD trial contains 40 observed rows by default. Set
# include_unobserved = TRUE to inspect all 50 latent calendar rows; the 10
# structurally unobserved rows then have missing n and events.
one_bswd_trial <- simulate_component_swcrt(bswd, assumptions, seed = 123)
stopifnot(nrow(one_bswd_trial) == 40L)

comparison <- compare_component_designs(
  designs = list(SWD = swd, BSWD = bswd),
  assumptions = assumptions,
  nsim = nsim,
>>>>>>> origin/main
  contrasts = contrasts,
  include_interaction = FALSE,
  multiplicity = "holm",
  multiplicity_family = contrasts,
<<<<<<< HEAD
  nAGQ = nAGQ_demo
)
print(one_fit$contrasts, row.names = FALSE)

comparison <- compare_swd_bswd(
  swd, bswd, assumptions,
  nsim = n_sims,
  contrasts = contrasts,
  include_interaction = FALSE,
  multiplicity = "holm",
  multiplicity_family = contrasts,
  nAGQ = nAGQ_demo,
=======
  nAGQ = nAGQ,
>>>>>>> origin/main
  n_cores = n_cores,
  seed = 2026,
  warn_on_design = FALSE
)
<<<<<<< HEAD
print(comparison)

time_comparison <- compare_batched_time_models(
  bswd, assumptions,
  nsim = n_sims,
  contrasts = contrasts,
  include_interaction = FALSE,
  multiplicity = "holm",
  multiplicity_family = contrasts,
  nAGQ = nAGQ_demo,
  n_cores = n_cores,
  seed = 2026,
  warn_on_design = FALSE
)
print(time_comparison)

type1 <- type1_batched_swcrt(
  bswd, assumptions,
  nsim = n_sims,
=======

cat("\nPower comparison\n")
print(comparison)

# Global-null rejection rates under the same schedules and resources.
null_assumptions <- sw_component_assumptions(
  baseline_prob = 0.15,
  treatment_or_a = 1,
  treatment_or_b = 1,
  interaction_mode = "none",
  icc = 0.05,
  n_per_cluster_period = 25L
)

null_comparison <- compare_component_designs(
  designs = list(SWD = swd, BSWD = bswd),
  assumptions = null_assumptions,
  nsim = nsim,
>>>>>>> origin/main
  contrasts = contrasts,
  include_interaction = FALSE,
  multiplicity = "holm",
  multiplicity_family = contrasts,
<<<<<<< HEAD
  nAGQ = nAGQ_demo,
=======
  nAGQ = nAGQ,
>>>>>>> origin/main
  n_cores = n_cores,
  seed = 22026,
  warn_on_design = FALSE
)
<<<<<<< HEAD
print(type1)
=======

cat("\nGlobal-null rejection-rate comparison\n")
print(null_comparison)

# Optional machine-readable outputs.
utils::write.csv(
  comparison$comparison,
  "SWD_BSWD_power_comparison.csv",
  row.names = FALSE
)
utils::write.csv(
  comparison$resource_check,
  "SWD_BSWD_resource_comparison.csv",
  row.names = FALSE
)
utils::write.csv(
  null_comparison$comparison,
  "SWD_BSWD_null_rejection_comparison.csv",
  row.names = FALSE
)
>>>>>>> origin/main
