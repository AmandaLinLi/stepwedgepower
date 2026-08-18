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
    `Group 1` = c("0", "1", "1+2", "1+2", "-"),
    `Group 2` = c("0", "0", "1", "1+2", "-"),
    `Group 3` = c("-", "0", "1", "1+2", "1+2"),
    `Group 4` = c("-", "0", "0", "1", "1+2")
  ),
  batch = c("Batch 1", "Batch 1", "Batch 2", "Batch 2")
)

assumptions <- sw_batched_assumptions(
  baseline_prob = 0.15,
  treatment_or_a = 1.35,
  treatment_or_b = 1.25,
  interaction_mode = "none",
  icc = 0.05,
  n_per_cluster_period = 25L,
  time_model = "calendar",
  time_effects = log(seq(1.00, 1.08, length.out = 5))
)

contrasts <- c("A_vs_control", "AB_vs_A", "AB_vs_control")

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
  contrasts = contrasts,
  include_interaction = FALSE,
  multiplicity = "holm",
  multiplicity_family = contrasts,
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
  n_cores = n_cores,
  seed = 2026,
  warn_on_design = FALSE
)
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
  contrasts = contrasts,
  include_interaction = FALSE,
  multiplicity = "holm",
  multiplicity_family = contrasts,
  nAGQ = nAGQ_demo,
  n_cores = n_cores,
  seed = 22026,
  warn_on_design = FALSE
)
print(type1)
