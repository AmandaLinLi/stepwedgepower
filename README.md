# stepwedgepower

[![CRAN status](https://www.r-pkg.org/badges/version/stepwedgepower)](https://CRAN.R-project.org/package=stepwedgepower)
<<<<<<< HEAD
[![R-CMD-check](https://github.com/AmandaLinLi/stepwedgepower/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AmandaLinLi/stepwedgepower/actions)
=======

>>>>>>> origin/main

`stepwedgepower` provides simulation-based design evaluation for stepped-wedge
cluster randomized trials with aggregated binary outcomes. It connects the
rollout schedule, data-generating assumptions, planned mixed model, and
empirical operating characteristics in one reproducible workflow.

Version 0.4.1 supports:

- arbitrary binary stepped-wedge crossover schedules;
- unequal clusters per sequence and flexible cluster-period sizes;
- secular trends and latent-scale ICC input;
- empirical power, type I error, bias, coverage, convergence, singularity, and
  Monte Carlo uncertainty;
- explicit fit-outcome diagnostics separating hard GLMM errors,
  nonconverged fits, converged fits with non-finite contrasts, singular fits,
  and successful fits;
- classic and batched stepped-wedge designs with delayed cluster initiation,
  structurally unobserved cluster-periods, and explicit batch metadata;
- calendar-time, time-on-trial, and separate batch-specific time models for
  simulation, analysis, auditing, power, and type I error evaluation;
- sensitivity grids and reproducible parallel simulation;
- asynchronous cumulative schedules in which clusters move from Control to A
  and later add B at different times;
- separate A, incremental-B, and total A+B contrasts, multiplicity adjustment,
  schedule auditing, conditional power, and failure-aware power;
- a general component engine for Control, A, B, and A+B, including
  arbitrary withdrawal and reintroduction schedules;
<<<<<<< HEAD
- structurally incomplete and batched stepped-wedge schedules in which dashes or
=======
- structurally incomplete and block stepped-wedge schedules in which dashes or
>>>>>>> origin/main
  missing cells contribute no cluster-period outcome;
- separate wash-in, restart, and carryover rules for A and B, together with an
  optional A-by-B interaction;
- standard factorial contrasts, formal estimability checks, and comparisons of
  multiple candidate schedules.

## Installation

```r
install.packages("stepwedgepower")
```

Install the development version from GitHub with:

```r
# install.packages("remotes")
remotes::install_github("AmandaLinLi/stepwedgepower")
```

## Standard stepped-wedge workflow

```r
library(stepwedgepower)

design <- sw_design(
  clusters_per_sequence = c(8, 8, 8, 8),
  crossover_period = c(2, 3, 4, 5),
  n_periods = 5
)

assumptions <- sw_assumptions(
  baseline_prob = 0.10,
  treatment_or = 1.50,
  icc = 0.05,
  period_effects = c(0, 0.05, 0.10, 0.15, 0.20),
  n_per_cluster_period = 30
)

trial <- simulate_swcrt(design, assumptions, seed = 1)
fit <- analyze_swcrt(trial)

# Use substantially more simulations for a final design decision.
power <- power_swcrt(design, assumptions, nsim = 500, seed = 2026)
summary(power)
```


## General Control, A, B, and A+B design

Version 0.4.1 accepts arbitrary four-state schedules. In this example, some
sequences follow `Control -> A -> A+B -> B`, so A is withdrawn and its effect
is allowed to decay over two periods. Other sequences begin with B before
adding A.

```r
state <- rbind(
  S1 = c(0, 0, 1, 1, 3, 3, 2, 2),
  S2 = c(0, 0, 0, 1, 1, 3, 3, 2),
  S3 = c(0, 0, 2, 2, 3, 3, 3, 3),
  S4 = c(0, 0, 0, 2, 2, 3, 3, 3),
  S5 = c(0, 0, 1, 1, 1, 3, 3, 3),
  S6 = c(0, 0, 2, 2, 2, 3, 3, 3)
)

component_design <- sw_component_design(
  clusters_per_sequence = rep(5, 6),
  state = state
)

component_assumptions <- sw_component_assumptions(
  baseline_prob = 0.15,
  treatment_or_a = 1.35,
  treatment_or_b = 1.25,
  interaction_or = 1.10,
  delay_a = 1,
  delay_b = 1,
  carryover_periods_a = 2,
  carryover_weights_a = c(0.50, 0.25),
  restart_rule_a = "reset",
  interaction_mode = "effective_overlap",
  icc = 0.05,
  period_effects = log(seq(1.00, 1.08, length.out = 8)),
  n_per_cluster_period = 25
)

audit_component_design(component_design, component_assumptions)

one_trial <- simulate_component_swcrt(
  component_design, component_assumptions, seed = 123
)
fit <- fit_component_model(one_trial, multiplicity = "holm")
fit$contrasts

# Use at least several thousand simulations for a manuscript analysis.
# component_power <- power_component_swcrt(
#   component_design, component_assumptions,
#   nsim = 5000, multiplicity = "holm", n_cores = 4, seed = 2026
# )
# summary(component_power)
# component_power$fit_diagnostics
# subset(
#   component_power$replicate_diagnostics,
#   fit_status != "successful_fit" | singular_fit
# )
```

`fit_diagnostics` summarizes the primary mutually exclusive analysis outcomes:
hard GLMM error, nonconverged fit, converged fit with a non-finite requested
contrast, and successful fit. `singular_fit` is reported as a separate,
non-exclusive property because a singular model may still converge and return
finite contrasts.

The standard contrasts are A versus Control, B versus Control, B versus A, A+B versus
Control, A+B versus A, A+B versus B, and the A-by-B interaction. When a
component is withdrawn, carryover weights are applied to its log-odds effect.
The interaction can either follow overlap of the effective component weights
or require both components to remain currently assigned.

The installed demonstration is available at:

```r
system.file("examples", "demo_component_four_state.R", package = "stepwedgepower")
```

<<<<<<< HEAD
## Classic SWD and batched stepped-wedge designs

A batched stepped-wedge design (BSWD) allows clusters to begin trial
participation in batches with staggered starts. A dash denotes a structurally
unobserved clinic-period and is never recoded as Control. Version 0.4.1 stores
calendar period and relative time since batch initiation; batch labels may be
supplied directly or inferred from first observed periods.

```r
swd <- sw_batched_design(
=======
## Structurally incomplete SWD and block SWD schedules

A dash denotes a cluster-period with no outcome data; it is not recoded as
Control. The latent schedule is retained for treatment history, while simulation
and analysis use only observed cells.

```r
swd <- sw_incomplete_component_design(
>>>>>>> origin/main
  clusters_per_sequence = c(5, 5),
  state = rbind(
    `Group 1` = c("0", "1", "1+2", "1+2"),
    `Group 2` = c("0", "0", "1", "1+2")
<<<<<<< HEAD
  ),
  batch = c("Batch 1", "Batch 1")
)

bswd <- sw_batched_design(
=======
  )
)

bswd <- sw_incomplete_component_design(
>>>>>>> origin/main
  clusters_per_sequence = c(2, 2, 3, 3),
  state = rbind(
    `Group 1` = c("0", "1", "1+2", "1+2", "-"),
    `Group 2` = c("0", "0", "1", "1+2", "-"),
    `Group 3` = c("-", "0", "1", "1+2", "1+2"),
    `Group 4` = c("-", "0", "0", "1", "1+2")
<<<<<<< HEAD
  ),
  batch = c("Batch 1", "Batch 1", "Batch 2", "Batch 2")
)

batched_assumptions <- sw_batched_assumptions(
=======
  )
)

incomplete_assumptions <- sw_component_assumptions(
>>>>>>> origin/main
  baseline_prob = 0.15,
  treatment_or_a = 1.35,
  treatment_or_b = 1.25,
  interaction_mode = "none",
  icc = 0.05,
<<<<<<< HEAD
  n_per_cluster_period = 25,
  time_model = "calendar",
  time_effects = log(seq(1.00, 1.08, length.out = 5))
)

component_resource_summary(swd, batched_assumptions)
component_resource_summary(bswd, batched_assumptions)
# Both designs have 40 observed clinic-periods and 1,000 observations.
```

The BSWD interface supports calendar time, shared time on trial, and separate
batch-specific time. It can compare designs under equal resources and can fit a
different time model from the one used to generate data.

```r
contrasts <- c("A_vs_control", "AB_vs_A", "AB_vs_control")

audit_batched_design(
  bswd, batched_assumptions,
  time_model = "separate",
  contrasts = contrasts,
  include_interaction = FALSE
)

# comparison <- compare_swd_bswd(
#   swd, bswd, batched_assumptions,
#   nsim = 5000, contrasts = contrasts,
#   include_interaction = FALSE, multiplicity = "holm",
#   n_cores = 4, seed = 2026
=======
  n_per_cluster_period = 25
)

component_resource_summary(swd, incomplete_assumptions)
component_resource_summary(bswd, incomplete_assumptions)
# Both designs have 40 observed clinic-periods and 1,000 observations.

# Use several thousand simulations for a final analysis.
# comparison <- compare_component_designs(
#   list(SWD = swd, BSWD = bswd),
#   incomplete_assumptions,
#   nsim = 5000,
#   contrasts = c("A_vs_control", "AB_vs_A", "AB_vs_control"),
#   include_interaction = FALSE,
#   multiplicity = "holm",
#   n_cores = 4,
#   seed = 2026
>>>>>>> origin/main
# )
```

The installed demonstration is available at:

```r
<<<<<<< HEAD
system.file("examples", "demo_batched_SWD_BSWD.R", package = "stepwedgepower")
=======
system.file("examples", "demo_exact_SWD_BSWD.R", package = "stepwedgepower")
>>>>>>> origin/main
```

## Asynchronous Control to A to A+B design

The cumulative extension treats B as an add-on intervention: B is never
observed without A, so its coefficient is the incremental effect of A+B versus
A.

```r
three_state <- sw_multistage_design(
  clusters_per_sequence = rep(6, 8),
  a_start = c(2, 3, 4, 5, 6, 7, 8, 9),
  b_start = c(6, 8, 7, 10, 9, 11, 12, 12),
  n_periods = 12,
  sequence_names = paste0("S", 1:8)
)

multistage_assumptions <- sw_multistage_assumptions(
  baseline_prob = 0.15,
  treatment_or_a = 1.40,
  incremental_or_b = 1.30,
  delay_a = 1,
  delay_b = 1,
  icc = 0.05,
  n_per_cluster_period = 30
)

audit_multistage_design(
  three_state,
  delay_a = multistage_assumptions$delay_a,
  delay_b = multistage_assumptions$delay_b
)

# power_ab <- power_multistage_swcrt(
#   three_state, multistage_assumptions,
#   nsim = 5000, multiplicity = "holm", n_cores = 4, seed = 2026
# )
# summary(power_ab)
```

To compare the same A rollout with and without B:

```r
two_state <- sw_multistage_design(
  clusters_per_sequence = three_state$clusters_per_sequence,
  a_start = three_state$a_start,
  b_start = rep(Inf, three_state$n_sequences),
  n_periods = three_state$n_periods,
  sequence_names = three_state$sequence_names
)

# comparison <- compare_multistage_designs(
#   two_state, three_state, multistage_assumptions,
#   nsim = 5000, multiplicity = "holm", n_cores = 4, seed = 2026
# )
# comparison
```

The installed demonstration is available at:

```r
system.file("examples", "demo_async_A_AB.R", package = "stepwedgepower")
```

## Vignettes

```r
vignette("stepped-wedge-design", package = "stepwedgepower")
vignette("component-four-state", package = "stepwedgepower")
vignette("incomplete-block-designs", package = "stepwedgepower")
vignette("asynchronous-a-ab", package = "stepwedgepower")
vignette("lpa-case-study", package = "stepwedgepower")
```

The Lp(a) functions are retained as application wrappers over the package's
generic clustered-data tools; the core simulation and power interfaces are
application-neutral.
