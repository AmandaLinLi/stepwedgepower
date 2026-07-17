# stepwedgepower


`stepwedgepower` provides simulation-based design evaluation for stepped-wedge
cluster randomized trials with aggregated binary outcomes. It connects the
rollout schedule, data-generating assumptions, planned mixed model, and
empirical operating characteristics in one reproducible workflow.

Version 0.3.0 supports:

- arbitrary binary stepped-wedge crossover schedules;
- unequal clusters per sequence and flexible cluster-period sizes;
- secular trends and latent-scale ICC input;
- empirical power, type I error, bias, coverage, convergence, singularity, and
  Monte Carlo uncertainty;
- sensitivity grids and reproducible parallel simulation;
- asynchronous cumulative schedules in which clusters move from Control to A
  and later add B at different times;
- separate A, incremental-B, and total A+B contrasts, multiplicity adjustment,
  schedule auditing, conditional power, and failure-aware power.

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
vignette("asynchronous-a-ab", package = "stepwedgepower")
vignette("lpa-case-study", package = "stepwedgepower")
```

The Lp(a) functions are retained as application wrappers over the package's
generic clustered-data tools; the core simulation and power interfaces are
application-neutral.
