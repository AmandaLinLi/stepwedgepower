# stepwedgepower 0.2.0

## New structured interface
* `sw_design()` constructs a stepped-wedge design from either per-sequence
  crossover periods or an explicit 0/1 treatment matrix. Supports multiple
  baseline periods, unequal clusters per sequence, and custom, incomplete, or
  simultaneously crossing schedules.
* `sw_assumptions()` collects the data-generating assumptions (baseline risk,
  treatment effect, ICC / cluster SD, secular period trend, cluster-period
  sample size). Baseline and sample size may be scalar, per-sequence, or per
  sequence-period; sample size may also be a function.
* `simulate_swcrt(design, assumptions)` is the generic simulation engine and
  returns application-neutral columns: `cluster_id`, `sequence`,
  `sequence_idx`, `period`, `intervention`, `n`, `events`.
* `fit_stepwedge_model()` / `analyze_swcrt()` fit a configurable binomial
  random-intercept model by variable name or a user-supplied `formula`, with no
  dependence on application-specific column names.

## Power result object
* `power_swcrt()` returns an `sw_power` object carrying `power`, `mcse`,
  `conf.int`, `failure_rate`, per-simulation `estimated_effects` /
  `estimated_ses`, `convergence_status`, `singular_fit`, `coverage`, and
  `bias`, with `print()`, `summary()`, and `plot()` methods.
* The Monte Carlo interval for power is **exact (Clopper-Pearson)** rather than
  Wald. A Wald interval collapses to zero width when the estimate reaches 0 or
  1 -- exactly the high-power regime a power analysis targets -- and materially
  under-covers as power approaches 1. `estimate_power()` and
  `estimate_type1_error()` inherit this via the shared engine.

## Secular trends and unequal sizes
* Explicit `period_effects` (log-odds secular trend) are simulated.
* Cluster-period sample sizes may be unequal (scalar, matrix, or function).

## Backward compatibility
* `simulate_stepwedge_trial()`, `run_stepwedge_analysis()`, `estimate_power()`,
  and `estimate_type1_error()` retain their 0.1.x interfaces (now superseded)
  and delegate to the new engine. Legacy simulated columns (`PID`, `step`,
  `specialty_idx`, `treat`, `n_patients`, `n_positive`) are still produced.

# stepwedgepower 0.1.1

* Renamed simulation and power arguments to application-neutral names
  (`treatment_or`, `n_clusters_per_sequence`, `sequence_names`,
  `baseline_probs`, `cluster_sd`, `n_per_cluster_period`); legacy names remain
  accepted with deprecation warnings.
* Simulated data now include application-neutral columns (`cluster_id`,
  `sequence`, `period`, `intervention`, `n`, `events`) alongside the original
  columns.
* Added `icc_to_cluster_sd()` and `cluster_sd_to_icc()`, and an `icc` argument
  as an alternative to `cluster_sd` (disagreeing values error).
* `estimate_power()` now reports the Monte Carlo standard error (`mcse`) and a
  confidence interval (`conf.int`) for the power estimate.
