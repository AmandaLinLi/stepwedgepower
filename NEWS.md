# stepwedgepower 0.3.0

## Asynchronous cumulative interventions
* Added `sw_multistage_design()` for schedules in which sequences transition
  from Control to A and later to A+B at different calendar periods. Start
  periods may be supplied directly or through an explicit 0/1/2 state matrix.
* Added `sw_multistage_assumptions()` and `simulate_multistage_swcrt()` with
  separate wash-in delays for A and B, an A-versus-Control effect, an
  incremental B effect, and the implied total A+B effect.
* Added `audit_multistage_design()` to identify weak or aliased schedules,
  including a common B calendar start, a constant A-to-B lag, absent concurrent
  Control/A or A/A+B comparisons, and rank-deficient fixed-effect models.
* Added `fit_multistage_model()`, `power_multistage_swcrt()`, and
  `type1_multistage_swcrt()`. Results include raw and multiplicity-adjusted
  component tests, the total A+B contrast, joint-success probability, Monte
  Carlo intervals, estimation diagnostics, convergence and singularity rates,
  conditional power, and failure-aware power that counts unusable fits as
  unsuccessful trials.
* Added `compare_multistage_designs()` for a like-for-like comparison of a
  Control-to-A design with a Control-to-A-to-A+B design.
* Added `vignette("asynchronous-a-ab")` and an installed demonstration script.

## Sensitivity analysis and parallel execution
* `power_grid()` runs `power_swcrt()` across a Cartesian grid of assumptions --
  effect size, ICC, cluster-period size, and the number of clusters per
  sequence -- and returns an `sw_power_grid` object with `print()`, `summary()`,
  and `plot()` methods. The plot draws power curves with exact Monte Carlo
  intervals.
* `power_swcrt()` and `power_grid()` gain an `n_cores` argument. Each replicate
  is assigned its own L'Ecuyer-CMRG random-number stream, so a run is
  reproducible under a given `seed` **regardless of the number of cores**.

## Validation
* `validate_engine()` re-runs a power calculation with an independently written
  reference implementation that shares no code with the main engine, and reports
  whether the two agree within Monte Carlo error.
* `compare_with_swdpwr()` lines the simulation up against the closed-form
  calculation in the `swdpwr` package (`Suggests`). This is offered for
  information, not as a validation target: closed-form stepped-wedge power is
  asymptotic in the number of clusters and can be markedly optimistic at the
  small cluster counts these trials typically use. Where the two disagree,
  prefer the simulated value.

## Change to the default analysis model
* `fit_stepwedge_model()` now defaults to `adjust_sequence = FALSE`, giving the
  standard Hussey-Hughes model: treatment, a categorical period effect, and a
  cluster random intercept.

  In a stepped-wedge design a cluster belongs to exactly one sequence for the
  whole trial, so sequence is a cluster-level attribute already absorbed by the
  cluster random intercept. Including `factor(sequence)` as a fixed effect is
  close to redundant: in testing it lowered power (0.32 vs 0.44 in one design)
  and roughly doubled the rate of singular fits. Baseline differences between
  sequences still enter the simulation through `baseline_prob`, where they
  belong.

  This changes the default output of `fit_stepwedge_model()` and
  `analyze_swcrt()`. Pass `adjust_sequence = TRUE` to recover the previous
  model. The legacy `run_stepwedge_analysis()` is unchanged and still fits the
  sequence term, so version 0.1.x code keeps its original behaviour.

## Generalized applied helpers
* Added `prepare_cluster_data()`, `summarize_by_group()`,
  `fit_grouped_rate_model()`, `estimate_group_rates()`, and
  `analyze_aggregated_outcomes()`: the application-neutral forms of the
  physician/specialty helpers. Clusters and groups may be anything.
* `prepare_physician_data()`, `summarize_by_specialty()`,
  `fit_specialty_rate_model()`, `estimate_specialty_rates()`, and
  `analyze_lpa_outcomes()` are retained as thin application wrappers over the
  generic functions. They are no longer the package's central interface.

## Vignettes
* `vignette("stepped-wedge-design")` -- designing and powering a stepped-wedge
  trial in general terms.
* `vignette("lpa-case-study")` -- the original Lp(a) physician testing study, as
  one application of the generic engine.

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
  random-intercept model by variable name or a user-supplied `formula`.

## Power result object
* `power_swcrt()` returns an `sw_power` object carrying `power`, `mcse`,
  `conf.int`, `failure_rate`, per-simulation `estimated_effects` /
  `estimated_ses`, `convergence_status`, `singular_fit`, `coverage`, and
  `bias`, with `print()`, `summary()`, and `plot()` methods.
* The Monte Carlo interval for power is exact (Clopper-Pearson) rather than
  Wald. A Wald interval collapses to zero width when the estimate reaches 0 or
  1 -- exactly the high-power regime a power analysis targets.

## Backward compatibility
* `simulate_stepwedge_trial()`, `run_stepwedge_analysis()`, `estimate_power()`,
  and `estimate_type1_error()` retain their 0.1.x interfaces and delegate to the
  new engine. Legacy simulated columns are still produced.

# stepwedgepower 0.1.1

* Renamed the simulation and power interface from provider/specialty
  terminology to cluster/sequence terminology; legacy names remain accepted with
  deprecation warnings.
* Simulated data carry application-neutral columns alongside the original ones.
* Added `icc_to_cluster_sd()` and `cluster_sd_to_icc()`, and an `icc` argument.
* Added Monte Carlo standard errors and confidence intervals for power.
