# stepwedgepower 0.4.1

## Batched stepped-wedge terminology and design objects
* Replaced the provisional term "block stepped-wedge design" with the standard
  term **batched stepped-wedge design (BSWD)**. A BSWD is represented as a
  structurally incomplete stepped-wedge design whose batches begin trial
  participation at staggered calendar periods.
* Added `sw_batched_design()` and `as_batched_design()`. Batch-aware design
  objects retain the treatment schedule and observation mask while adding
  batch labels, batch initiation periods, delays from the first batch, gaps
  from the preceding batch, calendar period, and relative time since batch
  initiation (`time_on_trial`). Batch labels can be supplied or inferred from
  first observed periods.

## Three time-effect parameterizations
* Added `sw_batched_assumptions()` for data generation under a calendar-time,
  shared time-on-trial, or separate batch-specific time model.
* Added `simulate_batched_swcrt()`, `fit_batched_model()`, and
  `audit_batched_design()`. The separate time model uses a categorical
  batch-by-local-time factor and supports batches with unequal observed
  lengths; the calendar model shares secular effects by calendar period; and
  the time-on-trial model shares relative-time effects across batches.
* Added `power_batched_swcrt()` and `type1_batched_swcrt()` so the generating
  and fitted time models can be matched or intentionally differed in a
  reproducible simulation study.
* The batched functions default to a no-interaction component model, which is
  appropriate for cumulative Control/A/A+B schedules without a B-only state;
  four-state designs can request the A-by-B interaction explicitly.

## SWD/BSWD comparisons
* Added `compare_batched_designs()`, `compare_swd_bswd()`, and
  `compare_batched_time_models()` for equal-resource comparisons of classic
  SWD and BSWD schedules and for sensitivity analysis across the three time
  parameterizations.
* Added the adjacent component contrast `B_vs_A`, completing the standard
  comparisons needed for paths such as Control -> A -> B -> A+B.
* Renamed the incomplete-design vignette to
  `vignette("batched-stepped-wedge-designs")` and updated the installed SWD/BSWD
  demonstration to use the batch-aware interface.

## Compatibility
* All 0.3.0 cumulative-intervention functions and all 0.4.0 component-design
  functions remain available. The 0.4.1 batched interface is additive.

# stepwedgepower 0.4.0

## General component-based designs
* Added `sw_component_design()` for arbitrary asynchronous schedules containing
  Control, A, B, and A+B. Schedules may be supplied as a four-state matrix or
  as separate binary component-assignment matrices. Unlike the cumulative
  interface, component schedules may include withdrawal and reintroduction.
* Added `as_component_design()` to convert the version 0.3.0 cumulative
  Control/A/A+B design into the general representation.

<<<<<<< HEAD
## Structurally incomplete and batched stepped-wedge designs
=======
## Structurally incomplete and block stepped-wedge designs
>>>>>>> origin/main
* Added an `observed` mask to `sw_component_design()`. Cells marked `FALSE`
  retain their latent treatment history but contribute no outcome and no row
  to the fitted model.
* Added `sw_incomplete_component_design()` for schedules written with `-`,
  blank cells, or `NA`. The constructor accepts manuscript labels such as
  `0`, `1`, and `1+2`, distinguishes missing periods from Control, and infers
  latent leading or trailing states when they are unambiguous.
* Added `component_resource_summary()` to report calendar and observed
  sequence-periods, cluster-periods, and total individual observations.
* Updated simulation, design auditing, model fitting, power calculation, and
  cross-design comparison to use only observed cluster-periods. This supports
<<<<<<< HEAD
  batched stepped-wedge designs in which different sequence groups are observed
=======
  block stepped-wedge designs in which different sequence groups are observed
>>>>>>> origin/main
  over different calendar windows.

## Wash-in, withdrawal, and carryover
* Added `sw_component_assumptions()` with separate A and B main effects, an
  A-by-B interaction, component-specific wash-in periods, restart rules, and
  user-defined non-increasing carryover weights after withdrawal.
* If a component is restarted before residual carryover has ended, the
  simulation retains the larger of the residual carryover effect and the
  restarted current effect. Interaction carryover can follow overlap of the
  effective component weights or be restricted to current assignment.
* Added `simulate_component_swcrt()` with complete assignment and effect-history
  columns, including exposure episodes, withdrawals, restarts, periods since
  withdrawal, current effects, residual carryover, and true component
  contributions.

## Analysis and operating characteristics
* Added `fit_component_model()` with standard contrasts for A versus Control,
  B versus Control, A+B versus Control, A+B versus A, A+B versus B, and the
  A-by-B interaction. Linear-contrast standard errors use the full fitted
  covariance matrix.
* Added `audit_component_design()` to check state support, concurrent treatment
  comparisons, withdrawal and restart patterns, fixed-effect rank, and formal
  estimability of requested contrasts under the planned wash-in and carryover
  rules.
* Added `power_component_swcrt()` and `type1_component_swcrt()` with exact Monte
  Carlo intervals, multiplicity-adjusted tests, joint-success probability,
  conditional and failure-aware power, bias, standard-error calibration,
  coverage, convergence, singularity, and warning diagnostics.
* Added `compare_component_designs()` for equal-resource or unequal-resource
  comparisons of two or more candidate component schedules.
* Expanded simulation diagnostics to distinguish hard GLMM errors,
  nonconverged fits, converged fits with non-finite requested contrasts,
  singular fits, and successful fits. Power objects now contain aggregate
  `fit_diagnostics`, per-simulation `replicate_diagnostics`, retained error and
  convergence messages, and the same breakdown in every power-table row.
  Singularity is reported as a non-exclusive property and does not by itself
  make an otherwise evaluable fit unsuccessful.
* Added a four-state vignette and an installed demonstration script showing a
  Control -> A -> A+B -> B withdrawal path alongside Control -> B -> A+B paths.

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
