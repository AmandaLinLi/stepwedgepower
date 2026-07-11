# stepwedgepower 0.1.1

## Generalized interface
* Generalized the simulation and power interface from provider/specialty
  terminology to cluster/sequence terminology.
* Simulated data now carry generic columns (`cluster_id`, `sequence`, `period`,
  `intervention`, `n`, `events`) alongside the legacy 0.1.0 columns, and
  `run_stepwedge_analysis()` accepts either set.
* Added `icc_to_cluster_sd()` and `cluster_sd_to_icc()`, and an `icc` argument
  as an alternative parameterisation of `cluster_sd`.

## Monte Carlo uncertainty and diagnostics
* `estimate_power()` and `estimate_type1_error()` return a `stepwedge_power`
  object reporting the estimate, an **exact (Clopper-Pearson) Monte Carlo
  interval**, the Monte Carlo standard error, fit-failure rate, and
  singular-fit rate. The exact interval retains nominal coverage at the
  boundary, where a Wald interval collapses to zero width.
* Added a `print.stepwedge_power()` method.

## Deprecation handling
* Legacy argument names are retained and mapped to their canonical
  counterparts. Deprecation warnings are now resolved once per call rather than
  once per simulated replicate, so a long run emits a single warning instead of
  one per iteration.
* Supplying both a canonical argument and its deprecated alias (or both
  `cluster_sd` and `icc`) is now an error rather than being resolved silently.
* `estimate_type1_error()` has an explicit argument list rather than `...`.

## Other
* `run_stepwedge_analysis()` builds its model formula from column names instead
  of `transform()`, avoiding "no visible binding for global variable" notes.
* Updated the package title in response to CRAN feedback.
