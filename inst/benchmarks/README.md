# Benchmark and validation module

This directory contains reproduction scripts for four numerical artifacts in
Tuo Lin et al. (2026), *Power and Sample Size Implications of Delayed Cluster
Initiation in Stepped Wedge Trials*:

1. Table 2: software comparison;
2. Figure 3: calendar-time power across delayed batch starts;
3. Table 3: separate-time GEE power grid;
4. Figure 5: limited-cluster scaled-variance curves.

The benchmark code is intentionally stored under `inst/benchmarks/`, rather
than `R/`. It does not add exported functions or alter the package's public API.

## Files

- `benchmark_helpers.R`: local numerical, validation, and output helpers.
- `table2_software_comparison.R`: reruns `swCRTdesign` and `swdpwr` calls.
- `figure3_calendar_delay.R`: independent matrix-inversion reproduction.
- `table3_separate_time_grid.R`: reruns the `swdpwr` marginal GEE grid.
- `figure5_limited_clusters.R`: evaluates the closed-form variance curves and
  independently verifies them by WLS matrix inversion.
- `run_all.R`: executes all four benchmarks and writes machine-readable output.
- `reference/`: manuscript values, independent numerical references, a
  provenance manifest, reference-environment metadata, and documented source
  inconsistencies.

The implementations were written independently from the formulas, numerical
settings, and public calls reported by the manuscript and its companion GitHub
repository. The original external scripts are not bundled.

## Full run

From an installed package:

```r
benchmark_dir <- system.file("benchmarks", package = "stepwedgepower")
Sys.setenv(
  STEPWEDGEPOWER_BENCHMARK_OUTPUT =
    file.path(getwd(), "stepwedgepower-benchmark-results"),
  STEPWEDGEPOWER_RUN_EXTERNAL = "true",
  STEPWEDGEPOWER_BENCHMARK_STRICT = "false"
)
source(file.path(benchmark_dir, "run_all.R"))
```

From a source checkout:

```sh
Rscript inst/benchmarks/run_all.R
```

`swCRTdesign` and `swdpwr` are optional. When they are unavailable, Table 2 and
Table 3 retain the manuscript reference values and are marked as skipped.
Figure 3 and Figure 5 use base R only and always run.

## Strict mode

Set:

```r
Sys.setenv(STEPWEDGEPOWER_BENCHMARK_STRICT = "true")
```

to stop when a reproduced value falls outside its prespecified tolerance.
Package versions and `sessionInfo()` are saved because numerical results from
external software may change across versions.

## Interpretation

This module distinguishes three types of evidence:

- manuscript reference values;
- independently reproduced analytical values;
- results returned by optional external R packages.

The main `stepwedgepower` engine is simulation-based and fits a binomial GLMM.
The manuscript's Table 2 and Table 3 values include linear approximations and
marginal GEE calculations. They are therefore external benchmarks and
provenance checks, not a claim that GLMM and GEE power must be numerically
identical.

## Source notes

The benchmark records several source-level discrepancies rather than silently
resolving them. In particular, the prose for Figure 3 states a treatment
prevalence of 0.3, whereas the public code and plotted values use 0.2. The
reproduction uses 0.2 and records this decision in
`reference/source_discrepancies.csv`.
