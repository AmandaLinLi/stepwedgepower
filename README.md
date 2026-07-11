# stepwedgepower

`stepwedgepower` provides a general simulation-based workflow for stepped-wedge cluster randomized trials with aggregated binary outcomes. Version 0.1.1 adds:

- generic cluster/sequence terminology,
- direct ICC specification for logistic random-intercept models,
- Monte Carlo standard errors and exact confidence intervals,
- convergence, fit-failure, and singular-fit diagnostics, and
- backward compatibility with the original physician/specialty interface.

## Project background

This package is based on a PhD biostatistics rotation project on **statistical methods for stepped wedge clinical trial designs**. According to the rotation evaluation, the project involved:

- research of published literature,
- summarizing literature in slides/presentations, and
- building **well-organized, well-documented R software**.

The software was also used to provide sample size calculations for a study under development. In the permission email shown in the screenshots, Prof. Florin Vaida approved publishing the software to GitHub.

## What was changed from the original script

The original file mixed together:

1. raw CSV imports,
2. one-off data cleaning,
3. model fitting,
4. ad hoc probability extraction, and
5. repeated simulation loops.

This package reorganizes those steps into exported functions:

- `prepare_physician_data()`
- `summarize_by_specialty()`
- `fit_specialty_rate_model()`
- `estimate_specialty_rates()`
- `analyze_lpa_outcomes()`
- `simulate_stepwedge_trial()`
- `run_stepwedge_analysis()`
- `estimate_power()`
- `estimate_type1_error()`

## Installation

```r
# install.packages("remotes")
remotes::install_github("AmandaLinLi/stepwedgepower")
```

## Minimal workflow

```r
library(stepwedgepower)

power_out <- estimate_power(
  n_simulations = 1000,
  treatment_or = 1.50,
  n_clusters_per_sequence = c(10, 10, 10, 10),
  sequence_names = paste0("Sequence ", 1:4),
  baseline_probs = c(0.05, 0.05, 0.05, 0.05),
  icc = 0.05,
  n_per_cluster_period = 20,
  seed = 2026
)

power_out
power_out$power
power_out$mcse
c(power_out$conf_low, power_out$conf_high)
```

Version 0.1.0 argument names remain available with deprecation warnings.

## Example data

A small synthetic dataset is included for quick testing:

```r
ex_dat <- read_example_physician_data()
head(ex_dat)
```

## Repository structure

```text
stepwedgepower/
  DESCRIPTION
  NAMESPACE
  R/
  man/
  inst/extdata/
  inst/scripts/
  tests/
  .github/workflows/
```

## Notes

- The package is structured to be **GitHub-ready**.
- I assumed an **MIT license** for convenience; you can change that before publishing.
- The original external CSV files are not bundled here, so the package ships with synthetic example data only.
- Because the current environment does not have an R runtime, this package scaffold was prepared carefully but not executed with `R CMD check` inside the container.

## Suggested next steps before publishing

1. Open the package locally in RStudio.
2. Run `devtools::document()`.
3. Run `devtools::check()`.
4. Replace `YOUR_GITHUB_USERNAME` in `README.md`.
5. Replace the maintainer email placeholder in `DESCRIPTION`.
6. Commit and push to GitHub.
