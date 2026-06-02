# stepwedgepower

`stepwedgepower` refactors a one-off academic R script into a reusable R package for:

- physician-level data cleaning,
- specialty-level binomial modeling,
- stepped-wedge simulation, and
- simulation-based power / type I error estimation.

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

dat0 <- read.csv("data_pseudo_homeDPT.csv")
dat <- prepare_physician_data(dat0)

summary_tbl <- summarize_by_specialty(
  dat,
  vars = c("n_total_pat", "n_ldl_pat")
)

results <- analyze_lpa_outcomes(dat)

results$overall$logit$glm_rates
results$overall$logit$glmer_rates

power_out <- estimate_power(
  n_simulations = 500,
  effect_size_or = 2.11,
  n_providers_per_specialty = c(40, 40, 40, 40) * 0.25,
  tau_provider = 1.21,
  base_probs = c(0.05, 0.05, 0.05, 0.05),
  pts_per_step = 50 / 5,
  seed = 2026
)

power_out$power
```

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
