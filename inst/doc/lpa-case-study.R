## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
set.seed(1)

## ----setup--------------------------------------------------------------------
library(stepwedgepower)

## -----------------------------------------------------------------------------
physicians <- read_example_physician_data()
str(physicians, max.level = 1)

## -----------------------------------------------------------------------------
analysis_set <- prepare_physician_data(
  physicians,
  min_patients = 100,
  max_patients = 10000
)

summarize_by_specialty(analysis_set, vars = "n_total_pat")

## -----------------------------------------------------------------------------
rate_model <- fit_specialty_rate_model(
  analysis_set,
  successes = "n_lpa_pat",
  trials = "n_total_pat",
  specialty_var = "specialty",
  provider_var = "prov_id",
  random_intercept = TRUE
)

baseline_rates <- estimate_specialty_rates(rate_model, specialty_var = "specialty")
baseline_rates[, c("specialty", "probability")]

## -----------------------------------------------------------------------------
tau2 <- as.numeric(lme4::VarCorr(rate_model)$prov_id[1, 1])
cluster_sd_to_icc(sqrt(tau2))

## -----------------------------------------------------------------------------
design <- sw_design(
  clusters_per_sequence = c(30, 30, 30, 30),
  crossover_period = c(2, 3, 4, 5),
  n_periods = 5,
  sequence_names = c("Cardiology", "Internal Medicine",
                     "Family Medicine", "Neurology")
)
design

## -----------------------------------------------------------------------------
assumptions <- sw_assumptions(
  baseline_prob = c(0.06, 0.04, 0.03, 0.02),
  treatment_or = 1.5,
  icc = 0.05,
  n_per_cluster_period = 20
)
assumptions

## ----eval = FALSE-------------------------------------------------------------
# power <- power_swcrt(design, assumptions, nsim = 1000, seed = 1, n_cores = 4)
# summary(power)

## ----eval = FALSE-------------------------------------------------------------
# grid <- power_grid(
#   design, assumptions,
#   vary = list(
#     treatment_or = c(1.3, 1.5, 1.75, 2.0),
#     icc = c(0.02, 0.05, 0.10)
#   ),
#   nsim = 500, n_cores = 4, seed = 1
# )
# summary(grid)
# plot(grid)

