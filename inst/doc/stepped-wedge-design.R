## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
set.seed(1)

## ----setup--------------------------------------------------------------------
library(stepwedgepower)

## -----------------------------------------------------------------------------
design <- sw_design(
  clusters_per_sequence = c(8, 8, 8, 8),
  crossover_period = c(2, 3, 4, 5),
  n_periods = 5
)
design

## -----------------------------------------------------------------------------
schedule <- matrix(
  c(0, 0, 1, 1, 1,
    0, 0, 0, 1, 1,
    0, 0, 0, 0, 1),
  nrow = 3, byrow = TRUE
)

custom <- sw_design(
  clusters_per_sequence = c(8, 10, 12),  # unequal
  treatment = schedule                   # two baseline periods
)
custom

## -----------------------------------------------------------------------------
assumptions <- sw_assumptions(
  baseline_prob = 0.05,     # control-arm risk
  treatment_or = 1.6,       # target effect
  icc = 0.05,               # cluster correlation
  period_effects = c(0, 0.05, 0.10, 0.15, 0.20),  # secular trend, log-odds
  n_per_cluster_period = 20
)
assumptions

## -----------------------------------------------------------------------------
icc_to_cluster_sd(0.05)
cluster_sd_to_icc(0.4161)

## -----------------------------------------------------------------------------
variable_size <- sw_assumptions(
  baseline_prob = 0.05, treatment_or = 1.6, icc = 0.05,
  n_per_cluster_period = function(n) rpois(n, lambda = 20) + 1
)
trial <- simulate_swcrt(design, variable_size, seed = 1)
range(trial$n)

## -----------------------------------------------------------------------------
trial <- simulate_swcrt(design, assumptions, seed = 123)
head(trial)

## -----------------------------------------------------------------------------
fit <- analyze_swcrt(trial)
fit$estimate    # log-odds
fit$p_value

## ----eval = FALSE-------------------------------------------------------------
# fit_stepwedge_model(
#   trial,
#   formula = cbind(events, n - events) ~ intervention + factor(period) +
#     (1 | cluster_id)
# )

## ----eval = FALSE-------------------------------------------------------------
# power <- power_swcrt(design, assumptions, nsim = 1000, seed = 1, n_cores = 4)
# summary(power)
# plot(power)

## ----eval = FALSE-------------------------------------------------------------
# grid <- power_grid(
#   design, assumptions,
#   vary = list(
#     treatment_or = c(1.3, 1.5, 1.75, 2.0),
#     icc = c(0.02, 0.05, 0.10),
#     clusters_per_sequence = list(c(6, 6, 6, 6), c(8, 8, 8, 8), c(10, 10, 10, 10))
#   ),
#   nsim = 500, n_cores = 4, seed = 1
# )
# summary(grid)
# plot(grid)

## ----eval = FALSE-------------------------------------------------------------
# validate_engine(design, assumptions, nsim = 500, seed = 1)

