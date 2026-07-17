## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")
set.seed(1)

## ----setup--------------------------------------------------------------------
library(stepwedgepower)

## -----------------------------------------------------------------------------
three_state <- sw_multistage_design(
  clusters_per_sequence = rep(6, 8),
  a_start = c(2, 3, 4, 5, 6, 7, 8, 9),
  b_start = c(6, 8, 7, 10, 9, 11, 12, 12),
  n_periods = 12,
  sequence_names = paste0("S", 1:8)
)
three_state

## -----------------------------------------------------------------------------
assumptions <- sw_multistage_assumptions(
  baseline_prob = 0.15,
  treatment_or_a = 1.40,
  incremental_or_b = 1.30,
  delay_a = 1,
  delay_b = 1,
  icc = 0.05,
  period_effects = log(seq(1.00, 1.12, length.out = 12)),
  n_per_cluster_period = 30
)
assumptions

## -----------------------------------------------------------------------------
audit_multistage_design(
  three_state,
  delay_a = assumptions$delay_a,
  delay_b = assumptions$delay_b
)

## -----------------------------------------------------------------------------
trial <- simulate_multistage_swcrt(three_state, assumptions, seed = 123)
head(trial)

## ----eval = FALSE-------------------------------------------------------------
# fit <- fit_multistage_model(trial, multiplicity = "holm")
# fit$contrasts

## ----eval = FALSE-------------------------------------------------------------
# power_three <- power_multistage_swcrt(
#   three_state,
#   assumptions,
#   nsim = 5000,
#   multiplicity = "holm",
#   n_cores = 4,
#   seed = 2026
# )
# summary(power_three)
# plot(power_three, type = "failure-aware")

## -----------------------------------------------------------------------------
two_state <- sw_multistage_design(
  clusters_per_sequence = three_state$clusters_per_sequence,
  a_start = three_state$a_start,
  b_start = rep(Inf, three_state$n_sequences),
  n_periods = three_state$n_periods,
  sequence_names = three_state$sequence_names
)

## ----eval = FALSE-------------------------------------------------------------
# comparison <- compare_multistage_designs(
#   design_two = two_state,
#   design_three = three_state,
#   assumptions_two = assumptions,
#   nsim = 5000,
#   multiplicity = "holm",
#   n_cores = 4,
#   seed = 2026
# )
# comparison

## ----eval = FALSE-------------------------------------------------------------
# type1 <- type1_multistage_swcrt(
#   three_state,
#   assumptions,
#   nsim = 5000,
#   multiplicity = "holm",
#   n_cores = 4,
#   seed = 22026
# )
# type1

