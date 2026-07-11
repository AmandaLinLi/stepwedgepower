# Build an sw_design + sw_assumptions for the classic sequential-crossover
# design used by the legacy interface, then simulate. Internal helper.
.legacy_design_assumptions <- function(
  treatment_or, n_clusters_per_sequence, sequence_names,
  baseline_probs, cluster_sd, n_per_cluster_period, n_steps
) {
  n_sequences <- length(n_clusters_per_sequence)
  if (is.null(n_steps)) n_steps <- n_sequences + 1L
  crossover_period <- seq_len(n_sequences) + 1L
  crossover_period[crossover_period > n_steps] <- Inf

  design <- sw_design(
    clusters_per_sequence = n_clusters_per_sequence,
    crossover_period = crossover_period,
    n_periods = n_steps,
    sequence_names = sequence_names
  )
  assumptions <- sw_assumptions(
    baseline_prob = baseline_probs,
    treatment_or = treatment_or,
    cluster_sd = cluster_sd,
    n_per_cluster_period = n_per_cluster_period
  )
  list(design = design, assumptions = assumptions)
}

#' Simulate one stepped-wedge trial dataset (legacy interface)
#'
#' `r lifecycle::badge("superseded")` Superseded by [simulate_swcrt()] with
#' [sw_design()] and [sw_assumptions()], which support arbitrary crossover
#' schedules, secular trends, and flexible sample sizes. This function is
#' retained for backward compatibility: it reproduces the classic
#' sequential-crossover design and returns both the application-neutral columns
#' (`cluster_id`, `sequence`, `sequence_idx`, `period`, `intervention`, `n`,
#' `events`) and the original 0.1.0 columns (`PID`, `specialty`,
#' `specialty_idx`, `step`, `treat`, `n_patients`, `n_positive`).
#'
#' @inheritParams estimate_power
#' @param seed Optional random seed.
#'
#' @return A data frame with one row per cluster-period.
#' @export
simulate_stepwedge_trial <- function(
  treatment_or = NULL,
  n_clusters_per_sequence = NULL,
  sequence_names = NULL,
  baseline_probs = NULL,
  cluster_sd = NULL,
  icc = NULL,
  n_per_cluster_period = NULL,
  n_steps = NULL,
  seed = NULL,
  effect_size_or = NULL,
  n_providers_per_specialty = NULL,
  specialty_names = NULL,
  base_probs = NULL,
  tau_provider = NULL,
  pts_per_step = NULL
) {
  treatment_or <- .deprecate_alias(
    treatment_or, effect_size_or,
    "simulate_stepwedge_trial(effect_size_or)",
    "simulate_stepwedge_trial(treatment_or)")
  n_clusters_per_sequence <- .deprecate_alias(
    n_clusters_per_sequence, n_providers_per_specialty,
    "simulate_stepwedge_trial(n_providers_per_specialty)",
    "simulate_stepwedge_trial(n_clusters_per_sequence)")
  sequence_names <- .deprecate_alias(
    sequence_names, specialty_names,
    "simulate_stepwedge_trial(specialty_names)",
    "simulate_stepwedge_trial(sequence_names)")
  baseline_probs <- .deprecate_alias(
    baseline_probs, base_probs,
    "simulate_stepwedge_trial(base_probs)",
    "simulate_stepwedge_trial(baseline_probs)")
  cluster_sd <- .deprecate_alias(
    cluster_sd, tau_provider,
    "simulate_stepwedge_trial(tau_provider)",
    "simulate_stepwedge_trial(cluster_sd)")
  n_per_cluster_period <- .deprecate_alias(
    n_per_cluster_period, pts_per_step,
    "simulate_stepwedge_trial(pts_per_step)",
    "simulate_stepwedge_trial(n_per_cluster_period)")

  if (is.null(treatment_or)) treatment_or <- 1.5
  if (is.null(n_clusters_per_sequence)) n_clusters_per_sequence <- c(40, 40, 40, 40)
  if (is.null(sequence_names)) sequence_names <- c("Cardiol", "IntMed", "FamMed", "Neurol")
  if (is.null(baseline_probs)) baseline_probs <- c(0.06, 0.04, 0.03, 0.02)
  if (is.null(n_per_cluster_period)) n_per_cluster_period <- 20
  cluster_sd <- .resolve_cluster_sd(icc = icc, cluster_sd = cluster_sd, default = 1.21)

  da <- .legacy_design_assumptions(
    treatment_or, n_clusters_per_sequence, sequence_names,
    baseline_probs, cluster_sd, n_per_cluster_period, n_steps)

  sim <- simulate_swcrt(da$design, da$assumptions, seed = seed)

  # Legacy columns for backward compatibility.
  sim$PID <- sim$cluster_id
  sim$specialty_idx <- sim$sequence_idx
  sim$specialty <- sim$sequence
  sim$step <- sim$period
  sim$treat <- sim$intervention
  sim$n_patients <- sim$n
  sim$n_positive <- sim$events
  sim
}

#' Fit the stepped-wedge analysis model to a simulated dataset (legacy interface)
#'
#' `r lifecycle::badge("superseded")` Superseded by [fit_stepwedge_model()].
#' Accepts either the application-neutral columns or the legacy 0.1.0 columns.
#'
#' @param sim_data A data frame from [simulate_stepwedge_trial()] or a compatible
#'   aggregated cluster-period dataset.
#' @param fit_link Link function used in the fitted model.
#' @param nAGQ Number of quadrature points for [lme4::glmer()].
#'
#' @return A list with fitted model, coefficient table, and treatment p-value.
#' @export
run_stepwedge_analysis <- function(
  sim_data, fit_link = c("logit", "identity"), nAGQ = 1
) {
  fit_link <- match.arg(fit_link)
  cols <- names(sim_data)
  pick <- function(generic, legacy) if (generic %in% cols) generic else legacy
  res <- fit_stepwedge_model(
    sim_data,
    outcome = c(pick("events", "n_positive"), pick("n", "n_patients")),
    treatment = pick("intervention", "treat"),
    period = pick("period", "step"),
    cluster = pick("cluster_id", "PID"),
    sequence = pick("sequence_idx", "specialty_idx"),
    adjust_sequence = TRUE, period_effect = "categorical",
    link = fit_link, nAGQ = nAGQ
  )
  list(fit = res$fit, coefficients = res$coefficients, p_value = res$p_value)
}

#' Estimate power by repeated stepped-wedge simulation (legacy interface)
#'
#' `r lifecycle::badge("superseded")` Superseded by [power_swcrt()], which
#' returns a richer [sw_power] object. This wrapper preserves the original
#' argument names and return shape while delegating to the new engine, and
#' reports the Monte Carlo standard error and a confidence interval for power.
#'
#' @param n_simulations Number of simulations.
#' @param alpha Significance threshold.
#' @param treatment_or Odds ratio under the data-generating model.
#' @param n_clusters_per_sequence Integer vector of clusters per sequence.
#' @param sequence_names Labels for the sequences.
#' @param cluster_sd Standard deviation of the cluster random intercept.
#' @param icc Optional intraclass correlation (alternative to `cluster_sd`).
#' @param baseline_probs Baseline probabilities by sequence.
#' @param n_per_cluster_period Units per cluster per period.
#' @param n_steps Number of periods.
#' @param fit_link Link used when fitting the analysis model.
#' @param seed Optional seed.
#' @param nAGQ Number of quadrature points.
#' @param effect_size_or,n_providers_per_specialty,specialty_names,base_probs,tau_provider,pts_per_step
#'   Deprecated legacy aliases.
#'
#' @return A list with `power`, `mcse`, `conf.int`, `alpha`, `p_values`,
#'   `n_successful`, `n_failed`, and the underlying [sw_power] object in
#'   `sw_power`.
#' @export
estimate_power <- function(
  n_simulations = 100, alpha = 0.05,
  treatment_or = NULL, n_clusters_per_sequence = NULL, sequence_names = NULL,
  cluster_sd = NULL, icc = NULL, baseline_probs = NULL,
  n_per_cluster_period = NULL, n_steps = NULL,
  fit_link = c("logit", "identity"), seed = NULL, nAGQ = 1,
  effect_size_or = NULL, n_providers_per_specialty = NULL, specialty_names = NULL,
  base_probs = NULL, tau_provider = NULL, pts_per_step = NULL
) {
  fit_link <- match.arg(fit_link)

  treatment_or <- .deprecate_alias(treatment_or, effect_size_or,
    "estimate_power(effect_size_or)", "estimate_power(treatment_or)")
  n_clusters_per_sequence <- .deprecate_alias(n_clusters_per_sequence, n_providers_per_specialty,
    "estimate_power(n_providers_per_specialty)", "estimate_power(n_clusters_per_sequence)")
  sequence_names <- .deprecate_alias(sequence_names, specialty_names,
    "estimate_power(specialty_names)", "estimate_power(sequence_names)")
  baseline_probs <- .deprecate_alias(baseline_probs, base_probs,
    "estimate_power(base_probs)", "estimate_power(baseline_probs)")
  cluster_sd <- .deprecate_alias(cluster_sd, tau_provider,
    "estimate_power(tau_provider)", "estimate_power(cluster_sd)")
  n_per_cluster_period <- .deprecate_alias(n_per_cluster_period, pts_per_step,
    "estimate_power(pts_per_step)", "estimate_power(n_per_cluster_period)")

  if (is.null(treatment_or)) treatment_or <- 2
  if (is.null(n_clusters_per_sequence)) n_clusters_per_sequence <- c(40, 40, 40, 40) * 0.25
  if (is.null(sequence_names)) sequence_names <- c("Cardiol", "IntMed", "FamMed", "Neurol")
  if (is.null(baseline_probs)) baseline_probs <- c(0.07, 0.04, 0.03, 0.02)
  if (is.null(n_per_cluster_period)) n_per_cluster_period <- 100 / 5
  cluster_sd <- .resolve_cluster_sd(icc = icc, cluster_sd = cluster_sd, default = 1.21)

  da <- .legacy_design_assumptions(
    treatment_or, n_clusters_per_sequence, sequence_names,
    baseline_probs, cluster_sd, n_per_cluster_period, n_steps)

  pw <- power_swcrt(da$design, da$assumptions, nsim = n_simulations,
                    alpha = alpha, fit_link = fit_link, nAGQ = nAGQ, seed = seed)

  list(
    call = match.call(),
    power = pw$power, mcse = pw$mcse, conf.int = pw$conf.int,
    alpha = alpha, p_values = pw$p_values,
    n_successful = pw$successful, n_failed = pw$failed,
    sw_power = pw
  )
}

#' Estimate type I error by repeated stepped-wedge simulation
#'
#' A convenience wrapper around [estimate_power()] that sets the treatment odds
#' ratio to 1.
#'
#' @inheritParams estimate_power
#' @return A list like [estimate_power()], with `type1_error` added.
#' @export
estimate_type1_error <- function(
  n_simulations = 100, alpha = 0.05,
  n_clusters_per_sequence = NULL, sequence_names = NULL,
  cluster_sd = NULL, icc = NULL, baseline_probs = NULL,
  n_per_cluster_period = NULL, n_steps = NULL,
  fit_link = c("logit", "identity"), seed = NULL, nAGQ = 1,
  n_providers_per_specialty = NULL, specialty_names = NULL,
  base_probs = NULL, tau_provider = NULL, pts_per_step = NULL
) {
  fit_link <- match.arg(fit_link)
  out <- estimate_power(
    n_simulations = n_simulations, alpha = alpha, treatment_or = 1,
    n_clusters_per_sequence = n_clusters_per_sequence, sequence_names = sequence_names,
    cluster_sd = cluster_sd, icc = icc, baseline_probs = baseline_probs,
    n_per_cluster_period = n_per_cluster_period, n_steps = n_steps,
    fit_link = fit_link, seed = seed, nAGQ = nAGQ,
    n_providers_per_specialty = n_providers_per_specialty, specialty_names = specialty_names,
    base_probs = base_probs, tau_provider = tau_provider, pts_per_step = pts_per_step)
  out$type1_error <- out$power
  out
}
