#' Convert a logistic-model ICC to a random-intercept standard deviation
#'
#' Uses the latent-variable approximation for a logistic random-intercept model,
#' where the level-1 variance is pi^2 / 3.
#'
#' @param icc Intraclass correlation coefficient in the open interval (0, 1).
#' @return The corresponding random-intercept standard deviation.
#' @examples
#' icc_to_cluster_sd(0.05)
#' @export
icc_to_cluster_sd <- function(icc) {
  if (!is.numeric(icc) || any(!is.finite(icc)) || any(icc <= 0 | icc >= 1)) {
    stop("icc must contain finite values strictly between 0 and 1.", call. = FALSE)
  }
  sqrt((icc * (pi^2 / 3)) / (1 - icc))
}

#' Convert a random-intercept standard deviation to a logistic-model ICC
#'
#' @param cluster_sd Nonnegative random-intercept standard deviation.
#' @return The corresponding latent-scale intraclass correlation coefficient.
#' @examples
#' cluster_sd_to_icc(0.416)
#' @export
cluster_sd_to_icc <- function(cluster_sd) {
  if (!is.numeric(cluster_sd) || any(!is.finite(cluster_sd)) || any(cluster_sd < 0)) {
    stop("cluster_sd must contain finite, nonnegative values.", call. = FALSE)
  }
  cluster_sd^2 / (cluster_sd^2 + pi^2 / 3)
}

# Resolve a canonical/deprecated argument pair.
#
# `new_supplied` is the result of `!missing(new_name)` in the caller, so we can
# tell "user explicitly passed the canonical argument" from "the default was
# used", without comparing against a hardcoded copy of the default.
.resolve_legacy_argument <- function(new_value, old_value, new_name, old_name,
                                     new_supplied = FALSE) {
  if (!is.null(old_value)) {
    if (isTRUE(new_supplied)) {
      stop(sprintf("Supply only one of '%s' and deprecated '%s'.",
                   new_name, old_name), call. = FALSE)
    }
    warning(sprintf("Argument '%s' is deprecated; use '%s' instead.",
                    old_name, new_name), call. = FALSE)
    return(old_value)
  }
  new_value
}

# Resolve the whole set of deprecated aliases at once. Called once per
# user-facing entry point, never inside a simulation loop.
.resolve_sim_args <- function(args, supplied) {
  map <- list(
    treatment_or            = "effect_size_or",
    n_clusters_per_sequence = "n_providers_per_specialty",
    sequence_names          = "specialty_names",
    baseline_probs          = "base_probs",
    n_per_cluster_period    = "pts_per_step",
    n_periods               = "n_steps",
    cluster_sd              = "tau_provider"
  )
  for (new_name in names(map)) {
    old_name <- map[[new_name]]
    args[[new_name]] <- .resolve_legacy_argument(
      args[[new_name]], args[[old_name]], new_name, old_name,
      new_supplied = isTRUE(supplied[[new_name]])
    )
    args[[old_name]] <- NULL
  }
  # icc is not a deprecated alias; it is an alternative parameterisation.
  if (!is.null(args$icc)) {
    if (isTRUE(supplied$cluster_sd)) {
      stop("Supply either cluster_sd or icc, not both.", call. = FALSE)
    }
    args$cluster_sd <- icc_to_cluster_sd(args$icc)
  }
  args$icc <- NULL
  args
}

# Core generator: assumes fully resolved, canonical arguments. No deprecation
# handling here, so it can be called in a loop without repeating warnings.
.simulate_stepwedge_core <- function(
  treatment_or, n_clusters_per_sequence, sequence_names, cluster_sd,
  baseline_probs, n_per_cluster_period, n_periods
) {
  if (length(n_clusters_per_sequence) != length(baseline_probs)) {
    stop("n_clusters_per_sequence and baseline_probs must have the same length.",
         call. = FALSE)
  }
  if (length(sequence_names) != length(n_clusters_per_sequence)) {
    stop("sequence_names must have the same length as n_clusters_per_sequence.",
         call. = FALSE)
  }
  if (any(n_clusters_per_sequence <= 0) ||
      any(n_clusters_per_sequence != floor(n_clusters_per_sequence))) {
    stop("n_clusters_per_sequence must contain positive integers.", call. = FALSE)
  }
  if (any(baseline_probs <= 0 | baseline_probs >= 1)) {
    stop("baseline_probs must be strictly between 0 and 1.", call. = FALSE)
  }
  if (length(n_per_cluster_period) != 1L || n_per_cluster_period <= 0) {
    stop("n_per_cluster_period must be a positive scalar.", call. = FALSE)
  }
  if (length(cluster_sd) != 1L || !is.finite(cluster_sd) || cluster_sd < 0) {
    stop("cluster_sd must be a single finite, nonnegative value.", call. = FALSE)
  }
  if (length(n_periods) != 1L || n_periods < 1 || n_periods != floor(n_periods)) {
    stop("n_periods must be a single positive integer.", call. = FALSE)
  }

  total_clusters <- sum(n_clusters_per_sequence)
  sequence_index <- rep(seq_along(n_clusters_per_sequence),
                        times = n_clusters_per_sequence)
  cluster_df <- data.frame(
    cluster_id = seq_len(total_clusters),
    sequence_index = sequence_index,
    sequence = sequence_names[sequence_index],
    random_intercept = stats::rnorm(total_clusters, 0, cluster_sd),
    stringsAsFactors = FALSE
  )
  sim_data <- merge(cluster_df, data.frame(period = seq_len(n_periods)), by = NULL)
  sim_data$intervention <- as.integer(sim_data$period > sim_data$sequence_index)
  sim_data$n <- as.integer(n_per_cluster_period)
  base_intercepts <- stats::qlogis(baseline_probs)
  sim_data$baseline_logit <- base_intercepts[sim_data$sequence_index]
  sim_data$true_logit <- sim_data$baseline_logit +
    log(treatment_or) * sim_data$intervention + sim_data$random_intercept
  sim_data$true_prob <- stats::plogis(sim_data$true_logit)
  sim_data$events <- stats::rbinom(nrow(sim_data), sim_data$n, sim_data$true_prob)

  # Legacy aliases retained so scripts written for version 0.1.0 continue to run.
  sim_data$PID <- sim_data$cluster_id
  sim_data$specialty_idx <- sim_data$sequence_index
  sim_data$specialty <- sim_data$sequence
  sim_data$b_i <- sim_data$random_intercept
  sim_data$step <- sim_data$period
  sim_data$treat <- sim_data$intervention
  sim_data$n_patients <- sim_data$n
  sim_data$n_positive <- sim_data$events
  sim_data$base_logit <- sim_data$baseline_logit

  sim_data[order(sim_data$cluster_id, sim_data$period), , drop = FALSE]
}

#' Simulate one stepped-wedge trial dataset
#'
#' Generates aggregated cluster-by-period binomial data for a stepped-wedge
#' design in which cluster sequences cross over sequentially.
#'
#' @param treatment_or Odds ratio for treatment under the data-generating model.
#' @param n_clusters_per_sequence Integer vector giving the number of clusters in
#'   each sequence.
#' @param sequence_names Labels for the stepped-wedge sequences.
#' @param cluster_sd Standard deviation of the cluster random intercept. Supply
#'   either this argument or \code{icc}.
#' @param icc Optional latent-scale intraclass correlation coefficient for a
#'   logistic random-intercept model.
#' @param baseline_probs Baseline outcome probabilities for each sequence.
#' @param n_per_cluster_period Number of observations per cluster-period.
#' @param n_periods Number of study periods. The default is one baseline period
#'   plus one crossover period per sequence.
#' @param seed Optional random seed.
#' @param effect_size_or Deprecated alias for \code{treatment_or}.
#' @param n_providers_per_specialty Deprecated alias for
#'   \code{n_clusters_per_sequence}.
#' @param specialty_names Deprecated alias for \code{sequence_names}.
#' @param tau_provider Deprecated alias for \code{cluster_sd}.
#' @param base_probs Deprecated alias for \code{baseline_probs}.
#' @param pts_per_step Deprecated alias for \code{n_per_cluster_period}.
#' @param n_steps Deprecated alias for \code{n_periods}.
#'
#' @return A data frame with one row per cluster-period combination. Generic
#'   column names are supplied together with legacy aliases for compatibility.
#' @examples
#' sim <- simulate_stepwedge_trial(
#'   treatment_or = 1.5,
#'   n_clusters_per_sequence = c(10, 10, 10, 10),
#'   baseline_probs = rep(0.05, 4),
#'   icc = 0.05,
#'   n_per_cluster_period = 20,
#'   seed = 1
#' )
#' head(sim[, c("cluster_id", "sequence", "period", "intervention", "n", "events")])
#' @export
simulate_stepwedge_trial <- function(
  treatment_or = 1.5,
  n_clusters_per_sequence = c(40, 40, 40, 40),
  sequence_names = paste0("Sequence ", seq_along(n_clusters_per_sequence)),
  cluster_sd = 1.21,
  icc = NULL,
  baseline_probs = c(0.06, 0.04, 0.03, 0.02),
  n_per_cluster_period = 20,
  n_periods = length(n_clusters_per_sequence) + 1L,
  seed = NULL,
  effect_size_or = NULL,
  n_providers_per_specialty = NULL,
  specialty_names = NULL,
  tau_provider = NULL,
  base_probs = NULL,
  pts_per_step = NULL,
  n_steps = NULL
) {
  supplied <- list(
    treatment_or = !missing(treatment_or),
    n_clusters_per_sequence = !missing(n_clusters_per_sequence),
    sequence_names = !missing(sequence_names),
    cluster_sd = !missing(cluster_sd),
    baseline_probs = !missing(baseline_probs),
    n_per_cluster_period = !missing(n_per_cluster_period),
    n_periods = !missing(n_periods)
  )
  args <- .resolve_sim_args(
    list(
      treatment_or = treatment_or,
      n_clusters_per_sequence = n_clusters_per_sequence,
      sequence_names = sequence_names,
      cluster_sd = cluster_sd,
      icc = icc,
      baseline_probs = baseline_probs,
      n_per_cluster_period = n_per_cluster_period,
      n_periods = n_periods,
      effect_size_or = effect_size_or,
      n_providers_per_specialty = n_providers_per_specialty,
      specialty_names = specialty_names,
      tau_provider = tau_provider,
      base_probs = base_probs,
      pts_per_step = pts_per_step,
      n_steps = n_steps
    ),
    supplied
  )

  # If the sequence count changed via a deprecated alias, the default label and
  # period vectors (which depend on it) must follow.
  n_seq <- length(args$n_clusters_per_sequence)
  if (!supplied$sequence_names && is.null(specialty_names) &&
      length(args$sequence_names) != n_seq) {
    args$sequence_names <- paste0("Sequence ", seq_len(n_seq))
  }
  if (!supplied$n_periods && is.null(n_steps)) {
    args$n_periods <- n_seq + 1L
  }

  if (!is.null(seed)) set.seed(seed)

  .simulate_stepwedge_core(
    treatment_or = args$treatment_or,
    n_clusters_per_sequence = args$n_clusters_per_sequence,
    sequence_names = args$sequence_names,
    cluster_sd = args$cluster_sd,
    baseline_probs = args$baseline_probs,
    n_per_cluster_period = args$n_per_cluster_period,
    n_periods = args$n_periods
  )
}

#' Fit the stepped-wedge analysis model to a simulated dataset
#'
#' Accepts either the generic column names (\code{events}, \code{n},
#' \code{intervention}, \code{period}, \code{sequence_index}, \code{cluster_id})
#' or the legacy 0.1.0 column names (\code{n_positive}, \code{n_patients},
#' \code{treat}, \code{step}, \code{specialty_idx}, \code{PID}).
#'
#' @param sim_data A data frame generated by \code{\link{simulate_stepwedge_trial}}
#'   or a compatible aggregated cluster-period dataset.
#' @param fit_link Link function used in the fitted model.
#' @param nAGQ Number of quadrature points for \code{lme4::glmer()}.
#' @return A list with fitted model, treatment coefficient table, p-value, and
#'   convergence diagnostics.
#' @examples
#' sim <- simulate_stepwedge_trial(n_clusters_per_sequence = c(5, 5, 5, 5),
#'                                 baseline_probs = rep(0.1, 4),
#'                                 icc = 0.05, seed = 1)
#' run_stepwedge_analysis(sim)$p_value
#' @export
run_stepwedge_analysis <- function(sim_data, fit_link = c("logit", "identity"),
                                   nAGQ = 1) {
  fit_link <- match.arg(fit_link)

  generic <- c("events", "n", "intervention", "period", "sequence_index", "cluster_id")
  legacy <- c("n_positive", "n_patients", "treat", "step", "specialty_idx", "PID")

  # Resolve column names rather than rewriting the data with transform(). This
  # keeps all variable references as strings, so R CMD check does not report
  # "no visible binding for global variable".
  if (all(generic %in% names(sim_data))) {
    cols <- stats::setNames(generic, generic)
  } else {
    .check_required_columns(sim_data, legacy)
    cols <- stats::setNames(legacy, generic)
  }

  model_formula <- stats::as.formula(sprintf(
    "cbind(%s, %s - %s) ~ %s + factor(%s) + factor(%s) + (1 | %s)",
    cols[["events"]], cols[["n"]], cols[["events"]],
    cols[["intervention"]], cols[["period"]],
    cols[["sequence_index"]], cols[["cluster_id"]]
  ))

  fit_error <- NULL
  fit <- tryCatch(
    lme4::glmer(
      model_formula,
      family = stats::binomial(link = fit_link),
      data = sim_data,
      nAGQ = nAGQ
    ),
    error = function(e) {
      fit_error <<- conditionMessage(e)
      NULL
    }
  )
  if (is.null(fit)) {
    return(list(fit = NULL, coefficients = NULL, p_value = NA_real_,
                converged = FALSE, singular = NA, error_message = fit_error))
  }

  coefs <- summary(fit)$coefficients
  treat_row <- cols[["intervention"]]
  p_value <- if (treat_row %in% rownames(coefs)) {
    coefs[treat_row, "Pr(>|z|)"]
  } else {
    NA_real_
  }
  opt_messages <- fit@optinfo$conv$lme4$messages

  list(
    fit = fit,
    coefficients = coefs,
    p_value = as.numeric(p_value),
    converged = is.null(opt_messages),
    singular = lme4::isSingular(fit, tol = 1e-4),
    error_message = if (is.null(opt_messages)) NULL else paste(opt_messages, collapse = "; ")
  )
}

# Shared simulation-and-fit driver for estimate_power() and
# estimate_type1_error(). Deprecated aliases are resolved by the caller, so no
# deprecation warning is ever emitted inside this loop.
.run_power_simulations <- function(args, n_simulations, alpha, fit_link, nAGQ, seed) {
  if (!is.null(seed)) set.seed(seed)

  p_values <- rep(NA_real_, n_simulations)
  converged <- singular <- rep(NA, n_simulations)

  for (i in seq_len(n_simulations)) {
    sim_data <- .simulate_stepwedge_core(
      treatment_or = args$treatment_or,
      n_clusters_per_sequence = args$n_clusters_per_sequence,
      sequence_names = args$sequence_names,
      cluster_sd = args$cluster_sd,
      baseline_probs = args$baseline_probs,
      n_per_cluster_period = args$n_per_cluster_period,
      n_periods = args$n_periods
    )
    analysis <- run_stepwedge_analysis(sim_data, fit_link = fit_link, nAGQ = nAGQ)
    p_values[i] <- analysis$p_value
    converged[i] <- analysis$converged
    singular[i] <- analysis$singular
  }

  n_successful <- sum(!is.na(p_values))
  n_rejected <- sum(p_values < alpha, na.rm = TRUE)
  power_est <- if (n_successful > 0L) n_rejected / n_successful else NA_real_
  mcse <- if (n_successful > 0L) {
    sqrt(power_est * (1 - power_est) / n_successful)
  } else {
    NA_real_
  }
  # Exact (Clopper-Pearson) Monte Carlo interval. This retains nominal coverage
  # at the boundary, where a Wald interval collapses to zero width.
  ci <- if (n_successful > 0L) {
    stats::binom.test(n_rejected, n_successful, conf.level = 1 - alpha)$conf.int
  } else {
    c(NA_real_, NA_real_)
  }

  structure(list(
    power = power_est, alpha = alpha,
    mcse = mcse, conf_low = unname(ci[1]), conf_high = unname(ci[2]),
    p_values = p_values, converged = converged, singular = singular,
    n_successful = n_successful, n_failed = n_simulations - n_successful,
    failure_rate = (n_simulations - n_successful) / n_simulations,
    singular_rate = mean(singular, na.rm = TRUE),
    n_simulations = n_simulations
  ), class = "stepwedge_power")
}

#' Estimate power by repeated stepped-wedge simulation
#'
#' Monte Carlo uncertainty is reported as an exact (Clopper-Pearson) interval,
#' which retains nominal coverage when the estimated power is at or near the
#' boundary.
#'
#' @param n_simulations Number of simulations.
#' @param alpha Significance threshold.
#' @inheritParams simulate_stepwedge_trial
#' @param fit_link Link used when fitting the analysis model.
#' @param nAGQ Number of quadrature points for the fitted mixed model.
#' @return An object of class \code{stepwedge_power} containing estimated power,
#'   Monte Carlo uncertainty, fit diagnostics, and simulation p-values.
#' @examples
#' \donttest{
#' estimate_power(
#'   n_simulations = 20, treatment_or = 2,
#'   n_clusters_per_sequence = c(10, 10, 10, 10),
#'   baseline_probs = rep(0.05, 4), icc = 0.05,
#'   n_per_cluster_period = 20, seed = 1
#' )
#' }
#' @export
estimate_power <- function(
  n_simulations = 100,
  alpha = 0.05,
  treatment_or = 2,
  n_clusters_per_sequence = c(10, 10, 10, 10),
  sequence_names = paste0("Sequence ", seq_along(n_clusters_per_sequence)),
  cluster_sd = 1.21,
  icc = NULL,
  baseline_probs = c(0.07, 0.04, 0.03, 0.02),
  n_per_cluster_period = 20,
  n_periods = length(n_clusters_per_sequence) + 1L,
  fit_link = c("logit", "identity"),
  seed = NULL,
  nAGQ = 1,
  effect_size_or = NULL,
  n_providers_per_specialty = NULL,
  specialty_names = NULL,
  tau_provider = NULL,
  base_probs = NULL,
  pts_per_step = NULL,
  n_steps = NULL
) {
  fit_link <- match.arg(fit_link)

  supplied <- list(
    treatment_or = !missing(treatment_or),
    n_clusters_per_sequence = !missing(n_clusters_per_sequence),
    sequence_names = !missing(sequence_names),
    cluster_sd = !missing(cluster_sd),
    baseline_probs = !missing(baseline_probs),
    n_per_cluster_period = !missing(n_per_cluster_period),
    n_periods = !missing(n_periods)
  )

  # Deprecated aliases are resolved exactly once, here, so a long simulation run
  # emits a single deprecation warning rather than one per replicate.
  args <- .resolve_sim_args(
    list(
      treatment_or = treatment_or,
      n_clusters_per_sequence = n_clusters_per_sequence,
      sequence_names = sequence_names,
      cluster_sd = cluster_sd,
      icc = icc,
      baseline_probs = baseline_probs,
      n_per_cluster_period = n_per_cluster_period,
      n_periods = n_periods,
      effect_size_or = effect_size_or,
      n_providers_per_specialty = n_providers_per_specialty,
      specialty_names = specialty_names,
      tau_provider = tau_provider,
      base_probs = base_probs,
      pts_per_step = pts_per_step,
      n_steps = n_steps
    ),
    supplied
  )

  n_seq <- length(args$n_clusters_per_sequence)
  if (!supplied$sequence_names && is.null(specialty_names) &&
      length(args$sequence_names) != n_seq) {
    args$sequence_names <- paste0("Sequence ", seq_len(n_seq))
  }
  if (!supplied$n_periods && is.null(n_steps)) {
    args$n_periods <- n_seq + 1L
  }

  out <- .run_power_simulations(args, n_simulations, alpha, fit_link, nAGQ, seed)
  out$call <- match.call()
  out
}

#' @export
print.stepwedge_power <- function(x, ...) {
  cat("Simulation-based stepped-wedge result\n")
  cat(sprintf("Estimate: %.3f (%.0f%% exact MC interval %.3f to %.3f)\n",
              x$power, 100 * (1 - x$alpha), x$conf_low, x$conf_high))
  cat(sprintf("Monte Carlo SE: %.4f\n", x$mcse))
  cat(sprintf("Successful fits: %d/%d; failed: %d\n",
              x$n_successful, x$n_simulations, x$n_failed))
  if (!is.nan(x$singular_rate)) {
    cat(sprintf("Singular fits: %.1f%%\n", 100 * x$singular_rate))
  }
  invisible(x)
}

#' Estimate type I error by repeated stepped-wedge simulation
#'
#' Equivalent to \code{\link{estimate_power}} with the treatment odds ratio set
#' to 1.
#'
#' @inheritParams estimate_power
#' @return A \code{stepwedge_power} object with \code{type1_error} added.
#' @examples
#' \donttest{
#' estimate_type1_error(
#'   n_simulations = 20,
#'   n_clusters_per_sequence = c(10, 10, 10, 10),
#'   baseline_probs = rep(0.05, 4), icc = 0.05,
#'   n_per_cluster_period = 20, seed = 1
#' )
#' }
#' @export
estimate_type1_error <- function(
  n_simulations = 100,
  alpha = 0.05,
  n_clusters_per_sequence = c(10, 10, 10, 10),
  sequence_names = paste0("Sequence ", seq_along(n_clusters_per_sequence)),
  cluster_sd = 1.21,
  icc = NULL,
  baseline_probs = c(0.07, 0.04, 0.03, 0.02),
  n_per_cluster_period = 20,
  n_periods = length(n_clusters_per_sequence) + 1L,
  fit_link = c("logit", "identity"),
  seed = NULL,
  nAGQ = 1,
  n_providers_per_specialty = NULL,
  specialty_names = NULL,
  tau_provider = NULL,
  base_probs = NULL,
  pts_per_step = NULL,
  n_steps = NULL
) {
  fit_link <- match.arg(fit_link)

  supplied <- list(
    treatment_or = FALSE,
    n_clusters_per_sequence = !missing(n_clusters_per_sequence),
    sequence_names = !missing(sequence_names),
    cluster_sd = !missing(cluster_sd),
    baseline_probs = !missing(baseline_probs),
    n_per_cluster_period = !missing(n_per_cluster_period),
    n_periods = !missing(n_periods)
  )

  args <- .resolve_sim_args(
    list(
      treatment_or = 1,
      n_clusters_per_sequence = n_clusters_per_sequence,
      sequence_names = sequence_names,
      cluster_sd = cluster_sd,
      icc = icc,
      baseline_probs = baseline_probs,
      n_per_cluster_period = n_per_cluster_period,
      n_periods = n_periods,
      effect_size_or = NULL,
      n_providers_per_specialty = n_providers_per_specialty,
      specialty_names = specialty_names,
      tau_provider = tau_provider,
      base_probs = base_probs,
      pts_per_step = pts_per_step,
      n_steps = n_steps
    ),
    supplied
  )

  n_seq <- length(args$n_clusters_per_sequence)
  if (!supplied$sequence_names && is.null(specialty_names) &&
      length(args$sequence_names) != n_seq) {
    args$sequence_names <- paste0("Sequence ", seq_len(n_seq))
  }
  if (!supplied$n_periods && is.null(n_steps)) {
    args$n_periods <- n_seq + 1L
  }

  out <- .run_power_simulations(args, n_simulations, alpha, fit_link, nAGQ, seed)
  out$call <- match.call()
  out$type1_error <- out$power
  out
}
