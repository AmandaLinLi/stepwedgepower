#' Estimate power for a stepped-wedge trial by simulation
#'
#' Repeatedly simulates a stepped-wedge trial from a [sw_design()] and
#' [sw_assumptions()], fits the analysis model to each replicate, and estimates
#' the rejection probability at level `alpha`. Returns a rich [sw_power] object
#' including the Monte Carlo standard error, an exact (Clopper-Pearson)
#' confidence interval for power, per-simulation treatment estimates and
#' standard errors, convergence and singular-fit diagnostics, and empirical bias
#' and CI coverage relative to the true effect.
#'
#' The Monte Carlo interval for power is exact rather than Wald: a Wald interval
#' collapses to zero width when the estimated power reaches 0 or 1, which is
#' exactly the high-power regime a power analysis is designed to land in.
#'
#' @param design An [sw_design()] object.
#' @param assumptions An [sw_assumptions()] object.
#' @param nsim Number of simulations.
#' @param alpha Two-sided significance level.
#' @param fit_link Link used when fitting the analysis model.
#' @param nAGQ Quadrature points for the analysis model.
#' @param analysis_args Optional named list of extra arguments to
#'   [fit_stepwedge_model()] (e.g. `period_effect`, `adjust_sequence`).
#' @param seed Optional seed.
#'
#' @return An object of class `"sw_power"`.
#' @examples
#' \donttest{
#' design <- sw_design(clusters_per_sequence = c(8, 8, 8, 8),
#'                     crossover_period = c(2, 3, 4, 5), n_periods = 5)
#' a <- sw_assumptions(baseline_prob = 0.05, treatment_or = 2, icc = 0.03,
#'                     n_per_cluster_period = 20)
#' pw <- power_swcrt(design, a, nsim = 50, seed = 1)
#' summary(pw)
#' }
#' @export
power_swcrt <- function(
  design,
  assumptions,
  nsim = 1000,
  alpha = 0.05,
  fit_link = c("logit", "identity"),
  nAGQ = 1,
  analysis_args = list(),
  seed = NULL
) {
  if (!inherits(design, "sw_design")) {
    stop("`design` must be an sw_design object.", call. = FALSE)
  }
  if (!inherits(assumptions, "sw_assumptions")) {
    stop("`assumptions` must be an sw_assumptions object.", call. = FALSE)
  }
  fit_link <- match.arg(fit_link)
  if (!is.null(seed)) set.seed(seed)

  p_values <- rep(NA_real_, nsim)
  estimates <- rep(NA_real_, nsim)
  ses <- rep(NA_real_, nsim)
  converged <- rep(NA, nsim)
  singular <- rep(NA, nsim)

  true_effect <- assumptions$treatment_effect
  z <- stats::qnorm(1 - alpha / 2)

  for (i in seq_len(nsim)) {
    sim <- simulate_swcrt(design, assumptions)
    res <- do.call(fit_stepwedge_model,
                   c(list(data = sim, link = fit_link, nAGQ = nAGQ), analysis_args))
    p_values[i] <- res$p_value
    estimates[i] <- res$estimate
    ses[i] <- res$std_error
    converged[i] <- isTRUE(res$converged)
    singular[i] <- isTRUE(res$singular)
  }

  ok <- !is.na(p_values)
  n_successful <- sum(ok)
  n_failed <- sum(!ok)
  n_rejected <- sum(p_values[ok] < alpha)

  power <- if (n_successful > 0) n_rejected / n_successful else NA_real_
  mcse <- if (n_successful > 0) sqrt(power * (1 - power) / n_successful) else NA_real_

  # Exact (Clopper-Pearson) Monte Carlo interval for the rejection probability.
  # A Wald interval collapses to zero width when the estimate reaches 0 or 1 --
  # precisely the high-power regime a power analysis targets -- so the exact
  # interval is used instead.
  ci <- if (n_successful > 0) {
    stats::binom.test(n_rejected, n_successful, conf.level = 1 - alpha)$conf.int
  } else {
    c(NA_real_, NA_real_)
  }
  conf_low <- unname(ci[1])
  conf_high <- unname(ci[2])

  est_ok <- !is.na(estimates) & !is.na(ses)
  bias <- if (any(est_ok)) mean(estimates[est_ok]) - true_effect else NA_real_
  coverage <- if (any(est_ok)) {
    lo <- estimates[est_ok] - z * ses[est_ok]
    hi <- estimates[est_ok] + z * ses[est_ok]
    mean(lo <= true_effect & true_effect <= hi)
  } else NA_real_

  structure(
    list(
      power = power, mcse = mcse, conf.int = c(conf_low, conf_high),
      alpha = alpha, nsim = nsim,
      successful = n_successful, failed = n_failed,
      failure_rate = n_failed / nsim,
      p_values = p_values,
      estimated_effects = estimates, estimated_ses = ses,
      convergence_status = converged, singular_fit = singular,
      true_effect = true_effect, bias = bias, coverage = coverage,
      design = design, assumptions = assumptions, call = match.call()
    ),
    class = "sw_power"
  )
}

#' Methods for `sw_power` objects
#'
#' `print`, `summary`, and `plot` methods for the object returned by
#' [power_swcrt()].
#'
#' @param x,object An `sw_power` object.
#' @param ... Unused.
#' @name sw_power
NULL

#' @rdname sw_power
#' @export
print.sw_power <- function(x, ...) {
  cat("<sw_power>\n")
  if (is.na(x$power)) {
    cat("  power: NA (no successful fits)\n")
  } else {
    cat(sprintf("  power = %.3f  (MCSE %.4f, %d%% exact MC interval %.3f-%.3f)\n",
                x$power, x$mcse, round(100 * (1 - x$alpha)),
                x$conf.int[1], x$conf.int[2]))
  }
  cat(sprintf("  nsim = %d  (successful %d, failed %d, failure rate %.3f)\n",
              x$nsim, x$successful, x$failed, x$failure_rate))
  invisible(x)
}

#' @rdname sw_power
#' @export
summary.sw_power <- function(object, ...) {
  cat("Stepped-wedge power (simulation)\n")
  cat("--------------------------------\n")
  print.sw_power(object)
  cat(sprintf("  alpha = %.3f\n", object$alpha))
  cat(sprintf("  true treatment effect (log-odds) = %.3f (OR %.3f)\n",
              object$true_effect, exp(object$true_effect)))
  if (!is.na(object$bias)) cat(sprintf("  empirical bias (log-odds) = %.4f\n", object$bias))
  if (!is.na(object$coverage)) cat(sprintf("  CI coverage of true effect = %.3f\n", object$coverage))
  conv <- mean(object$convergence_status, na.rm = TRUE)
  sing <- mean(object$singular_fit, na.rm = TRUE)
  if (!is.nan(conv)) cat(sprintf("  convergence rate = %.3f\n", conv))
  if (!is.nan(sing)) cat(sprintf("  singular-fit rate = %.3f\n", sing))
  invisible(object)
}

#' @rdname sw_power
#' @export
plot.sw_power <- function(x, ...) {
  eff <- x$estimated_effects
  eff <- eff[!is.na(eff)]
  if (length(eff) == 0) stop("No estimated effects to plot.", call. = FALSE)
  graphics::hist(eff, breaks = "FD",
                 main = "Distribution of estimated treatment effect",
                 xlab = "Estimated treatment effect (log-odds)",
                 col = "grey85", border = "white")
  graphics::abline(v = x$true_effect, col = "red", lwd = 2, lty = 2)
  graphics::abline(v = mean(eff), col = "blue", lwd = 2)
  graphics::legend("topright", legend = c("true effect", "mean estimate"),
                   col = c("red", "blue"), lty = c(2, 1), lwd = 2, bty = "n")
  invisible(x)
}
