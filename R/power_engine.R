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
#' @param n_cores Number of cores. `1` runs serially. Results are reproducible
#'   under a given `seed` regardless of `n_cores`, because each replicate is
#'   assigned its own L'Ecuyer-CMRG random-number stream.
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
  n_cores = 1,
  seed = NULL
) {
  if (!inherits(design, "sw_design")) {
    stop("`design` must be an sw_design object.", call. = FALSE)
  }
  if (!inherits(assumptions, "sw_assumptions")) {
    stop("`assumptions` must be an sw_assumptions object.", call. = FALSE)
  }
  fit_link <- match.arg(fit_link)

  true_effect <- assumptions$treatment_effect
  z <- stats::qnorm(1 - alpha / 2)

  one_rep <- function(i) {
    sim <- simulate_swcrt(design, assumptions)
    res <- do.call(fit_stepwedge_model,
                   c(list(data = sim, link = fit_link, nAGQ = nAGQ), analysis_args))
    list(
      p = res$p_value, est = res$estimate, se = res$std_error,
      conv = isTRUE(res$converged), sing = isTRUE(res$singular)
    )
  }

  reps <- .simulate_replicates(one_rep, nsim, n_cores = n_cores, seed = seed)

  p_values <- vapply(reps, function(r) r$p, numeric(1))
  estimates <- vapply(reps, function(r) r$est, numeric(1))
  ses <- vapply(reps, function(r) r$se, numeric(1))
  converged <- vapply(reps, function(r) r$conv, logical(1))
  singular <- vapply(reps, function(r) r$sing, logical(1))

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

# Run `nsim` replicates of `fun`, serially or in parallel.
#
# Reproducibility does not depend on `n_cores`: each replicate is given its own
# L'Ecuyer-CMRG substream, so the i-th replicate sees the same random numbers
# whether it runs on one core or many. This matters because a power estimate
# that shifts when you add cores is not a power estimate anyone can cite.
.simulate_replicates <- function(fun, nsim, n_cores = 1, seed = NULL) {
  n_cores <- max(1L, as.integer(n_cores))

  if (is.null(seed)) {
    if (n_cores == 1L) return(lapply(seq_len(nsim), fun))
    return(.par_lapply(seq_len(nsim), fun, n_cores))
  }

  # Pre-compute one RNG stream per replicate from the user's seed.
  old_kind <- RNGkind()[1]
  old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv())
  } else {
    NULL
  }
  on.exit({
    RNGkind(old_kind)
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = globalenv())
  }, add = TRUE)

  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  streams <- vector("list", nsim)
  s <- .Random.seed
  for (i in seq_len(nsim)) {
    streams[[i]] <- s
    s <- parallel::nextRNGStream(s)
  }

  run_i <- function(i) {
    assign(".Random.seed", streams[[i]], envir = globalenv())
    fun(i)
  }

  if (n_cores == 1L) {
    return(lapply(seq_len(nsim), run_i))
  }
  .par_lapply(seq_len(nsim), run_i, n_cores)
}

.par_lapply <- function(x, fun, n_cores) {
  n_cores <- min(n_cores, length(x))
  available <- parallel::detectCores(logical = FALSE)
  if (!is.na(available)) n_cores <- min(n_cores, available)
  if (n_cores <= 1L) return(lapply(x, fun))

  if (.Platform$OS.type != "windows") {
    return(parallel::mclapply(x, fun, mc.cores = n_cores))
  }
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, requireNamespace("stepwedgepower", quietly = TRUE))
  parallel::parLapply(cl, x, fun)
}
