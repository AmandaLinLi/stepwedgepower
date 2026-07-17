#' Estimate power for an asynchronous Control to A to A+B design
#'
#' Repeatedly simulates a cumulative-intervention stepped-wedge trial and fits
#' the prespecified mixed model. Separate operating characteristics are reported
#' for A versus Control, incremental B (A+B versus A), total A+B versus Control,
#' multiplicity-adjusted component tests, and joint success of both component
#' tests.
#'
#' Two power definitions are reported. **Conditional power** uses only
#' replicates in which the planned analysis converged and produced the relevant
#' test. **Failure-aware power** divides the same number of rejections by all
#' simulated trials, treating an unusable analysis as an unsuccessful trial.
#' Their difference reveals how much nominal performance depends on silently
#' discarding failed fits.
#'
#' @param design An [sw_multistage_design()] object.
#' @param assumptions An [sw_multistage_assumptions()] object.
#' @param nsim Number of simulated trials.
#' @param alpha Two-sided significance level.
#' @param multiplicity Adjustment for the A and incremental-B component tests:
#'   `"holm"`, `"bonferroni"`, or `"none"`.
#' @param fit_link Link used in [fit_multistage_model()].
#' @param nAGQ Adaptive Gauss-Hermite quadrature points.
#' @param analysis_args Optional named list of additional arguments passed to
#'   [fit_multistage_model()], such as `adjust_sequence` or `period_effect`.
#' @param n_cores Number of cores. Reproducibility under a fixed `seed` does not
#'   depend on this value.
#' @param seed Optional random seed.
#' @param check_design Logical; audit the schedule before simulation and stop
#'   for a rank-deficient or non-estimable default model.
#' @param warn_on_design Logical; emit non-fatal design-audit cautions.
#'
#' @return An object of class `"sw_multistage_power"`.
#' @examples
#' \donttest{
#' design <- sw_multistage_design(
#'   clusters_per_sequence = rep(5, 5),
#'   a_start = c(2, 3, 4, 5, 6),
#'   b_start = c(6, 8, 7, 9, 10),
#'   n_periods = 10
#' )
#' assumptions <- sw_multistage_assumptions(
#'   baseline_prob = 0.15,
#'   treatment_or_a = 1.4,
#'   incremental_or_b = 1.3,
#'   delay_a = 1, delay_b = 1,
#'   icc = 0.05,
#'   n_per_cluster_period = 30
#' )
#' result <- power_multistage_swcrt(
#'   design, assumptions, nsim = 100, seed = 1
#' )
#' result
#' }
#' @export
power_multistage_swcrt <- function(
  design,
  assumptions,
  nsim = 1000,
  alpha = 0.05,
  multiplicity = c("holm", "bonferroni", "none"),
  fit_link = c("logit", "identity"),
  nAGQ = 1,
  analysis_args = list(),
  n_cores = 1,
  seed = NULL,
  check_design = TRUE,
  warn_on_design = TRUE
) {
  if (!inherits(design, "sw_multistage_design")) {
    stop("`design` must be an sw_multistage_design object.", call. = FALSE)
  }
  if (!inherits(assumptions, "sw_multistage_assumptions")) {
    stop("`assumptions` must be an sw_multistage_assumptions object.",
         call. = FALSE)
  }
  if (length(nsim) != 1L || !is.finite(nsim) || nsim < 1 ||
      nsim != round(nsim)) {
    stop("`nsim` must be one positive integer.", call. = FALSE)
  }
  nsim <- as.integer(nsim)
  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be one number strictly between 0 and 1.", call. = FALSE)
  }
  multiplicity <- match.arg(multiplicity)
  fit_link <- match.arg(fit_link)
  if (!is.list(analysis_args) || is.null(names(analysis_args)) && length(analysis_args)) {
    stop("`analysis_args` must be a named list.", call. = FALSE)
  }
  reserved <- intersect(
    names(analysis_args),
    c("data", "alpha", "multiplicity", "link", "nAGQ")
  )
  if (length(reserved)) {
    stop("Do not place reserved arguments in `analysis_args`: ",
         paste(reserved, collapse = ", "), call. = FALSE)
  }

  audit <- audit_multistage_design(
    design,
    delay_a = assumptions$delay_a,
    delay_b = assumptions$delay_b,
    adjust_sequence = isTRUE(analysis_args$adjust_sequence)
  )
  if (isTRUE(check_design)) {
    fatal <- !audit$full_rank || !audit$has_a_variation ||
      (design$has_b && !audit$has_b_variation)
    if (fatal) {
      stop(
        "The requested default multistage analysis is not estimable. ",
        paste(audit$messages, collapse = " "),
        call. = FALSE
      )
    }
  }
  if (isTRUE(warn_on_design) && length(audit$messages)) {
    warning(
      "Design audit: ", paste(audit$messages, collapse = " "),
      call. = FALSE
    )
  }

  has_b <- design$has_b
  contrast_names <- c(
    "A_vs_control",
    if (has_b) c("AB_vs_A", "AB_vs_control")
  )

  one_replicate <- function(i) {
    trial <- simulate_multistage_swcrt(design, assumptions)
    fit <- do.call(
      fit_multistage_model,
      c(
        list(
          data = trial,
          link = fit_link,
          nAGQ = nAGQ,
          alpha = alpha,
          multiplicity = multiplicity
        ),
        analysis_args
      )
    )

    contrast_rows <- match(contrast_names, fit$contrasts$contrast)
    estimates <- fit$contrasts$estimate[contrast_rows]
    standard_errors <- fit$contrasts$std_error[contrast_rows]
    raw_p <- fit$contrasts$p_value[contrast_rows]
    names(estimates) <- names(standard_errors) <- names(raw_p) <- contrast_names

    adjusted <- c(A_vs_control = NA_real_, AB_vs_A = NA_real_)
    adjusted[names(fit$adjusted_p_values)] <- fit$adjusted_p_values

    list(
      estimates = estimates,
      standard_errors = standard_errors,
      raw_p = raw_p,
      adjusted_p = adjusted,
      joint_success = fit$joint_success,
      converged = isTRUE(fit$converged),
      singular = fit$singular,
      has_warning = length(fit$warnings) > 0L,
      warnings = fit$warnings,
      error = fit$error
    )
  }

  replicates <- .simulate_replicates(
    one_replicate,
    nsim = nsim,
    n_cores = n_cores,
    seed = seed
  )

  raw_p <- .multistage_bind_numeric(replicates, "raw_p", contrast_names)
  estimates <- .multistage_bind_numeric(
    replicates, "estimates", contrast_names
  )
  standard_errors <- .multistage_bind_numeric(
    replicates, "standard_errors", contrast_names
  )
  adjusted_p <- .multistage_bind_numeric(
    replicates,
    "adjusted_p",
    c("A_vs_control", "AB_vs_A")
  )
  converged <- vapply(replicates, function(x) x$converged, logical(1))
  singular <- vapply(replicates, function(x) {
    if (length(x$singular) == 0L || is.na(x$singular)) NA else isTRUE(x$singular)
  }, logical(1))
  has_warning <- vapply(replicates, function(x) x$has_warning, logical(1))
  joint_success <- vapply(replicates, function(x) {
    if (length(x$joint_success) == 0L || is.na(x$joint_success)) NA else isTRUE(x$joint_success)
  }, logical(1))

  power_rows <- list()
  power_rows[[length(power_rows) + 1L]] <- .summarize_multistage_test(
    p_values = raw_p[, "A_vs_control"],
    converged = converged,
    nsim = nsim,
    alpha = alpha,
    test = "A_vs_control_raw",
    label = "A vs Control (unadjusted)",
    adjustment = "none"
  )

  if (has_b) {
    power_rows[[length(power_rows) + 1L]] <- .summarize_multistage_test(
      raw_p[, "AB_vs_A"], converged, nsim, alpha,
      test = "AB_vs_A_raw",
      label = "A+B vs A: incremental B (unadjusted)",
      adjustment = "none"
    )
    power_rows[[length(power_rows) + 1L]] <- .summarize_multistage_test(
      raw_p[, "AB_vs_control"], converged, nsim, alpha,
      test = "AB_vs_control_raw",
      label = "A+B vs Control: total effect",
      adjustment = "none"
    )
    power_rows[[length(power_rows) + 1L]] <- .summarize_multistage_test(
      adjusted_p[, "A_vs_control"], converged, nsim, alpha,
      test = "A_vs_control_adjusted",
      label = paste0("A vs Control (", multiplicity, "-adjusted)"),
      adjustment = multiplicity
    )
    power_rows[[length(power_rows) + 1L]] <- .summarize_multistage_test(
      adjusted_p[, "AB_vs_A"], converged, nsim, alpha,
      test = "AB_vs_A_adjusted",
      label = paste0("A+B vs A (", multiplicity, "-adjusted)"),
      adjustment = multiplicity
    )
    power_rows[[length(power_rows) + 1L]] <- .summarize_multistage_indicator(
      indicator = joint_success,
      converged = converged,
      nsim = nsim,
      alpha = alpha,
      test = "joint_component_success",
      label = paste0("Joint success for A and B (", multiplicity, ")"),
      adjustment = multiplicity
    )
  }
  power_table <- do.call(rbind, power_rows)
  rownames(power_table) <- NULL

  true_effects <- if (fit_link == "logit") {
    c(
      A_vs_control = assumptions$treatment_effect_a,
      AB_vs_A = assumptions$incremental_effect_b,
      AB_vs_control = assumptions$total_effect_ab
    )
  } else {
    c(A_vs_control = NA_real_, AB_vs_A = NA_real_,
      AB_vs_control = NA_real_)
  }
  estimation_rows <- lapply(contrast_names, function(contrast) {
    .summarize_multistage_estimation(
      estimates = estimates[, contrast],
      standard_errors = standard_errors[, contrast],
      converged = converged,
      true_effect = true_effects[[contrast]],
      alpha = alpha,
      contrast = contrast
    )
  })
  estimation_table <- do.call(rbind, estimation_rows)
  rownames(estimation_table) <- NULL

  fit_success <- converged & is.finite(raw_p[, "A_vs_control"])
  if (has_b) {
    fit_success <- fit_success & is.finite(raw_p[, "AB_vs_A"])
  }

  structure(
    list(
      power_table = power_table,
      estimation_table = estimation_table,
      alpha = alpha,
      nsim = nsim,
      multiplicity = multiplicity,
      fit_success_rate = mean(fit_success),
      failure_rate = mean(!fit_success),
      convergence_rate = mean(converged),
      singular_rate = if (any(converged & !is.na(singular))) {
        mean(singular[converged], na.rm = TRUE)
      } else {
        NA_real_
      },
      warning_rate = mean(has_warning),
      raw_p_values = raw_p,
      adjusted_p_values = adjusted_p,
      estimated_effects = estimates,
      estimated_ses = standard_errors,
      convergence_status = converged,
      singular_fit = singular,
      joint_success = joint_success,
      design_audit = audit,
      design = design,
      assumptions = assumptions,
      call = match.call()
    ),
    class = "sw_multistage_power"
  )
}

.multistage_bind_numeric <- function(replicates, field, names_expected) {
  out <- matrix(
    NA_real_, nrow = length(replicates), ncol = length(names_expected),
    dimnames = list(NULL, names_expected)
  )
  for (i in seq_along(replicates)) {
    value <- replicates[[i]][[field]]
    if (is.null(value)) next
    common <- intersect(names_expected, names(value))
    if (length(common)) out[i, common] <- as.numeric(value[common])
  }
  out
}

.summarize_multistage_test <- function(
  p_values,
  converged,
  nsim,
  alpha,
  test,
  label,
  adjustment
) {
  evaluable <- converged & is.finite(p_values)
  reject <- evaluable & p_values < alpha
  .summarize_multistage_rejection(
    reject, evaluable, nsim, alpha, test, label, adjustment
  )
}

.summarize_multistage_indicator <- function(
  indicator,
  converged,
  nsim,
  alpha,
  test,
  label,
  adjustment
) {
  evaluable <- converged & !is.na(indicator)
  reject <- evaluable & indicator
  .summarize_multistage_rejection(
    reject, evaluable, nsim, alpha, test, label, adjustment
  )
}

.summarize_multistage_rejection <- function(
  reject,
  evaluable,
  nsim,
  alpha,
  test,
  label,
  adjustment
) {
  n_evaluable <- sum(evaluable)
  n_rejected <- sum(reject)

  conditional_power <- if (n_evaluable) n_rejected / n_evaluable else NA_real_
  failure_aware_power <- n_rejected / nsim
  conditional_mcse <- if (n_evaluable) {
    sqrt(conditional_power * (1 - conditional_power) / n_evaluable)
  } else {
    NA_real_
  }
  failure_aware_mcse <- sqrt(
    failure_aware_power * (1 - failure_aware_power) / nsim
  )
  conditional_ci <- .exact_binomial_interval(
    n_rejected, n_evaluable, conf_level = 1 - alpha
  )
  failure_aware_ci <- .exact_binomial_interval(
    n_rejected, nsim, conf_level = 1 - alpha
  )

  data.frame(
    test = test,
    label = label,
    adjustment = adjustment,
    conditional_power = conditional_power,
    conditional_mcse = conditional_mcse,
    conditional_conf_low = conditional_ci[1],
    conditional_conf_high = conditional_ci[2],
    failure_aware_power = failure_aware_power,
    failure_aware_mcse = failure_aware_mcse,
    failure_aware_conf_low = failure_aware_ci[1],
    failure_aware_conf_high = failure_aware_ci[2],
    n_rejected = n_rejected,
    n_evaluable = n_evaluable,
    n_failed = nsim - n_evaluable,
    nsim = nsim,
    stringsAsFactors = FALSE
  )
}

.exact_binomial_interval <- function(successes, trials, conf_level) {
  if (!trials) return(c(NA_real_, NA_real_))
  unname(stats::binom.test(
    successes, trials, conf.level = conf_level
  )$conf.int[1:2])
}

.summarize_multistage_estimation <- function(
  estimates,
  standard_errors,
  converged,
  true_effect,
  alpha,
  contrast
) {
  usable <- converged & is.finite(estimates) & is.finite(standard_errors) &
    standard_errors > 0
  n_usable <- sum(usable)
  if (!n_usable) {
    return(data.frame(
      contrast = contrast,
      true_effect = true_effect,
      true_odds_ratio = exp(true_effect),
      mean_estimate = NA_real_,
      bias = NA_real_,
      empirical_sd = NA_real_,
      mean_model_se = NA_real_,
      se_ratio = NA_real_,
      coverage = NA_real_,
      n_usable = 0L,
      stringsAsFactors = FALSE
    ))
  }

  estimate <- estimates[usable]
  model_se <- standard_errors[usable]
  critical <- stats::qnorm(1 - alpha / 2)
  empirical_sd <- if (length(estimate) > 1L) stats::sd(estimate) else NA_real_
  mean_model_se <- mean(model_se)
  coverage <- mean(
    estimate - critical * model_se <= true_effect &
      true_effect <= estimate + critical * model_se
  )

  finite_truth <- is.finite(true_effect)
  if (!finite_truth) coverage <- NA_real_

  data.frame(
    contrast = contrast,
    true_effect = true_effect,
    true_odds_ratio = if (finite_truth) exp(true_effect) else NA_real_,
    mean_estimate = mean(estimate),
    bias = if (finite_truth) mean(estimate) - true_effect else NA_real_,
    empirical_sd = empirical_sd,
    mean_model_se = mean_model_se,
    se_ratio = if (is.finite(empirical_sd) && empirical_sd > 0) {
      mean_model_se / empirical_sd
    } else {
      NA_real_
    },
    coverage = coverage,
    n_usable = n_usable,
    stringsAsFactors = FALSE
  )
}

#' Estimate global-null type I error for a multistage stepped-wedge design
#'
#' Convenience wrapper around [power_multistage_swcrt()] that sets both the A
#' and incremental-B effects to zero while preserving every other assumption.
#'
#' @inheritParams power_multistage_swcrt
#' @return An `sw_multistage_power` object evaluated under the global null.
#' @examples
#' \donttest{
#' design <- sw_multistage_design(
#'   clusters_per_sequence = rep(4, 4),
#'   a_start = c(2, 3, 4, 5), b_start = c(5, 7, 6, 8),
#'   n_periods = 8
#' )
#' a <- sw_multistage_assumptions(
#'   baseline_prob = 0.15, treatment_or_a = 1.4,
#'   incremental_or_b = 1.3, icc = 0.05
#' )
#' type1_multistage_swcrt(design, a, nsim = 100, seed = 1)
#' }
#' @export
type1_multistage_swcrt <- function(
  design,
  assumptions,
  nsim = 1000,
  alpha = 0.05,
  multiplicity = c("holm", "bonferroni", "none"),
  fit_link = c("logit", "identity"),
  nAGQ = 1,
  analysis_args = list(),
  n_cores = 1,
  seed = NULL,
  check_design = TRUE,
  warn_on_design = TRUE
) {
  if (!inherits(assumptions, "sw_multistage_assumptions")) {
    stop("`assumptions` must be an sw_multistage_assumptions object.",
         call. = FALSE)
  }
  null_assumptions <- assumptions
  null_assumptions$treatment_effect_a <- 0
  null_assumptions$treatment_or_a <- 1
  null_assumptions$incremental_effect_b <- 0
  null_assumptions$incremental_or_b <- 1
  null_assumptions$total_effect_ab <- 0
  null_assumptions$total_or_ab <- 1

  out <- power_multistage_swcrt(
    design = design,
    assumptions = null_assumptions,
    nsim = nsim,
    alpha = alpha,
    multiplicity = match.arg(multiplicity),
    fit_link = match.arg(fit_link),
    nAGQ = nAGQ,
    analysis_args = analysis_args,
    n_cores = n_cores,
    seed = seed,
    check_design = check_design,
    warn_on_design = warn_on_design
  )
  out$global_null <- TRUE
  out$call <- match.call()
  out
}

#' Methods for multistage stepped-wedge power objects
#'
#' @param x,object An `sw_multistage_power` object.
#' @param type Power definition for the plot: `"conditional"` or
#'   `"failure-aware"`.
#' @param ... Additional graphical arguments for `plot`; otherwise unused.
#' @name sw_multistage_power
NULL

#' @rdname sw_multistage_power
#' @export
print.sw_multistage_power <- function(x, ...) {
  cat("<sw_multistage_power>\n")
  display <- x$power_table[, c(
    "label", "conditional_power", "failure_aware_power",
    "n_evaluable", "n_failed"
  ), drop = FALSE]
  names(display) <- c(
    "test", "conditional", "failure_aware", "evaluable", "failed"
  )
  print(display, row.names = FALSE, digits = 3)
  cat(sprintf(
    "\n  nsim = %d; convergence %.3f; fit success %.3f; singular %.3f\n",
    x$nsim, x$convergence_rate, x$fit_success_rate, x$singular_rate
  ))
  invisible(x)
}

#' @rdname sw_multistage_power
#' @export
summary.sw_multistage_power <- function(object, ...) {
  cat("Asynchronous cumulative stepped-wedge operating characteristics\n")
  cat("---------------------------------------------------------------\n")
  print.sw_multistage_power(object)
  cat("\nEstimation diagnostics (log-odds scale)\n")
  print(object$estimation_table, row.names = FALSE, digits = 3)
  if (length(object$design_audit$messages)) {
    cat("\nDesign-audit cautions\n")
    cat(paste0("- ", object$design_audit$messages, collapse = "\n"), "\n")
  }
  invisible(object)
}

#' @rdname sw_multistage_power
#' @export
plot.sw_multistage_power <- function(
  x,
  type = c("conditional", "failure-aware"),
  ...
) {
  type <- match.arg(type)
  table <- x$power_table
  if (type == "conditional") {
    estimate <- table$conditional_power
    lower <- table$conditional_conf_low
    upper <- table$conditional_conf_high
    xlab <- "Conditional rejection probability"
  } else {
    estimate <- table$failure_aware_power
    lower <- table$failure_aware_conf_low
    upper <- table$failure_aware_conf_high
    xlab <- "Failure-aware rejection probability"
  }
  ok <- is.finite(estimate)
  if (!any(ok)) stop("No finite power estimates to plot.", call. = FALSE)
  y <- rev(seq_len(sum(ok)))
  graphics::plot(
    estimate[ok], y,
    xlim = c(0, 1), ylim = c(0.5, sum(ok) + 0.5),
    yaxt = "n", ylab = "", xlab = xlab,
    pch = 19, ...
  )
  graphics::segments(lower[ok], y, upper[ok], y)
  graphics::axis(2, at = y, labels = table$label[ok], las = 1)
  graphics::abline(v = 0.8, lty = 2)
  invisible(x)
}
