#' Estimate power for a component-based stepped-wedge design
#'
#' Repeatedly simulates a Control/A/B/A+B trial and fits the planned component
#' mixed model. The output distinguishes conditional power among evaluable fits
#' from failure-aware power, which counts unusable analyses as unsuccessful
#' trials. It also reports estimation bias, standard-error calibration,
#' confidence-interval coverage, convergence, singularity, and Monte Carlo
#' uncertainty for every requested contrast. Fit diagnostics separately report
#' hard GLMM errors, nonconverged fits, converged fits with non-finite requested
#' contrasts, singular fits, and successful fits. Singularity is a non-exclusive
#' flag and does not automatically make a fit unsuccessful.
#'
#' @param design An [sw_component_design()] object.
#' @param assumptions An [sw_component_assumptions()] object.
#' @param nsim Number of simulated trials.
#' @param alpha Two-sided significance level.
#' @param contrasts Standard contrasts to evaluate.
#' @param include_interaction Logical; fit the interaction term.
#' @param multiplicity Multiplicity adjustment for `multiplicity_family`.
#' @param multiplicity_family Contrasts forming the multiplicity family.
#' @param fit_link Binomial link used by [fit_component_model()].
#' @param nAGQ Quadrature points for the analysis model.
#' @param analysis_args Optional named list of additional arguments passed to
#'   [fit_component_model()].
#' @param n_cores Number of processor cores. Reproducibility under a fixed seed
#'   does not depend on this value.
#' @param seed Optional random seed.
#' @param check_design Logical; run [audit_component_design()] first.
#' @param warn_on_design Logical; warn when the audit returns cautions.
#'
#' @return An object of class `"sw_component_power"`. In addition to the
#'   power and estimation tables, `fit_diagnostics` gives aggregate counts and
#'   rates for the five fit categories, and `replicate_diagnostics` gives one
#'   row per simulation with error and convergence messages.
#' @examples
#' \donttest{
#' state <- rbind(
#'   S1 = c(0, 1, 1, 3, 3, 2),
#'   S2 = c(0, 0, 2, 2, 3, 3),
#'   S3 = c(0, 0, 1, 1, 3, 3),
#'   S4 = c(0, 0, 2, 3, 3, 3)
#' )
#' d <- sw_component_design(rep(4, 4), state = state)
#' a <- sw_component_assumptions(
#'   baseline_prob = 0.15, treatment_or_a = 1.5,
#'   treatment_or_b = 1.4, interaction_or = 1.1,
#'   carryover_periods_a = 1, carryover_weights_a = 0.5,
#'   icc = 0.05, n_per_cluster_period = 30
#' )
#' p <- power_component_swcrt(d, a, nsim = 20, seed = 1,
#'                            warn_on_design = FALSE)
#' p
#' }
#' @export
power_component_swcrt <- function(
  design,
  assumptions,
  nsim = 1000,
  alpha = 0.05,
  contrasts = NULL,
  include_interaction = TRUE,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL,
  fit_link = c("logit", "identity"),
  nAGQ = 1,
  analysis_args = list(),
  n_cores = 1,
  seed = NULL,
  check_design = TRUE,
  warn_on_design = TRUE
) {
  if (!inherits(design, "sw_component_design")) {
    stop("`design` must be an sw_component_design object.", call. = FALSE)
  }
  if (!inherits(assumptions, "sw_component_assumptions")) {
    stop("`assumptions` must be an sw_component_assumptions object.",
         call. = FALSE)
  }
  if (length(nsim) != 1L || !is.finite(nsim) || nsim < 1 ||
      nsim != round(nsim)) {
    stop("`nsim` must be one positive integer.", call. = FALSE)
  }
  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be one number strictly between 0 and 1.", call. = FALSE)
  }
  if (length(nAGQ) != 1L || !is.finite(nAGQ) || nAGQ < 0 ||
      nAGQ != round(nAGQ)) {
    stop("`nAGQ` must be one non-negative integer.", call. = FALSE)
  }
  if (length(n_cores) != 1L || !is.finite(n_cores) || n_cores < 1 ||
      n_cores != round(n_cores)) {
    stop("`n_cores` must be one positive integer.", call. = FALSE)
  }
  nsim <- as.integer(nsim)
  n_cores <- as.integer(n_cores)
  multiplicity <- match.arg(multiplicity)
  fit_link <- match.arg(fit_link)
  include_interaction <- isTRUE(include_interaction)
  if (include_interaction && assumptions$interaction_mode == "none") {
    stop(
      "Set `include_interaction = FALSE` when `interaction_mode = \"none\"`.",
      call. = FALSE
    )
  }
  if (!include_interaction &&
      abs(assumptions$interaction_effect) > sqrt(.Machine$double.eps)) {
    stop(
      "A nonzero interaction is present in the data-generating model. ",
      "Set `include_interaction = TRUE` or set `interaction_or = 1`.",
      call. = FALSE
    )
  }
  contrasts <- .validate_component_contrasts(contrasts, include_interaction)

  if (is.null(multiplicity_family)) {
    multiplicity_family <- intersect(
      c("A_vs_control", "B_vs_control",
        if (include_interaction) "interaction"),
      contrasts
    )
  }
  if (!is.character(multiplicity_family) || anyNA(multiplicity_family) ||
      length(setdiff(multiplicity_family, contrasts))) {
    stop("`multiplicity_family` must contain only requested contrasts.",
         call. = FALSE)
  }
  multiplicity_family <- unique(multiplicity_family)

  if (!is.list(analysis_args)) {
    stop("`analysis_args` must be a named list.", call. = FALSE)
  }
  if (length(analysis_args) &&
      (is.null(names(analysis_args)) || any(!nzchar(names(analysis_args))))) {
    stop("Every element of `analysis_args` must be named.", call. = FALSE)
  }
  protected_arguments <- c(
    "contrasts", "include_interaction", "multiplicity",
    "multiplicity_family", "link", "nAGQ", "alpha"
  )
  conflicting_arguments <- intersect(names(analysis_args), protected_arguments)
  if (length(conflicting_arguments)) {
    stop(
      "Supply ", paste0("`", conflicting_arguments, "`", collapse = ", "),
      " directly to `power_component_swcrt()`, not through `analysis_args`.",
      call. = FALSE
    )
  }

  audit <- if (isTRUE(check_design)) {
    audit_component_design(
      design = design,
      assumptions = assumptions,
      contrasts = contrasts,
      include_interaction = include_interaction,
      adjust_sequence = isTRUE(analysis_args$adjust_sequence),
      period_effect = if (!is.null(analysis_args$period_effect))
        analysis_args$period_effect else "categorical"
    )
  } else {
    structure(list(messages = character(), all_requested_estimable = NA),
              class = "sw_component_audit")
  }
  if (isTRUE(warn_on_design) && length(audit$messages)) {
    warning(
      "Component-design audit cautions:\n- ",
      paste(audit$messages, collapse = "\n- "),
      call. = FALSE
    )
  }

  fit_defaults <- list(
    contrasts = contrasts,
    include_interaction = include_interaction,
    multiplicity = multiplicity,
    multiplicity_family = multiplicity_family,
    link = fit_link,
    nAGQ = nAGQ,
    alpha = alpha
  )
  fit_arguments <- utils::modifyList(fit_defaults, analysis_args)

  one_rep <- function(i) {
    simulated <- simulate_component_swcrt(design, assumptions)
    fit <- do.call(
      fit_component_model,
      c(list(data = simulated), fit_arguments)
    )
    contrast_table <- fit$contrasts
    estimates <- stats::setNames(
      contrast_table$estimate, contrast_table$contrast
    )
    standard_errors <- stats::setNames(
      contrast_table$std_error, contrast_table$contrast
    )
    list(
      raw_p = fit$raw_p_values,
      adjusted_p = fit$adjusted_p_values,
      estimate = estimates,
      se = standard_errors,
      hard_error = isTRUE(fit$hard_error),
      fit_status = fit$fit_status,
      converged = isTRUE(fit$converged),
      singular = if (is.na(fit$singular)) NA else isTRUE(fit$singular),
      has_warning = length(fit$warnings) > 0L,
      nonfinite_contrasts = paste(fit$nonfinite_contrasts, collapse = ", "),
      optimizer_code = fit$optimizer_code,
      error_message = .collapse_diagnostic_messages(fit$error),
      convergence_message =
        .collapse_diagnostic_messages(fit$convergence_messages),
      joint_success = fit$joint_success
    )
  }

  replicates <- .simulate_replicates(
    one_rep, nsim, n_cores = n_cores, seed = seed
  )
  raw_p <- .component_bind_numeric(replicates, "raw_p", contrasts)
  adjusted_p <- .component_bind_numeric(
    replicates, "adjusted_p", multiplicity_family
  )
  estimates <- .component_bind_numeric(replicates, "estimate", contrasts)
  standard_errors <- .component_bind_numeric(replicates, "se", contrasts)
  hard_error <- vapply(replicates, function(x) x$hard_error, logical(1))
  fit_status <- vapply(replicates, function(x) x$fit_status, character(1))
  converged <- vapply(replicates, function(x) x$converged, logical(1))
  singular <- vapply(replicates, function(x) x$singular, logical(1))
  has_warning <- vapply(replicates, function(x) x$has_warning, logical(1))
  nonfinite_contrasts <- vapply(
    replicates, function(x) x$nonfinite_contrasts, character(1)
  )
  optimizer_code <- vapply(
    replicates,
    function(x) if (is.null(x$optimizer_code) || is.na(x$optimizer_code)) {
      NA_character_
    } else {
      as.character(x$optimizer_code)
    },
    character(1)
  )
  error_message <- vapply(
    replicates,
    function(x) if (is.null(x$error_message)) NA_character_ else x$error_message,
    character(1)
  )
  convergence_message <- vapply(
    replicates,
    function(x) if (is.null(x$convergence_message)) {
      NA_character_
    } else {
      x$convergence_message
    },
    character(1)
  )
  joint_success <- vapply(
    replicates,
    function(x) if (is.null(x$joint_success)) NA else x$joint_success,
    logical(1)
  )

  labels <- .component_contrast_labels()
  raw_rows <- lapply(contrasts, function(name) {
    .summarize_component_test(
      p_values = raw_p[, name],
      converged = converged,
      hard_error = hard_error,
      singular = singular,
      nsim = nsim,
      alpha = alpha,
      test = paste0(name, "_raw"),
      contrast = name,
      label = unname(labels[[name]]),
      adjustment = "none"
    )
  })

  adjusted_rows <- lapply(multiplicity_family, function(name) {
    .summarize_component_test(
      p_values = adjusted_p[, name],
      converged = converged,
      hard_error = hard_error,
      singular = singular,
      nsim = nsim,
      alpha = alpha,
      test = paste0(name, "_adjusted"),
      contrast = name,
      label = paste0(unname(labels[[name]]), " (", multiplicity, " adjusted)"),
      adjustment = multiplicity
    )
  })

  joint_rows <- list()
  if (length(multiplicity_family)) {
    joint_rows <- list(.summarize_component_indicator(
      indicator = joint_success,
      converged = converged,
      hard_error = hard_error,
      singular = singular,
      nsim = nsim,
      alpha = alpha,
      test = "joint_family_success",
      contrast = "joint_family_success",
      label = paste0(
        "Joint success: ",
        paste(unname(labels[multiplicity_family]), collapse = " and "),
        " (", multiplicity, ")"
      ),
      adjustment = multiplicity
    ))
  }
  power_table <- do.call(rbind, c(raw_rows, adjusted_rows, joint_rows))
  rownames(power_table) <- NULL

  true_effects <- .component_true_effects(assumptions, include_interaction)
  estimation_rows <- lapply(contrasts, function(name) {
    .summarize_component_estimation(
      estimates = estimates[, name],
      standard_errors = standard_errors[, name],
      converged = converged,
      true_effect = true_effects[[name]],
      alpha = alpha,
      contrast = name,
      label = unname(labels[[name]])
    )
  })
  estimation_table <- do.call(rbind, estimation_rows)
  rownames(estimation_table) <- NULL

  replicate_diagnostics <- .make_replicate_diagnostics(
    fit_status = fit_status,
    singular = singular,
    has_warning = has_warning,
    nonfinite_contrasts = nonfinite_contrasts,
    error_message = error_message,
    convergence_message = convergence_message,
    optimizer_code = optimizer_code
  )
  fit_diagnostics <- .summarize_fit_diagnostics(replicate_diagnostics)
  fit_success <- replicate_diagnostics$successful_fit

  structure(
    list(
      power_table = power_table,
      estimation_table = estimation_table,
      fit_diagnostics = fit_diagnostics,
      replicate_diagnostics = replicate_diagnostics,
      alpha = alpha,
      nsim = nsim,
      contrasts = contrasts,
      include_interaction = include_interaction,
      multiplicity = multiplicity,
      multiplicity_family = multiplicity_family,
      fit_success_rate = mean(fit_success),
      failure_rate = mean(!fit_success),
      convergence_rate = mean(converged),
      singular_rate = if (any(converged & !is.na(singular))) {
        mean(singular[converged], na.rm = TRUE)
      } else {
        NA_real_
      },
      singular_rate_all = mean(singular %in% TRUE),
      hard_glmm_error_rate = mean(hard_error),
      nonconverged_fit_rate = mean(fit_status == "nonconverged_fit"),
      nonfinite_contrast_fit_rate = mean(
        fit_status == "converged_nonfinite_contrast"
      ),
      warning_rate = mean(has_warning),
      raw_p_values = raw_p,
      adjusted_p_values = adjusted_p,
      estimated_effects = estimates,
      estimated_ses = standard_errors,
      fit_status = fit_status,
      hard_glmm_error = hard_error,
      convergence_status = converged,
      singular_fit = singular,
      error_messages = error_message,
      convergence_messages = convergence_message,
      nonfinite_contrasts = nonfinite_contrasts,
      joint_success = joint_success,
      design_audit = audit,
      design = design,
      assumptions = assumptions,
      global_null = FALSE,
      call = match.call()
    ),
    class = "sw_component_power"
  )
}

.component_bind_numeric <- function(replicates, field, names_expected) {
  out <- matrix(
    NA_real_, nrow = length(replicates), ncol = length(names_expected),
    dimnames = list(NULL, names_expected)
  )
  if (!length(names_expected)) return(out)
  for (i in seq_along(replicates)) {
    value <- replicates[[i]][[field]]
    if (is.null(value)) next
    common <- intersect(names_expected, names(value))
    if (length(common)) out[i, common] <- as.numeric(value[common])
  }
  out
}

.summarize_component_test <- function(
  p_values,
  converged,
  hard_error,
  singular,
  nsim,
  alpha,
  test,
  contrast,
  label,
  adjustment
) {
  diagnostics <- .analysis_evaluability_counts(
    finite_result = is.finite(p_values),
    converged = converged,
    hard_error = hard_error,
    singular = singular
  )
  evaluable <- diagnostics$successful_fit
  reject <- evaluable & p_values < alpha
  .summarize_component_rejection(
    reject, evaluable, diagnostics, nsim, alpha,
    test, contrast, label, adjustment
  )
}

.summarize_component_indicator <- function(
  indicator,
  converged,
  hard_error,
  singular,
  nsim,
  alpha,
  test,
  contrast,
  label,
  adjustment
) {
  diagnostics <- .analysis_evaluability_counts(
    finite_result = !is.na(indicator),
    converged = converged,
    hard_error = hard_error,
    singular = singular
  )
  evaluable <- diagnostics$successful_fit
  reject <- evaluable & indicator
  .summarize_component_rejection(
    reject, evaluable, diagnostics, nsim, alpha,
    test, contrast, label, adjustment
  )
}

.summarize_component_rejection <- function(
  reject,
  evaluable,
  diagnostics,
  nsim,
  alpha,
  test,
  contrast,
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
    contrast = contrast,
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
    n_hard_glmm_error = diagnostics$n_hard_glmm_error,
    n_nonconverged_fit = diagnostics$n_nonconverged_fit,
    n_converged_nonfinite_contrast =
      diagnostics$n_converged_nonfinite_contrast,
    n_singular_fit = diagnostics$n_singular_fit,
    n_singular_evaluable_fit = diagnostics$n_singular_successful_fit,
    n_successful_fit = diagnostics$n_successful_fit,
    nsim = nsim,
    stringsAsFactors = FALSE
  )
}

.summarize_component_estimation <- function(
  estimates,
  standard_errors,
  converged,
  true_effect,
  alpha,
  contrast,
  label
) {
  usable <- converged & is.finite(estimates) & is.finite(standard_errors) &
    standard_errors > 0
  n_usable <- sum(usable)
  if (!n_usable) {
    return(data.frame(
      contrast = contrast,
      label = label,
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

  data.frame(
    contrast = contrast,
    label = label,
    true_effect = true_effect,
    true_odds_ratio = exp(true_effect),
    mean_estimate = mean(estimate),
    bias = mean(estimate) - true_effect,
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

#' Estimate global-null type I error for a component design
#'
#' Sets the A, B, and interaction effects to zero while preserving the schedule,
#' carryover structure, ICC, secular trend, sample size, and planned analysis.
#'
#' @inheritParams power_component_swcrt
#' @return An `sw_component_power` object evaluated under the global null.
#' @examples
#' \donttest{
#' state <- rbind(
#'   S1 = c(0, 1, 1, 3), S2 = c(0, 2, 2, 3),
#'   S3 = c(0, 0, 1, 3), S4 = c(0, 0, 2, 3)
#' )
#' d <- sw_component_design(rep(3, 4), state = state)
#' a <- sw_component_assumptions(
#'   baseline_prob = 0.15, treatment_or_a = 1.4,
#'   treatment_or_b = 1.3, interaction_or = 1.1, icc = 0.05
#' )
#' type1_component_swcrt(d, a, nsim = 20, seed = 1,
#'                       warn_on_design = FALSE)
#' }
#' @export
type1_component_swcrt <- function(
  design,
  assumptions,
  nsim = 1000,
  alpha = 0.05,
  contrasts = NULL,
  include_interaction = TRUE,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL,
  fit_link = c("logit", "identity"),
  nAGQ = 1,
  analysis_args = list(),
  n_cores = 1,
  seed = NULL,
  check_design = TRUE,
  warn_on_design = TRUE
) {
  if (!inherits(assumptions, "sw_component_assumptions")) {
    stop("`assumptions` must be an sw_component_assumptions object.",
         call. = FALSE)
  }
  null <- assumptions
  null$treatment_effect_a <- 0
  null$treatment_or_a <- 1
  null$treatment_effect_b <- 0
  null$treatment_or_b <- 1
  null$interaction_effect <- 0
  null$interaction_or <- 1

  out <- power_component_swcrt(
    design = design,
    assumptions = null,
    nsim = nsim,
    alpha = alpha,
    contrasts = contrasts,
    include_interaction = include_interaction,
    multiplicity = match.arg(multiplicity),
    multiplicity_family = multiplicity_family,
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

#' Methods for component-based power objects
#'
#' @param x,object An `sw_component_power` object.
#' @param type Plot `"conditional"` or `"failure-aware"` rejection rates.
#' @param ... Additional graphical arguments for `plot`; otherwise unused.
#' @name sw_component_power
NULL

#' @rdname sw_component_power
#' @export
print.sw_component_power <- function(x, ...) {
  cat("<sw_component_power>\n")
  display <- x$power_table[, c(
    "label", "conditional_power", "failure_aware_power",
    "n_evaluable", "n_failed"
  ), drop = FALSE]
  names(display) <- c(
    "test", "conditional", "failure_aware", "evaluable", "non_evaluable"
  )
  print(display, row.names = FALSE, digits = 3)
  cat("\nFit diagnostics\n")
  diagnostic_display <- x$fit_diagnostics[, c(
    "label", "count", "rate"
  ), drop = FALSE]
  names(diagnostic_display) <- c("category", "count", "rate")
  print(diagnostic_display, row.names = FALSE, digits = 3)
  cat(
    "  Note: singular fit is a non-exclusive flag and may overlap with ",
    "successful fit.\n",
    sep = ""
  )
  invisible(x)
}

#' @rdname sw_component_power
#' @export
summary.sw_component_power <- function(object, ...) {
  cat("Component-based stepped-wedge operating characteristics\n")
  cat("-------------------------------------------------------\n")
  print.sw_component_power(object)
  cat("\nEstimation diagnostics (log-odds scale)\n")
  print(object$estimation_table, row.names = FALSE, digits = 3)
  if (length(object$design_audit$messages)) {
    cat("\nDesign-audit cautions\n")
    cat(paste0("- ", object$design_audit$messages, collapse = "\n"), "\n")
  }
  invisible(object)
}

#' @rdname sw_component_power
#' @export
plot.sw_component_power <- function(
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
