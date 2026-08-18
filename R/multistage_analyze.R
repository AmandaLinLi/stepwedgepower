#' Fit a cumulative A then A+B stepped-wedge analysis model
#'
#' Fits a binomial cluster-random-intercept model with separate indicators for
#' the delayed effect of A and the delayed incremental effect of B. The default
#' model is
#'
#' `cbind(events, n - events) ~ a_effective + b_effective + factor(period) +
#' (1 | cluster_id)`.
#'
#' The A coefficient compares A with control after the A wash-in period. The B
#' coefficient compares A+B with A after the B wash-in period. The total A+B
#' effect relative to control is the linear contrast `beta_A + beta_B` and its
#' standard error includes the covariance between the two estimates.
#'
#' @param data Aggregated cluster-period data, typically from
#'   [simulate_multistage_swcrt()].
#' @param formula Optional custom model formula. If supplied, the variable-name
#'   and structural arguments are ignored, but `term_a` and `term_b` must name
#'   the fitted coefficients representing A and incremental B.
#' @param outcome Length-two character vector `c(events, n)`.
#' @param intervention_a Name of the delayed A indicator.
#' @param intervention_b Name of the delayed B indicator.
#' @param term_a,term_b Coefficient names used to construct contrasts. By
#'   default they equal `intervention_a` and `intervention_b`.
#' @param period Name of the calendar-period column.
#' @param cluster Name of the cluster identifier.
#' @param sequence Name of the sequence column.
#' @param adjust_sequence Logical; include a fixed sequence effect.
#' @param period_effect One of `"categorical"`, `"linear"`, or `"none"`.
#' @param link Binomial link, `"logit"` or `"identity"`.
#' @param nAGQ Adaptive Gauss-Hermite quadrature points.
#' @param alpha Two-sided significance level used for confidence intervals and
#'   rejection indicators.
#' @param multiplicity Adjustment for the two component tests A and B. One of
#'   `"none"`, `"holm"`, or `"bonferroni"`. The total A+B contrast is reported
#'   separately and is not included in this adjustment family.
#'
#' @return A list containing the fitted model, a contrast table, raw and
#'   adjusted p-values, detailed fit status, convergence and singularity
#'   diagnostics, captured warnings, and a joint-success indicator for the A
#'   and B component tests. Fit status distinguishes hard GLMM errors,
#'   nonconvergence, converged fits with non-finite contrasts, and successful
#'   fits; singularity is reported separately.
#' @examples
#' \donttest{
#' design <- sw_multistage_design(
#'   clusters_per_sequence = rep(5, 4),
#'   a_start = c(2, 3, 4, 5), b_start = c(5, 7, 6, 8),
#'   n_periods = 8
#' )
#' assumptions <- sw_multistage_assumptions(
#'   baseline_prob = 0.15, treatment_or_a = 1.6,
#'   incremental_or_b = 1.4, delay_a = 1, delay_b = 1,
#'   icc = 0.05, n_per_cluster_period = 40
#' )
#' fit <- fit_multistage_model(
#'   simulate_multistage_swcrt(design, assumptions, seed = 1)
#' )
#' fit$contrasts
#' }
#' @export
fit_multistage_model <- function(
  data,
  formula = NULL,
  outcome = c("events", "n"),
  intervention_a = "a_effective",
  intervention_b = "b_effective",
  term_a = intervention_a,
  term_b = intervention_b,
  period = "period",
  cluster = "cluster_id",
  sequence = "sequence",
  adjust_sequence = FALSE,
  period_effect = c("categorical", "linear", "none"),
  link = c("logit", "identity"),
  nAGQ = 1,
  alpha = 0.05,
  multiplicity = c("holm", "bonferroni", "none")
) {
  period_effect <- match.arg(period_effect)
  link <- match.arg(link)
  multiplicity <- match.arg(multiplicity)
  if (length(alpha) != 1L || !is.finite(alpha) || alpha <= 0 || alpha >= 1) {
    stop("`alpha` must be one number strictly between 0 and 1.", call. = FALSE)
  }
  if (length(nAGQ) != 1L || !is.finite(nAGQ) || nAGQ < 0 ||
      nAGQ != round(nAGQ)) {
    stop("`nAGQ` must be one non-negative integer.", call. = FALSE)
  }

  if (length(outcome) != 2L) {
    stop("`outcome` must be c(events, n).", call. = FALSE)
  }
  events <- outcome[1]
  n_total <- outcome[2]
  .check_required_columns(
    data,
    c(events, n_total, intervention_a, intervention_b, period, cluster)
  )
  if (anyNA(data[[events]]) || anyNA(data[[n_total]]) ||
      any(data[[events]] < 0) || any(data[[n_total]] < 1) ||
      any(data[[events]] > data[[n_total]])) {
    stop("Aggregated outcomes must satisfy 0 <= events <= n and n >= 1.",
         call. = FALSE)
  }

  b_varies <- length(unique(data[[intervention_b]])) > 1L
  has_b <- b_varies && any(data[[intervention_b]] == 1L)

  if (is.null(formula)) {
    lhs <- sprintf("cbind(%s, %s - %s)", events, n_total, events)
    rhs <- intervention_a
    if (has_b) rhs <- c(rhs, intervention_b)
    if (period_effect == "categorical") {
      rhs <- c(rhs, sprintf("factor(%s)", period))
    } else if (period_effect == "linear") {
      rhs <- c(rhs, period)
    }
    if (isTRUE(adjust_sequence)) {
      .check_required_columns(data, sequence)
      if (length(unique(data[[sequence]])) > 1L) {
        rhs <- c(rhs, sprintf("factor(%s)", sequence))
      }
    }
    rhs <- c(rhs, sprintf("(1 | %s)", cluster))
    formula <- stats::as.formula(
      paste(lhs, "~", paste(rhs, collapse = " + "))
    )
  }

  captured_warnings <- character()
  fit_error <- NULL
  fit <- tryCatch(
    withCallingHandlers(
      lme4::glmer(
        formula = formula,
        family = stats::binomial(link = link),
        data = data,
        nAGQ = as.integer(nAGQ)
      ),
      warning = function(w) {
        captured_warnings <<- c(captured_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      fit_error <<- conditionMessage(e)
      NULL
    }
  )

  contrast_names <- c(
    "A_vs_control",
    if (has_b) c("AB_vs_A", "AB_vs_control")
  )
  empty_contrasts <- .empty_multistage_contrasts(contrast_names)

  if (is.null(fit)) {
    component_names <- c("A_vs_control", if (has_b) "AB_vs_A")
    return(list(
      fit = NULL,
      coefficients = NULL,
      formula = formula,
      contrasts = empty_contrasts,
      raw_p_values = stats::setNames(rep(NA_real_, length(contrast_names)),
                                     contrast_names),
      adjusted_p_values = stats::setNames(
        rep(NA_real_, length(component_names)), component_names
      ),
      reject_adjusted = stats::setNames(
        rep(NA, length(component_names)), component_names
      ),
      joint_success = NA,
      has_b = has_b,
      hard_error = TRUE,
      fit_status = "hard_glmm_error",
      converged = FALSE,
      convergence_messages = character(),
      optimizer_code = NA_character_,
      nonfinite_contrasts = contrast_names,
      singular = NA,
      usable = FALSE,
      successful = FALSE,
      warnings = unique(captured_warnings),
      error = fit_error,
      multiplicity = multiplicity,
      alpha = alpha
    ))
  }

  coefficients <- summary(fit)$coefficients
  convergence <- .extract_lme4_convergence(fit)
  convergence_messages <- convergence$messages
  converged <- convergence$converged
  singular <- tryCatch(lme4::isSingular(fit), error = function(e) NA)

  contrast_a <- .multistage_wald_contrast(
    fit, terms = term_a, contrast = "A_vs_control", alpha = alpha
  )
  contrasts <- contrast_a

  if (has_b) {
    contrast_b <- .multistage_wald_contrast(
      fit, terms = term_b, contrast = "AB_vs_A", alpha = alpha
    )
    contrast_total <- .multistage_wald_contrast(
      fit, terms = c(term_a, term_b),
      contrast = "AB_vs_control", alpha = alpha
    )
    contrasts <- rbind(contrasts, contrast_b, contrast_total)
  }
  rownames(contrasts) <- NULL

  raw_p <- stats::setNames(contrasts$p_value, contrasts$contrast)
  component_names <- c("A_vs_control", if (has_b) "AB_vs_A")
  component_p <- raw_p[component_names]
  adjusted_p <- rep(NA_real_, length(component_names))
  names(adjusted_p) <- component_names
  finite_component <- is.finite(component_p)
  if (any(finite_component)) {
    adjusted_p[finite_component] <- stats::p.adjust(
      component_p[finite_component], method = multiplicity
    )
  }
  contrasts$p_adjusted <- NA_real_
  component_rows <- match(component_names, contrasts$contrast)
  contrasts$p_adjusted[component_rows] <- adjusted_p
  contrasts$reject <- is.finite(contrasts$p_value) &
    contrasts$p_value < alpha
  contrasts$reject_adjusted <- NA
  contrasts$reject_adjusted[component_rows] <-
    is.finite(adjusted_p) & adjusted_p < alpha

  reject_adjusted <- stats::setNames(
    contrasts$reject_adjusted[component_rows],
    component_names
  )
  joint_success <- if (has_b && all(!is.na(reject_adjusted))) {
    all(reject_adjusted)
  } else {
    NA
  }

  nonfinite_contrasts <- names(raw_p)[!is.finite(raw_p)]
  fit_status <- .classify_analysis_fit(
    hard_error = FALSE,
    converged = converged,
    p_values = raw_p
  )
  usable <- identical(fit_status, "successful_fit")

  list(
    fit = fit,
    coefficients = coefficients,
    formula = formula,
    contrasts = contrasts,
    raw_p_values = raw_p,
    adjusted_p_values = adjusted_p,
    reject_adjusted = reject_adjusted,
    joint_success = joint_success,
    has_b = has_b,
    hard_error = FALSE,
    fit_status = fit_status,
    converged = converged,
    convergence_messages = convergence_messages,
    optimizer_code = convergence$optimizer_code,
    nonfinite_contrasts = nonfinite_contrasts,
    singular = singular,
    usable = usable,
    successful = usable,
    warnings = unique(c(
      captured_warnings, convergence_messages, convergence$advisory_messages
    )),
    error = fit_error,
    multiplicity = multiplicity,
    alpha = alpha
  )
}

.empty_multistage_contrasts <- function(contrast_names) {
  data.frame(
    contrast = contrast_names,
    estimate = NA_real_,
    std_error = NA_real_,
    z_value = NA_real_,
    p_value = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    p_adjusted = NA_real_,
    reject = NA,
    reject_adjusted = NA,
    stringsAsFactors = FALSE
  )
}

.multistage_wald_contrast <- function(fit, terms, contrast, alpha) {
  beta <- lme4::fixef(fit)
  covariance <- as.matrix(stats::vcov(fit))
  missing_terms <- setdiff(terms, names(beta))
  if (length(missing_terms)) {
    return(data.frame(
      contrast = contrast,
      estimate = NA_real_, std_error = NA_real_, z_value = NA_real_,
      p_value = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  linear <- stats::setNames(rep(0, length(beta)), names(beta))
  linear[terms] <- linear[terms] + 1
  estimate <- sum(linear * beta)
  variance <- as.numeric(t(linear) %*% covariance %*% linear)
  if (!is.finite(variance) || variance <= 0) {
    return(data.frame(
      contrast = contrast,
      estimate = estimate, std_error = NA_real_, z_value = NA_real_,
      p_value = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      stringsAsFactors = FALSE
    ))
  }

  std_error <- sqrt(variance)
  z_value <- estimate / std_error
  p_value <- 2 * stats::pnorm(abs(z_value), lower.tail = FALSE)
  critical <- stats::qnorm(1 - alpha / 2)
  data.frame(
    contrast = contrast,
    estimate = estimate,
    std_error = std_error,
    z_value = z_value,
    p_value = p_value,
    conf_low = estimate - critical * std_error,
    conf_high = estimate + critical * std_error,
    stringsAsFactors = FALSE
  )
}
