#' Fit a component-based stepped-wedge analysis model
#'
#' Fits a binomial cluster-random-intercept model with separate A and B effect
#' weights and, optionally, an A-by-B interaction weight. The default model is
#'
#' `cbind(events, n - events) ~ a_effect_weight + b_effect_weight +
#' ab_effect_weight + factor(period) + (1 | cluster_id)`.
#'
#' The standard contrasts are A versus Control, B versus Control, A+B versus
#' Control, A+B versus A, A+B versus B, and the A-by-B interaction. All contrast
#' standard errors use the complete fitted covariance matrix.
#'
#' @param data Aggregated cluster-period data, normally from
#'   [simulate_component_swcrt()].
#' @param formula Optional custom mixed-model formula.
#' @param outcome Length-two character vector `c(events, n)`.
#' @param weight_a,weight_b,weight_ab Predictor names for the component effect
#'   weights.
#' @param term_a,term_b,term_ab Coefficient names used in the standard
#'   contrasts. These are especially important with a custom formula.
#' @param include_interaction Logical; fit and test an A-by-B interaction.
#' @param contrasts Standard contrasts to report. `NULL` reports all available
#'   contrasts.
#' @param multiplicity One of `"none"`, `"holm"`, or `"bonferroni"`.
#' @param multiplicity_family Contrasts included in the multiplicity family.
#'   The default is A versus Control, B versus Control, and the interaction when
#'   fitted. Adjusted p-values are reported only for this family.
#' @param period,cluster,sequence Column names.
#' @param adjust_sequence Logical; include a fixed sequence effect.
#' @param period_effect One of `"categorical"`, `"linear"`, or `"none"`.
#' @param link Binomial link, `"logit"` or `"identity"`.
#' @param nAGQ Adaptive Gauss-Hermite quadrature points.
#' @param alpha Two-sided significance level.
#'
#' @return A list containing the fitted model, contrast table, adjusted tests,
#'   and model-fitting diagnostics.
#' @examples
#' \donttest{
#' state <- rbind(
#'   S1 = c(0, 1, 1, 3, 3, 2),
#'   S2 = c(0, 0, 2, 2, 3, 3),
#'   S3 = c(0, 0, 1, 1, 3, 3),
#'   S4 = c(0, 0, 2, 3, 3, 3)
#' )
#' d <- sw_component_design(rep(5, 4), state = state)
#' a <- sw_component_assumptions(
#'   baseline_prob = 0.15, treatment_or_a = 1.6,
#'   treatment_or_b = 1.5, interaction_or = 1.15,
#'   icc = 0.05, n_per_cluster_period = 40
#' )
#' fit <- fit_component_model(simulate_component_swcrt(d, a, seed = 1))
#' fit$contrasts
#' }
#' @export
fit_component_model <- function(
  data,
  formula = NULL,
  outcome = c("events", "n"),
  weight_a = "a_effect_weight",
  weight_b = "b_effect_weight",
  weight_ab = "ab_effect_weight",
  term_a = weight_a,
  term_b = weight_b,
  term_ab = weight_ab,
  include_interaction = TRUE,
  contrasts = NULL,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL,
  period = "period",
  cluster = "cluster_id",
  sequence = "sequence",
  adjust_sequence = FALSE,
  period_effect = c("categorical", "linear", "none"),
  link = c("logit", "identity"),
  nAGQ = 1,
  alpha = 0.05
) {
  period_effect <- match.arg(period_effect)
  link <- match.arg(link)
  multiplicity <- match.arg(multiplicity)
  include_interaction <- isTRUE(include_interaction)
  contrasts <- .validate_component_contrasts(contrasts, include_interaction)

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
  .check_required_columns(data, c(events, n_total))
  if (anyNA(data[[events]]) || anyNA(data[[n_total]]) ||
      any(data[[events]] < 0) || any(data[[n_total]] < 1) ||
      any(data[[events]] > data[[n_total]])) {
    stop("Aggregated outcomes must satisfy 0 <= events <= n and n >= 1.",
         call. = FALSE)
  }

  if (is.null(multiplicity_family)) {
    default_family <- c("A_vs_control", "B_vs_control")
    if (include_interaction) default_family <- c(default_family, "interaction")
    multiplicity_family <- intersect(default_family, contrasts)
  }
  if (!is.character(multiplicity_family) || anyNA(multiplicity_family)) {
    stop("`multiplicity_family` must be a character vector.", call. = FALSE)
  }
  if (length(setdiff(multiplicity_family, contrasts))) {
    stop("Every member of `multiplicity_family` must also be in `contrasts`.",
         call. = FALSE)
  }
  multiplicity_family <- unique(multiplicity_family)

  if (is.null(formula)) {
    required <- c(weight_a, weight_b, period, cluster)
    if (include_interaction) required <- c(required, weight_ab)
    .check_required_columns(data, required)

    lhs <- sprintf("cbind(%s, %s - %s)", events, n_total, events)
    rhs <- c(weight_a, weight_b)
    if (include_interaction) rhs <- c(rhs, weight_ab)
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

  labels <- .component_contrast_labels()
  empty <- .empty_component_contrasts(contrasts)
  if (is.null(fit)) {
    return(list(
      fit = NULL,
      coefficients = NULL,
      formula = formula,
      contrasts = empty,
      raw_p_values = stats::setNames(rep(NA_real_, length(contrasts)), contrasts),
      adjusted_p_values = stats::setNames(
        rep(NA_real_, length(multiplicity_family)), multiplicity_family
      ),
      reject_adjusted = stats::setNames(
        rep(NA, length(multiplicity_family)), multiplicity_family
      ),
      joint_success = NA,
      converged = FALSE,
      singular = NA,
      usable = FALSE,
      warnings = unique(captured_warnings),
      error = fit_error,
      multiplicity = multiplicity,
      multiplicity_family = multiplicity_family,
      alpha = alpha,
      include_interaction = include_interaction
    ))
  }

  coefficients <- summary(fit)$coefficients
  convergence_messages <- fit@optinfo$conv$lme4$messages
  converged <- is.null(convergence_messages)
  singular <- tryCatch(lme4::isSingular(fit), error = function(e) NA)

  standard_matrix <- .component_contrast_matrix(include_interaction)
  term_map <- c(a = term_a, b = term_b, ab = term_ab)
  contrast_rows <- lapply(contrasts, function(name) {
    component_weights <- standard_matrix[name, ]
    term_weights <- stats::setNames(numeric(0), character(0))
    for (component in names(component_weights)) {
      if (component_weights[[component]] != 0) {
        term <- term_map[[component]]
        term_weights[[term]] <- component_weights[[component]]
      }
    }
    .component_wald_contrast(
      fit = fit,
      term_weights = term_weights,
      contrast = name,
      label = unname(labels[[name]]),
      alpha = alpha
    )
  })
  contrast_table <- do.call(rbind, contrast_rows)
  rownames(contrast_table) <- NULL

  raw_p <- stats::setNames(contrast_table$p_value, contrast_table$contrast)
  adjusted_p <- stats::setNames(
    rep(NA_real_, length(multiplicity_family)), multiplicity_family
  )
  if (length(multiplicity_family)) {
    family_p <- raw_p[multiplicity_family]
    finite <- is.finite(family_p)
    if (any(finite)) {
      adjusted_p[finite] <- stats::p.adjust(
        family_p[finite], method = multiplicity, n = length(family_p)
      )
    }
  }

  contrast_table$p_adjusted <- NA_real_
  contrast_table$reject <- is.finite(contrast_table$p_value) &
    contrast_table$p_value < alpha
  contrast_table$reject_adjusted <- NA
  if (length(multiplicity_family)) {
    family_rows <- match(multiplicity_family, contrast_table$contrast)
    contrast_table$p_adjusted[family_rows] <- adjusted_p
    contrast_table$reject_adjusted[family_rows] <-
      is.finite(adjusted_p) & adjusted_p < alpha
  }

  reject_adjusted <- stats::setNames(
    if (length(multiplicity_family))
      contrast_table$reject_adjusted[match(
        multiplicity_family, contrast_table$contrast
      )]
    else logical(0),
    multiplicity_family
  )
  joint_success <- if (length(reject_adjusted) &&
                       all(!is.na(reject_adjusted))) {
    all(reject_adjusted)
  } else {
    NA
  }
  usable <- converged && all(is.finite(raw_p))

  list(
    fit = fit,
    coefficients = coefficients,
    formula = formula,
    contrasts = contrast_table,
    raw_p_values = raw_p,
    adjusted_p_values = adjusted_p,
    reject_adjusted = reject_adjusted,
    joint_success = joint_success,
    converged = converged,
    singular = singular,
    usable = usable,
    warnings = unique(c(captured_warnings, convergence_messages)),
    error = fit_error,
    multiplicity = multiplicity,
    multiplicity_family = multiplicity_family,
    alpha = alpha,
    include_interaction = include_interaction
  )
}

.empty_component_contrasts <- function(contrast_names) {
  labels <- .component_contrast_labels()
  data.frame(
    contrast = contrast_names,
    label = unname(labels[contrast_names]),
    estimate = NA_real_,
    std_error = NA_real_,
    z_value = NA_real_,
    p_value = NA_real_,
    conf_low = NA_real_,
    conf_high = NA_real_,
    odds_ratio = NA_real_,
    p_adjusted = NA_real_,
    reject = NA,
    reject_adjusted = NA,
    stringsAsFactors = FALSE
  )
}

.component_wald_contrast <- function(
  fit,
  term_weights,
  contrast,
  label,
  alpha
) {
  beta <- lme4::fixef(fit)
  covariance <- as.matrix(stats::vcov(fit))
  missing_terms <- setdiff(names(term_weights), names(beta))
  if (length(missing_terms)) {
    return(data.frame(
      contrast = contrast, label = label,
      estimate = NA_real_, std_error = NA_real_, z_value = NA_real_,
      p_value = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      odds_ratio = NA_real_, stringsAsFactors = FALSE
    ))
  }

  linear <- stats::setNames(rep(0, length(beta)), names(beta))
  for (term in names(term_weights)) {
    linear[[term]] <- linear[[term]] + term_weights[[term]]
  }
  estimate <- sum(linear * beta)
  variance <- as.numeric(t(linear) %*% covariance %*% linear)
  if (!is.finite(variance) || variance <= 0) {
    return(data.frame(
      contrast = contrast, label = label,
      estimate = estimate, std_error = NA_real_, z_value = NA_real_,
      p_value = NA_real_, conf_low = NA_real_, conf_high = NA_real_,
      odds_ratio = exp(estimate), stringsAsFactors = FALSE
    ))
  }

  std_error <- sqrt(variance)
  z_value <- estimate / std_error
  p_value <- 2 * stats::pnorm(abs(z_value), lower.tail = FALSE)
  critical <- stats::qnorm(1 - alpha / 2)
  data.frame(
    contrast = contrast,
    label = label,
    estimate = estimate,
    std_error = std_error,
    z_value = z_value,
    p_value = p_value,
    conf_low = estimate - critical * std_error,
    conf_high = estimate + critical * std_error,
    odds_ratio = exp(estimate),
    stringsAsFactors = FALSE
  )
}
