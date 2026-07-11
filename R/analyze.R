#' Fit a stepped-wedge analysis model
#'
#' Fits a binomial random-intercept model to aggregated cluster-period data. The
#' model is fully configurable through variable names, or a user-supplied
#' `formula`, and does not depend on any application-specific column names.
#'
#' The default model is treatment plus a period effect plus an optional sequence
#' adjustment plus a cluster random intercept:
#' `cbind(events, n - events) ~ intervention + factor(period) +
#'   factor(sequence) + (1 | cluster_id)`.
#'
#' @param data Aggregated cluster-period data (e.g. from [simulate_swcrt()]).
#' @param formula Optional model formula. If supplied, the variable-name and
#'   structural arguments below are ignored.
#' @param outcome Length-2 character vector `c(events, n)`.
#' @param treatment Name of the 0/1 treatment column.
#' @param period Name of the period column.
#' @param cluster Name of the cluster identifier (random-effect grouping) column.
#' @param sequence Name of the sequence column (used when `adjust_sequence`).
#' @param adjust_sequence Logical; include a fixed sequence adjustment.
#' @param period_effect One of `"categorical"`, `"linear"`, or `"none"`.
#' @param link Binomial link, `"logit"` or `"identity"`.
#' @param nAGQ Number of adaptive Gauss-Hermite quadrature points.
#'
#' @return A list with the fitted model, the treatment coefficient row, the
#'   treatment estimate and standard error, the p-value, and
#'   convergence/singularity flags. On fit failure, `fit` is `NULL` and numeric
#'   fields are `NA`.
#' @examples
#' design <- sw_design(clusters_per_sequence = c(6, 6, 6, 6),
#'                     crossover_period = c(2, 3, 4, 5), n_periods = 5)
#' a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.03,
#'                     n_per_cluster_period = 30)
#' fit <- fit_stepwedge_model(simulate_swcrt(design, a, seed = 1))
#' fit$p_value
#' @export
fit_stepwedge_model <- function(
  data,
  formula = NULL,
  outcome = c("events", "n"),
  treatment = "intervention",
  period = "period",
  cluster = "cluster_id",
  sequence = "sequence",
  adjust_sequence = TRUE,
  period_effect = c("categorical", "linear", "none"),
  link = c("logit", "identity"),
  nAGQ = 1
) {
  period_effect <- match.arg(period_effect)
  link <- match.arg(link)

  if (is.null(formula)) {
    if (length(outcome) != 2L) stop("`outcome` must be c(events, n).", call. = FALSE)
    events <- outcome[1]; n <- outcome[2]
    .check_required_columns(data, c(events, n, treatment, period, cluster))

    lhs <- sprintf("cbind(%s, %s - %s)", events, n, events)
    rhs_terms <- treatment
    if (period_effect == "categorical") {
      rhs_terms <- c(rhs_terms, sprintf("factor(%s)", period))
    } else if (period_effect == "linear") {
      rhs_terms <- c(rhs_terms, period)
    }
    if (isTRUE(adjust_sequence)) {
      .check_required_columns(data, sequence)
      if (length(unique(data[[sequence]])) > 1L) {
        rhs_terms <- c(rhs_terms, sprintf("factor(%s)", sequence))
      }
    }
    rhs_terms <- c(rhs_terms, sprintf("(1 | %s)", cluster))
    formula <- stats::as.formula(paste(lhs, "~", paste(rhs_terms, collapse = " + ")))
    treatment_term <- treatment
  } else {
    treatment_term <- treatment
  }

  fit <- tryCatch(
    suppressWarnings(
      lme4::glmer(formula, family = stats::binomial(link = link),
                  data = data, nAGQ = nAGQ)
    ),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(list(fit = NULL, coefficients = NULL, treatment_row = NULL,
                estimate = NA_real_, std_error = NA_real_, p_value = NA_real_,
                converged = FALSE, singular = NA))
  }

  coefs <- summary(fit)$coefficients
  row_name <- if (treatment_term %in% rownames(coefs)) treatment_term else NA
  if (is.na(row_name)) {
    cand <- setdiff(rownames(coefs), "(Intercept)")
    row_name <- if (length(cand)) cand[1] else NA
  }

  est <- se <- pval <- NA_real_
  if (!is.na(row_name)) {
    est <- unname(coefs[row_name, "Estimate"])
    se <- unname(coefs[row_name, "Std. Error"])
    pval <- unname(coefs[row_name, "Pr(>|z|)"])
  }

  singular <- tryCatch(lme4::isSingular(fit), error = function(e) NA)
  converged <- is.null(fit@optinfo$conv$lme4$messages)

  list(
    fit = fit,
    coefficients = coefs,
    treatment_row = if (!is.na(row_name)) coefs[row_name, , drop = FALSE] else NULL,
    estimate = est, std_error = se, p_value = as.numeric(pval),
    converged = converged, singular = singular
  )
}

#' Analyze a simulated stepped-wedge trial
#'
#' Convenience wrapper around [fit_stepwedge_model()] for data produced by
#' [simulate_swcrt()], using its default column names.
#'
#' @param trial A data frame from [simulate_swcrt()].
#' @param ... Passed to [fit_stepwedge_model()].
#' @return The list returned by [fit_stepwedge_model()].
#' @export
analyze_swcrt <- function(trial, ...) {
  fit_stepwedge_model(trial, ...)
}
