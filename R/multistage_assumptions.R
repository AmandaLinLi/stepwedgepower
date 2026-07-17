#' Specify assumptions for a cumulative A then A+B stepped-wedge trial
#'
#' Collects the data-generating assumptions for an asynchronous cumulative
#' intervention design created by [sw_multistage_design()]. The binary-outcome
#' generating model is
#' 
#' \deqn{\mathrm{logit}(p_{ijt}) = \beta_{0,jt} + \gamma_t + b_i +
#' \beta_A A^*_{ijt} + \beta_B B^*_{ijt},}
#'
#' where `A*` becomes one after A has completed its prespecified delay and `B*`
#' becomes one after B has completed its delay. Because B is never delivered
#' alone, `exp(beta_B)` is the incremental effect of adding B among clusters
#' already receiving A. The total A+B effect relative to control is
#' `exp(beta_A + beta_B)`.
#'
#' A delay of zero means that the effect begins in the first active period. A
#' delay of one treats the first active period as wash-in and begins the effect
#' in the second exposure period.
#'
#' @param outcome Outcome type. Currently `"binary"`.
#' @param baseline_prob Baseline probability: a scalar, a vector with one value
#'   per sequence, or a sequence-by-period matrix. Supply this or
#'   `baseline_logit`, not both.
#' @param baseline_logit Baseline linear predictor on the logit scale, with the
#'   same permitted shapes as `baseline_prob`.
#' @param treatment_or_a Odds ratio for A versus control after the A delay.
#'   Supply this or `treatment_effect_a`.
#' @param treatment_effect_a Effect of A on the log-odds scale.
#' @param incremental_or_b Incremental odds ratio for adding B to A after the B
#'   delay. Supply this or `incremental_effect_b`. Defaults to one.
#' @param incremental_effect_b Incremental B effect on the log-odds scale.
#' @param delay_a,delay_b Non-negative integer wash-in periods for A and B.
#' @param icc Latent-scale intraclass correlation. Supply this or `cluster_sd`.
#' @param cluster_sd Cluster random-intercept standard deviation.
#' @param period_effects Numeric vector with one secular log-odds effect per
#'   period. `NULL` means no secular trend.
#' @param n_per_cluster_period Cluster-period sample size: a scalar, a
#'   sequence-by-period matrix, or a function returning one count per row.
#'
#' @return An object of class `"sw_multistage_assumptions"`.
#' @examples
#' assumptions <- sw_multistage_assumptions(
#'   baseline_prob = 0.15,
#'   treatment_or_a = 1.40,
#'   incremental_or_b = 1.30,
#'   delay_a = 1,
#'   delay_b = 1,
#'   icc = 0.05,
#'   n_per_cluster_period = 30
#' )
#' assumptions
#' @export
sw_multistage_assumptions <- function(
  outcome = "binary",
  baseline_prob = NULL,
  baseline_logit = NULL,
  treatment_or_a = NULL,
  treatment_effect_a = NULL,
  incremental_or_b = NULL,
  incremental_effect_b = NULL,
  delay_a = 0L,
  delay_b = 0L,
  icc = NULL,
  cluster_sd = NULL,
  period_effects = NULL,
  n_per_cluster_period = 20
) {
  outcome <- match.arg(outcome, "binary")

  if (is.null(baseline_prob) && is.null(baseline_logit)) {
    stop("Supply one of `baseline_prob` or `baseline_logit`.", call. = FALSE)
  }
  if (!is.null(baseline_prob) && !is.null(baseline_logit)) {
    stop("Supply only one of `baseline_prob` or `baseline_logit`.", call. = FALSE)
  }
  if (!is.null(baseline_prob)) {
    if (!is.numeric(baseline_prob) || anyNA(baseline_prob) ||
        any(!is.finite(baseline_prob)) ||
        any(baseline_prob <= 0 | baseline_prob >= 1)) {
      stop("`baseline_prob` must contain finite values strictly between 0 and 1.",
           call. = FALSE)
    }
    baseline_logit <- stats::qlogis(baseline_prob)
  } else if (!is.numeric(baseline_logit) || anyNA(baseline_logit) ||
             any(!is.finite(baseline_logit))) {
    stop("`baseline_logit` must contain finite numeric values.", call. = FALSE)
  }

  effect_a <- .resolve_multistage_effect(
    odds_ratio = treatment_or_a,
    log_effect = treatment_effect_a,
    odds_name = "treatment_or_a",
    effect_name = "treatment_effect_a",
    required = TRUE
  )
  effect_b <- .resolve_multistage_effect(
    odds_ratio = incremental_or_b,
    log_effect = incremental_effect_b,
    odds_name = "incremental_or_b",
    effect_name = "incremental_effect_b",
    required = FALSE,
    default_effect = 0
  )

  delay_a <- .validate_multistage_delay(delay_a, "delay_a")
  delay_b <- .validate_multistage_delay(delay_b, "delay_b")

  cluster_sd <- .resolve_cluster_sd(icc = icc, cluster_sd = cluster_sd)
  if (is.null(cluster_sd)) {
    stop("Supply one of `icc` or `cluster_sd`.", call. = FALSE)
  }
  if (length(cluster_sd) != 1L || is.na(cluster_sd) ||
      !is.finite(cluster_sd) || cluster_sd < 0) {
    stop("`cluster_sd` must be one finite non-negative value.", call. = FALSE)
  }

  structure(
    list(
      outcome = outcome,
      baseline_logit = baseline_logit,
      treatment_effect_a = effect_a,
      treatment_or_a = exp(effect_a),
      incremental_effect_b = effect_b,
      incremental_or_b = exp(effect_b),
      total_effect_ab = effect_a + effect_b,
      total_or_ab = exp(effect_a + effect_b),
      delay_a = delay_a,
      delay_b = delay_b,
      cluster_sd = cluster_sd,
      icc = cluster_sd_to_icc(cluster_sd),
      period_effects = period_effects,
      n_per_cluster_period = n_per_cluster_period
    ),
    class = "sw_multistage_assumptions"
  )
}

.resolve_multistage_effect <- function(
  odds_ratio,
  log_effect,
  odds_name,
  effect_name,
  required = TRUE,
  default_effect = NULL,
  tolerance = 1e-6
) {
  if (is.null(odds_ratio) && is.null(log_effect)) {
    if (isTRUE(required)) {
      stop("Supply one of `", odds_name, "` or `", effect_name, "`.",
           call. = FALSE)
    }
    return(default_effect)
  }

  if (!is.null(odds_ratio)) {
    if (length(odds_ratio) != 1L || !is.numeric(odds_ratio) ||
        is.na(odds_ratio) || !is.finite(odds_ratio) || odds_ratio <= 0) {
      stop("`", odds_name, "` must be one finite positive number.",
           call. = FALSE)
    }
  }
  if (!is.null(log_effect)) {
    if (length(log_effect) != 1L || !is.numeric(log_effect) ||
        is.na(log_effect) || !is.finite(log_effect)) {
      stop("`", effect_name, "` must be one finite number.", call. = FALSE)
    }
  }

  if (!is.null(odds_ratio) && !is.null(log_effect) &&
      abs(log(odds_ratio) - log_effect) > tolerance) {
    stop("`", odds_name, "` and `", effect_name, "` disagree.",
         call. = FALSE)
  }
  if (!is.null(log_effect)) as.numeric(log_effect) else log(odds_ratio)
}

#' @param x An `sw_multistage_assumptions` object.
#' @param ... Unused.
#' @rdname sw_multistage_assumptions
#' @export
print.sw_multistage_assumptions <- function(x, ...) {
  cat("<sw_multistage_assumptions>\n")
  cat("  outcome:", x$outcome, "\n")
  cat(sprintf("  A vs Control: OR = %.3f (log-odds %.3f), delay %d\n",
              x$treatment_or_a, x$treatment_effect_a, x$delay_a))
  cat(sprintf("  incremental B: OR = %.3f (log-odds %.3f), delay %d\n",
              x$incremental_or_b, x$incremental_effect_b, x$delay_b))
  cat(sprintf("  total A+B vs Control: OR = %.3f\n", x$total_or_ab))
  cat(sprintf("  cluster_sd = %.3f (latent ICC = %.3f)\n",
              x$cluster_sd, x$icc))
  cat("  baseline probabilities:",
      paste(sprintf("%.3f", stats::plogis(as.numeric(x$baseline_logit))),
            collapse = ", "), "\n")
  if (!is.null(x$period_effects)) {
    cat("  period effects (log-odds):",
        paste(sprintf("%.3f", x$period_effects), collapse = ", "), "\n")
  }
  invisible(x)
}
