#' Specify the data-generating assumptions for a stepped-wedge trial
#'
#' Collects the outcome-model assumptions used to simulate a stepped-wedge
#' trial: baseline risk, treatment effect, cluster random-effect magnitude,
#' secular (period) trend, and the cluster-period sample size. The design (who
#' crosses when) is kept separate in [sw_design()].
#'
#' The generating model for a binary outcome is, on the logit scale,
#' \deqn{\mathrm{logit}(p_{ijt}) = \beta_{0,jt} + \beta_{\mathrm{trt}} A_{jt} +
#'   \gamma_t + b_i,}
#' where \eqn{\beta_{0,jt}} is the baseline linear predictor for sequence
#' \eqn{j} in period \eqn{t}, \eqn{A_{jt}} the treatment indicator from the
#' design, \eqn{\gamma_t} the period effect, and \eqn{b_i \sim N(0, \sigma^2)}
#' the cluster random intercept.
#'
#' @param outcome Outcome type. Currently `"binary"`.
#' @param baseline_prob Baseline probability: a single value (common baseline),
#'   a vector of length `n_sequences`, or an `n_sequences x n_periods` matrix.
#'   Provide this or `baseline_logit`, not both.
#' @param baseline_logit Baseline linear predictor on the logit scale, same
#'   shapes as `baseline_prob`.
#' @param treatment_or Treatment odds ratio. Provide this or `treatment_effect`.
#' @param treatment_effect Treatment effect on the log-odds scale.
#' @param icc Latent-scale intraclass correlation. Provide this or `cluster_sd`.
#' @param cluster_sd Random-intercept standard deviation.
#' @param period_effects Numeric vector of length `n_periods` on the log-odds
#'   scale (secular trend). Defaults to zeros; validated against the design in
#'   [simulate_swcrt()].
#' @param n_per_cluster_period Cluster-period sample size: a single value, an
#'   `n_sequences x n_periods` matrix, or a function taking the number of rows
#'   and returning that many counts (e.g. `function(n) rpois(n, 20) + 1`).
#'
#' @return An object of class `"sw_assumptions"`.
#' @examples
#' sw_assumptions(
#'   baseline_prob = c(0.06, 0.04, 0.03, 0.02),
#'   treatment_or = 1.5,
#'   icc = 0.05,
#'   period_effects = c(0, 0.05, 0.10, 0.15, 0.20),
#'   n_per_cluster_period = 20
#' )
#' @export
sw_assumptions <- function(
  outcome = "binary",
  baseline_prob = NULL,
  baseline_logit = NULL,
  treatment_or = NULL,
  treatment_effect = NULL,
  icc = NULL,
  cluster_sd = NULL,
  period_effects = NULL,
  n_per_cluster_period = 20
) {
  outcome <- match.arg(outcome, c("binary"))

  if (is.null(baseline_prob) && is.null(baseline_logit)) {
    stop("Supply one of `baseline_prob` or `baseline_logit`.", call. = FALSE)
  }
  if (!is.null(baseline_prob) && !is.null(baseline_logit)) {
    stop("Supply only one of `baseline_prob` or `baseline_logit`.", call. = FALSE)
  }
  if (!is.null(baseline_prob)) {
    if (any(baseline_prob <= 0 | baseline_prob >= 1)) {
      stop("`baseline_prob` must be strictly between 0 and 1.", call. = FALSE)
    }
    baseline_logit <- stats::qlogis(baseline_prob)
  }

  if (is.null(treatment_or) && is.null(treatment_effect)) {
    stop("Supply one of `treatment_or` or `treatment_effect`.", call. = FALSE)
  }
  if (!is.null(treatment_or) && !is.null(treatment_effect)) {
    if (abs(log(treatment_or) - treatment_effect) > 1e-6) {
      stop("`treatment_or` and `treatment_effect` disagree.", call. = FALSE)
    }
  }
  if (!is.null(treatment_or)) {
    if (treatment_or <= 0) stop("`treatment_or` must be positive.", call. = FALSE)
    treatment_effect <- log(treatment_or)
  } else {
    treatment_or <- exp(treatment_effect)
  }

  cluster_sd <- .resolve_cluster_sd(icc = icc, cluster_sd = cluster_sd)
  if (is.null(cluster_sd)) {
    stop("Supply one of `icc` or `cluster_sd`.", call. = FALSE)
  }

  structure(
    list(
      outcome = outcome,
      baseline_logit = baseline_logit,
      treatment_effect = treatment_effect,
      treatment_or = treatment_or,
      cluster_sd = cluster_sd,
      icc = cluster_sd_to_icc(cluster_sd),
      period_effects = period_effects,
      n_per_cluster_period = n_per_cluster_period
    ),
    class = "sw_assumptions"
  )
}

#' @param x An `sw_assumptions` object.
#' @param ... Unused.
#' @rdname sw_assumptions
#' @export
print.sw_assumptions <- function(x, ...) {
  cat("<sw_assumptions>\n")
  cat("  outcome:", x$outcome, "\n")
  cat(sprintf("  treatment: OR = %.3f (log-odds %.3f)\n",
              x$treatment_or, x$treatment_effect))
  cat(sprintf("  cluster_sd = %.3f (icc = %.3f)\n", x$cluster_sd, x$icc))
  cat("  baseline (prob scale):",
      paste(sprintf("%.3f", stats::plogis(as.numeric(x$baseline_logit))),
            collapse = ", "), "\n")
  if (!is.null(x$period_effects)) {
    cat("  period effects (log-odds):",
        paste(sprintf("%.2f", x$period_effects), collapse = ", "), "\n")
  }
  invisible(x)
}

# ---- Expansion helpers ------------------------------------------------------

.expand_baseline_logit <- function(baseline_logit, n_sequences, n_periods) {
  bl <- baseline_logit
  if (is.matrix(bl)) {
    if (!all(dim(bl) == c(n_sequences, n_periods))) {
      stop("Baseline matrix must be ", n_sequences, " x ", n_periods, ".",
           call. = FALSE)
    }
    return(bl)
  }
  if (length(bl) == 1L) return(matrix(bl, n_sequences, n_periods))
  if (length(bl) == n_sequences) {
    return(matrix(bl, n_sequences, n_periods, byrow = FALSE))
  }
  stop("Baseline must be length 1, length n_sequences (", n_sequences,
       "), or an n_sequences x n_periods matrix.", call. = FALSE)
}

.expand_period_effects <- function(period_effects, n_periods) {
  if (is.null(period_effects)) return(rep(0, n_periods))
  if (length(period_effects) != n_periods) {
    stop("`period_effects` must have length n_periods (", n_periods, ").",
         call. = FALSE)
  }
  period_effects
}

.resolve_sample_size <- function(spec, seq_idx, period_idx, n_sequences, n_periods) {
  n_rows <- length(seq_idx)
  if (is.function(spec)) {
    n <- spec(n_rows)
    if (length(n) != n_rows) {
      stop("Sample-size function must return one value per row.", call. = FALSE)
    }
    return(as.integer(round(n)))
  }
  if (is.matrix(spec)) {
    if (!all(dim(spec) == c(n_sequences, n_periods))) {
      stop("Sample-size matrix must be ", n_sequences, " x ", n_periods, ".",
           call. = FALSE)
    }
    return(as.integer(spec[cbind(seq_idx, period_idx)]))
  }
  if (length(spec) == 1L) return(rep.int(as.integer(spec), n_rows))
  stop("`n_per_cluster_period` must be a scalar, an n_sequences x n_periods ",
       "matrix, or a function of the number of rows.", call. = FALSE)
}
