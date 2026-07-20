#' Specify assumptions for a component-based stepped-wedge trial
#'
#' Collects the data-generating assumptions for designs containing Control, A,
#' B, and A+B. Component effects are parameterized on the log-odds scale. The
#' interaction is the departure of A+B from additivity on that scale.
#'
#' A nonzero `delay_a` or `delay_b` creates a wash-in period. For example,
#' `delay_a = 1` means that the first assigned period has no A effect and the
#' full A effect begins in the second consecutive A period.
#'
#' Withdrawal can be followed by carryover. `carryover_periods_a = 2` with
#' `carryover_weights_a = c(0.6, 0.3)` applies 60 percent and 30 percent of the
#' full A log-odds effect in the first and second periods after withdrawal.
#' When weights are omitted, the full effect persists for the specified number
#' of periods. If a component is restarted while carryover remains, the effect
#' weight is the larger of the residual carryover and the restarted current
#' effect.
#'
#' @param outcome Currently only `"binary"`.
#' @param baseline_prob,baseline_logit Baseline risk or baseline logit. Supply
#'   exactly one. Values may be scalar, per sequence, or sequence-by-period as
#'   supported by the main simulation engine.
#' @param treatment_or_a,treatment_effect_a Full effect of A versus Control as
#'   an odds ratio or log-odds coefficient. If omitted, the effect is zero.
#' @param treatment_or_b,treatment_effect_b Full effect of B versus Control.
#' @param interaction_or,interaction_effect Multiplicative interaction odds
#'   ratio or its log-odds coefficient. A value of one means no interaction.
#' @param delay_a,delay_b Non-negative integer wash-in periods.
#' @param carryover_periods_a,carryover_periods_b Number of periods for which a
#'   mature component effect can persist after withdrawal.
#' @param carryover_weights_a,carryover_weights_b Optional non-increasing
#'   weights in `[0, 1]`. Supply one value to repeat it for all carryover
#'   periods, or one value per period.
#' @param restart_rule_a,restart_rule_b Either `"reset"` or `"resume"`.
#'   `"reset"` restarts the wash-in clock after each interruption; `"resume"`
#'   uses cumulative assigned exposure across episodes.
#' @param interaction_mode How the interaction effect is activated.
#'   `"effective_overlap"` uses the product of the A and B effect weights and
#'   therefore permits interaction carryover; `"assigned_overlap"` requires
#'   both components to be currently assigned and past wash-in;
#'   `"none"` suppresses the interaction contribution.
#' @param icc,cluster_sd Supply one latent-scale logistic-normal ICC or cluster
#'   random-intercept standard deviation.
#' @param period_effects Numeric vector of secular log-odds effects, or `NULL`.
#' @param n_per_cluster_period Cluster-period sample size specification.
#'
#' @return An object of class `"sw_component_assumptions"`.
#' @examples
#' assumptions <- sw_component_assumptions(
#'   baseline_prob = 0.15,
#'   treatment_or_a = 1.35,
#'   treatment_or_b = 1.25,
#'   interaction_or = 1.10,
#'   delay_a = 1,
#'   delay_b = 1,
#'   carryover_periods_a = 2,
#'   carryover_weights_a = c(0.5, 0.25),
#'   icc = 0.05,
#'   n_per_cluster_period = 25
#' )
#' assumptions
#' @export
sw_component_assumptions <- function(
  outcome = "binary",
  baseline_prob = NULL,
  baseline_logit = NULL,
  treatment_or_a = NULL,
  treatment_effect_a = NULL,
  treatment_or_b = NULL,
  treatment_effect_b = NULL,
  interaction_or = NULL,
  interaction_effect = NULL,
  delay_a = 0L,
  delay_b = 0L,
  carryover_periods_a = 0L,
  carryover_periods_b = 0L,
  carryover_weights_a = NULL,
  carryover_weights_b = NULL,
  restart_rule_a = c("reset", "resume"),
  restart_rule_b = c("reset", "resume"),
  interaction_mode = c("effective_overlap", "assigned_overlap", "none"),
  icc = NULL,
  cluster_sd = NULL,
  period_effects = NULL,
  n_per_cluster_period = 20
) {
  outcome <- match.arg(outcome, "binary")
  restart_rule_a <- match.arg(restart_rule_a)
  restart_rule_b <- match.arg(restart_rule_b)
  interaction_mode <- match.arg(interaction_mode)

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
    required = FALSE,
    default_effect = 0
  )
  effect_b <- .resolve_multistage_effect(
    odds_ratio = treatment_or_b,
    log_effect = treatment_effect_b,
    odds_name = "treatment_or_b",
    effect_name = "treatment_effect_b",
    required = FALSE,
    default_effect = 0
  )
  effect_ab <- .resolve_multistage_effect(
    odds_ratio = interaction_or,
    log_effect = interaction_effect,
    odds_name = "interaction_or",
    effect_name = "interaction_effect",
    required = FALSE,
    default_effect = 0
  )
  if (interaction_mode == "none" &&
      abs(effect_ab) > sqrt(.Machine$double.eps)) {
    warning(
      "The interaction effect is ignored because `interaction_mode = \"none\"`.",
      call. = FALSE
    )
    effect_ab <- 0
  }

  delay_a <- .validate_multistage_delay(delay_a, "delay_a")
  delay_b <- .validate_multistage_delay(delay_b, "delay_b")
  carry_a <- .resolve_component_carryover(
    carryover_periods_a, carryover_weights_a,
    "carryover_periods_a", "carryover_weights_a"
  )
  carry_b <- .resolve_component_carryover(
    carryover_periods_b, carryover_weights_b,
    "carryover_periods_b", "carryover_weights_b"
  )

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
      treatment_effect_b = effect_b,
      treatment_or_b = exp(effect_b),
      interaction_effect = effect_ab,
      interaction_or = exp(effect_ab),
      delay_a = delay_a,
      delay_b = delay_b,
      carryover_periods_a = length(carry_a),
      carryover_periods_b = length(carry_b),
      carryover_weights_a = carry_a,
      carryover_weights_b = carry_b,
      restart_rule_a = restart_rule_a,
      restart_rule_b = restart_rule_b,
      interaction_mode = interaction_mode,
      cluster_sd = cluster_sd,
      icc = cluster_sd_to_icc(cluster_sd),
      period_effects = period_effects,
      n_per_cluster_period = n_per_cluster_period
    ),
    class = "sw_component_assumptions"
  )
}

.resolve_component_carryover <- function(
  periods,
  weights,
  periods_name,
  weights_name
) {
  if (length(periods) != 1L || !is.numeric(periods) || is.na(periods) ||
      !is.finite(periods) || periods < 0 || periods != round(periods)) {
    stop("`", periods_name, "` must be one non-negative integer.",
         call. = FALSE)
  }
  periods <- as.integer(periods)

  if (periods == 0L) {
    if (!is.null(weights) && length(weights) > 0L) {
      stop("`", weights_name, "` must be NULL when `", periods_name,
           "` is zero.", call. = FALSE)
    }
    return(numeric(0))
  }

  if (is.null(weights)) weights <- rep(1, periods)
  if (!is.numeric(weights) || anyNA(weights) || any(!is.finite(weights))) {
    stop("`", weights_name, "` must contain finite numeric values.",
         call. = FALSE)
  }
  if (length(weights) == 1L) weights <- rep(weights, periods)
  if (length(weights) != periods) {
    stop("`", weights_name, "` must have length one or `", periods_name,
         "` values.", call. = FALSE)
  }
  if (any(weights < 0 | weights > 1)) {
    stop("`", weights_name, "` must lie between 0 and 1.", call. = FALSE)
  }
  if (length(weights) > 1L && any(diff(weights) > sqrt(.Machine$double.eps))) {
    stop("`", weights_name, "` must be non-increasing over time.",
         call. = FALSE)
  }
  as.numeric(weights)
}

#' @param x An `sw_component_assumptions` object.
#' @param ... Unused.
#' @rdname sw_component_assumptions
#' @export
print.sw_component_assumptions <- function(x, ...) {
  cat("<sw_component_assumptions>\n")
  cat(sprintf("  A vs Control: OR = %.3f; wash-in = %d period(s)\n",
              x$treatment_or_a, x$delay_a))
  cat(sprintf("  B vs Control: OR = %.3f; wash-in = %d period(s)\n",
              x$treatment_or_b, x$delay_b))
  cat(sprintf("  A-by-B interaction OR = %.3f; mode = %s\n",
              x$interaction_or, x$interaction_mode))
  cat("  A carryover weights:",
      if (length(x$carryover_weights_a))
        paste(sprintf("%.3f", x$carryover_weights_a), collapse = ", ")
      else "none", "\n")
  cat("  B carryover weights:",
      if (length(x$carryover_weights_b))
        paste(sprintf("%.3f", x$carryover_weights_b), collapse = ", ")
      else "none", "\n")
  cat(sprintf("  restart rules: A = %s; B = %s\n",
              x$restart_rule_a, x$restart_rule_b))
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
