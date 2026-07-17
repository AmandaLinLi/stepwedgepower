#' Simulate an asynchronous Control to A to A+B stepped-wedge trial
#'
#' Generates aggregated cluster-period binary outcomes for a cumulative
#' intervention design. Sequences can start A and add B in different periods,
#' and each intervention can have a prespecified wash-in delay.
#'
#' @param design An [sw_multistage_design()] object.
#' @param assumptions An [sw_multistage_assumptions()] object.
#' @param seed Optional random seed.
#'
#' @return A data frame with one row per cluster-period. In addition to the
#'   standard count columns, it contains the active treatment state, exposure
#'   times since A and B began, and the delayed-effect indicators used by the
#'   default analysis.
#' @examples
#' design <- sw_multistage_design(
#'   clusters_per_sequence = rep(4, 4),
#'   a_start = c(2, 3, 4, 5),
#'   b_start = c(5, 7, 6, 8),
#'   n_periods = 8
#' )
#' assumptions <- sw_multistage_assumptions(
#'   baseline_prob = 0.15, treatment_or_a = 1.4,
#'   incremental_or_b = 1.3, delay_a = 1, delay_b = 1,
#'   icc = 0.05, n_per_cluster_period = 30
#' )
#' head(simulate_multistage_swcrt(design, assumptions, seed = 1))
#' @export
simulate_multistage_swcrt <- function(design, assumptions, seed = NULL) {
  if (!inherits(design, "sw_multistage_design")) {
    stop("`design` must be an sw_multistage_design object.", call. = FALSE)
  }
  if (!inherits(assumptions, "sw_multistage_assumptions")) {
    stop("`assumptions` must be an sw_multistage_assumptions object.",
         call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  n_seq <- design$n_sequences
  n_per <- design$n_periods
  baseline <- .expand_baseline_logit(
    assumptions$baseline_logit, n_seq, n_per
  )
  period_effect <- .expand_period_effects(
    assumptions$period_effects, n_per
  )

  sequence_of_cluster <- rep(
    seq_len(n_seq),
    times = design$clusters_per_sequence
  )
  n_clusters <- length(sequence_of_cluster)
  random_intercept <- stats::rnorm(
    n_clusters, mean = 0, sd = assumptions$cluster_sd
  )

  cluster_id <- rep(seq_len(n_clusters), each = n_per)
  period <- rep(seq_len(n_per), times = n_clusters)
  sequence_idx <- sequence_of_cluster[cluster_id]
  index <- cbind(sequence_idx, period)

  a_active <- as.integer(design$intervention_a[index])
  b_active <- as.integer(design$intervention_b[index])
  state_code <- as.integer(design$state[index])

  a_start <- design$a_start[sequence_idx]
  b_start <- design$b_start[sequence_idx]
  a_exposure_time <- ifelse(
    a_active == 1L,
    period - a_start + 1,
    0
  )
  b_exposure_time <- ifelse(
    b_active == 1L,
    period - b_start + 1,
    0
  )
  a_exposure_time <- as.integer(a_exposure_time)
  b_exposure_time <- as.integer(b_exposure_time)
  a_effective <- as.integer(a_exposure_time > assumptions$delay_a)
  b_effective <- as.integer(b_exposure_time > assumptions$delay_b)

  n <- .resolve_sample_size(
    assumptions$n_per_cluster_period,
    sequence_idx,
    period,
    n_seq,
    n_per
  )
  if (anyNA(n) || any(n < 1L)) {
    stop("All cluster-period sample sizes must be positive integers.",
         call. = FALSE)
  }

  contribution_a <- assumptions$treatment_effect_a * a_effective
  contribution_b <- assumptions$incremental_effect_b * b_effective
  eta <- baseline[index] +
    period_effect[period] +
    random_intercept[cluster_id] +
    contribution_a +
    contribution_b
  probability <- stats::plogis(eta)
  events <- stats::rbinom(length(probability), size = n, prob = probability)

  out <- data.frame(
    cluster_id = cluster_id,
    sequence = design$sequence_names[sequence_idx],
    sequence_idx = sequence_idx,
    period = period,
    state = factor(
      design$stage_names[state_code + 1L],
      levels = design$stage_names,
      ordered = TRUE
    ),
    a_active = a_active,
    b_active = b_active,
    a_exposure_time = a_exposure_time,
    b_exposure_time = b_exposure_time,
    a_effective = a_effective,
    b_effective = b_effective,
    n = as.integer(n),
    events = as.integer(events),
    true_prob = probability,
    true_contribution_a = contribution_a,
    true_contribution_b = contribution_b,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$cluster_id, out$period), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "sequence_levels") <- design$sequence_names
  attr(out, "delay_a") <- assumptions$delay_a
  attr(out, "delay_b") <- assumptions$delay_b
  out
}
