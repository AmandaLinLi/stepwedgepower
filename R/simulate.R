#' Simulate one stepped-wedge cluster-randomized trial
#'
#' Generates an aggregated cluster-by-period dataset from a [sw_design()] and
#' [sw_assumptions()]. Clusters are assigned to sequences, cross from control to
#' intervention according to the design's schedule, and produce aggregated
#' binomial outcomes under
#' \deqn{\mathrm{logit}(p_{ijt}) = \beta_{0,jt} + \beta_{\mathrm{trt}} A_{jt} +
#'   \gamma_t + b_i,\quad b_i \sim N(0, \sigma^2).}
#'
#' The returned data frame is application-agnostic: clusters may be hospitals,
#' clinics, schools, practices, physicians, or regions.
#'
#' @param design An [sw_design()] object.
#' @param assumptions An [sw_assumptions()] object.
#' @param seed Optional random seed.
#'
#' @return A data frame with one row per cluster-period and columns
#'   `cluster_id`, `sequence`, `sequence_idx`, `period`, `intervention`, `n`,
#'   `events`, and the true generating probability `true_prob`.
#' @examples
#' design <- sw_design(
#'   clusters_per_sequence = c(8, 8, 8, 8),
#'   crossover_period = c(2, 3, 4, 5), n_periods = 5
#' )
#' a <- sw_assumptions(baseline_prob = 0.05, treatment_or = 1.5, icc = 0.05,
#'                     n_per_cluster_period = 20)
#' head(simulate_swcrt(design, a, seed = 1))
#' @export
simulate_swcrt <- function(design, assumptions, seed = NULL) {
  if (!inherits(design, "sw_design")) {
    stop("`design` must be an sw_design object (see sw_design()).", call. = FALSE)
  }
  if (!inherits(assumptions, "sw_assumptions")) {
    stop("`assumptions` must be an sw_assumptions object.", call. = FALSE)
  }
  if (!is.null(seed)) set.seed(seed)

  n_seq <- design$n_sequences
  n_per <- design$n_periods

  baseline_mat <- .expand_baseline_logit(assumptions$baseline_logit, n_seq, n_per)
  period_eff <- .expand_period_effects(assumptions$period_effects, n_per)

  seq_of_cluster <- rep(seq_len(n_seq), times = design$clusters_per_sequence)
  n_clusters <- length(seq_of_cluster)
  b_i <- stats::rnorm(n_clusters, 0, assumptions$cluster_sd)

  cluster_id <- rep(seq_len(n_clusters), each = n_per)
  period <- rep(seq_len(n_per), times = n_clusters)
  seq_idx <- seq_of_cluster[cluster_id]

  intervention <- design$treatment[cbind(seq_idx, period)]

  n <- .resolve_sample_size(assumptions$n_per_cluster_period,
                            seq_idx, period, n_seq, n_per)
  if (any(n < 1)) {
    stop("All cluster-period sample sizes must be >= 1.", call. = FALSE)
  }

  eta <- baseline_mat[cbind(seq_idx, period)] +
    assumptions$treatment_effect * intervention +
    period_eff[period] +
    b_i[cluster_id]
  prob <- stats::plogis(eta)
  events <- stats::rbinom(length(prob), size = n, prob = prob)

  out <- data.frame(
    cluster_id = cluster_id,
    sequence = design$sequence_names[seq_idx],
    sequence_idx = seq_idx,
    period = period,
    intervention = as.integer(intervention),
    n = as.integer(n),
    events = as.integer(events),
    true_prob = prob,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$cluster_id, out$period), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "sequence_levels") <- design$sequence_names
  out
}
