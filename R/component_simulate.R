# Internal effect history for one component in one sequence.
.component_effect_history <- function(
  active,
  delay = 0L,
  carryover_weights = numeric(0),
  restart_rule = c("reset", "resume")
) {
  restart_rule <- match.arg(restart_rule)
  active <- as.integer(active)
  if (anyNA(active) || any(!active %in% c(0L, 1L))) {
    stop("Internal component assignment must contain only 0 and 1.",
         call. = FALSE)
  }
  delay <- .validate_multistage_delay(delay, "delay")
  n <- length(active)

  exposure_time <- integer(n)
  cumulative_exposure <- integer(n)
  episode <- integer(n)
  withdrawal <- integer(n)
  restart <- integer(n)
  periods_since_withdrawal <- integer(n)
  current_weight <- numeric(n)
  carryover_weight <- numeric(n)
  effect_weight <- numeric(n)

  consecutive <- 0L
  cumulative <- 0L
  episode_id <- 0L
  ever_active <- FALSE
  last_withdrawal_any <- NA_integer_
  last_mature_active_period <- NA_integer_
  previous_current_weight <- 0

  for (t in seq_len(n)) {
    previous_active <- if (t > 1L) active[t - 1L] else 0L

    if (active[t] == 1L) {
      if (previous_active == 0L) {
        episode_id <- episode_id + 1L
        consecutive <- 1L
        if (ever_active) restart[t] <- 1L
      } else {
        consecutive <- consecutive + 1L
      }
      cumulative <- cumulative + 1L
      exposure_time[t] <- consecutive
      cumulative_exposure[t] <- cumulative
      episode[t] <- episode_id
      maturity_clock <- if (restart_rule == "reset") consecutive else cumulative
      current_weight[t] <- as.numeric(maturity_clock > delay)
      ever_active <- TRUE
    } else {
      consecutive <- 0L
      cumulative_exposure[t] <- cumulative
      episode[t] <- episode_id
      if (previous_active == 1L) {
        withdrawal[t] <- 1L
        last_withdrawal_any <- t - 1L
        if (previous_current_weight > 0) {
          last_mature_active_period <- t - 1L
        }
      }
    }

    if (!is.na(last_withdrawal_any)) {
      periods_since_withdrawal[t] <- max(0L, t - last_withdrawal_any)
    }

    if (!is.na(last_mature_active_period) && length(carryover_weights)) {
      carry_index <- t - last_mature_active_period
      if (carry_index >= 1L && carry_index <= length(carryover_weights)) {
        carryover_weight[t] <- carryover_weights[carry_index]
      }
    }

    # A restarted component can retain residual effect during a renewed wash-in.
    effect_weight[t] <- max(current_weight[t], carryover_weight[t])
    previous_current_weight <- current_weight[t]
  }

  data.frame(
    active = active,
    exposure_time = exposure_time,
    cumulative_exposure = cumulative_exposure,
    episode = episode,
    withdrawal = withdrawal,
    restart = restart,
    periods_since_withdrawal = periods_since_withdrawal,
    current_effect_weight = current_weight,
    carryover_weight = carryover_weight,
    effect_weight = effect_weight,
    stringsAsFactors = FALSE
  )
}

# Internal sequence-period representation shared by simulation and auditing.
.component_sequence_period <- function(design, assumptions = NULL) {
  if (!inherits(design, "sw_component_design")) {
    stop("`design` must be an sw_component_design object.", call. = FALSE)
  }

  if (is.null(assumptions)) {
    delay_a <- delay_b <- 0L
    carry_a <- carry_b <- numeric(0)
    restart_a <- restart_b <- "reset"
    interaction_mode <- "effective_overlap"
  } else {
    if (!inherits(assumptions, "sw_component_assumptions")) {
      stop("`assumptions` must be an sw_component_assumptions object.",
           call. = FALSE)
    }
    delay_a <- assumptions$delay_a
    delay_b <- assumptions$delay_b
    carry_a <- assumptions$carryover_weights_a
    carry_b <- assumptions$carryover_weights_b
    restart_a <- assumptions$restart_rule_a
    restart_b <- assumptions$restart_rule_b
    interaction_mode <- assumptions$interaction_mode
  }

  observed_matrix <- .component_observed_matrix(design)
  rows <- vector("list", design$n_sequences)
  for (s in seq_len(design$n_sequences)) {
    hist_a <- .component_effect_history(
      design$component_a[s, ], delay = delay_a,
      carryover_weights = carry_a, restart_rule = restart_a
    )
    hist_b <- .component_effect_history(
      design$component_b[s, ], delay = delay_b,
      carryover_weights = carry_b, restart_rule = restart_b
    )

    if (interaction_mode == "effective_overlap") {
      interaction_weight <- hist_a$effect_weight * hist_b$effect_weight
    } else if (interaction_mode == "assigned_overlap") {
      interaction_weight <- hist_a$current_effect_weight *
        hist_b$current_effect_weight
    } else {
      interaction_weight <- rep(0, design$n_periods)
    }

    rows[[s]] <- data.frame(
      sequence_idx = s,
      sequence = design$sequence_names[s],
      period = seq_len(design$n_periods),
      observed = as.logical(observed_matrix[s, ]),
      state_code = as.integer(design$state[s, ]),
      state = factor(
        design$stage_names[design$state[s, ] + 1L],
        levels = design$stage_names
      ),
      a_active = hist_a$active,
      b_active = hist_b$active,
      ab_active = hist_a$active * hist_b$active,
      a_exposure_time = hist_a$exposure_time,
      b_exposure_time = hist_b$exposure_time,
      a_cumulative_exposure = hist_a$cumulative_exposure,
      b_cumulative_exposure = hist_b$cumulative_exposure,
      a_episode = hist_a$episode,
      b_episode = hist_b$episode,
      a_withdrawal = hist_a$withdrawal,
      b_withdrawal = hist_b$withdrawal,
      a_restart = hist_a$restart,
      b_restart = hist_b$restart,
      a_periods_since_withdrawal = hist_a$periods_since_withdrawal,
      b_periods_since_withdrawal = hist_b$periods_since_withdrawal,
      a_current_weight = hist_a$current_effect_weight,
      b_current_weight = hist_b$current_effect_weight,
      a_carryover_weight = hist_a$carryover_weight,
      b_carryover_weight = hist_b$carryover_weight,
      a_effect_weight = hist_a$effect_weight,
      b_effect_weight = hist_b$effect_weight,
      ab_effect_weight = interaction_weight,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Simulate a component-based stepped-wedge trial
#'
#' Generates aggregated binary cluster-period outcomes. Structurally unobserved
#' cells retain latent treatment history but do not receive outcomes.
#'
#' @param design An [sw_component_design()] object.
#' @param assumptions An [sw_component_assumptions()] object.
#' @param seed Optional random seed.
#' @param include_unobserved Retain latent unobserved rows with missing outcomes.
#' @return A data frame with observed cluster-periods by default.
#' @export
simulate_component_swcrt <- function(design, assumptions, seed = NULL, include_unobserved = FALSE) {
  if (!inherits(design, "sw_component_design")) stop("`design` must be an sw_component_design object.", call. = FALSE)
  if (!inherits(assumptions, "sw_component_assumptions")) stop("`assumptions` must be an sw_component_assumptions object.", call. = FALSE)
  if (length(include_unobserved) != 1L || is.na(include_unobserved)) stop("`include_unobserved` must be TRUE or FALSE.", call. = FALSE)
  include_unobserved <- isTRUE(include_unobserved)
  if (!is.null(seed)) set.seed(seed)
  n_seq <- design$n_sequences; n_per <- design$n_periods
  baseline <- .expand_baseline_logit(assumptions$baseline_logit, n_seq, n_per)
  sequence_period <- .component_sequence_period(design, assumptions)
  sequence_of_cluster <- rep(seq_len(n_seq), times = design$clusters_per_sequence)
  n_clusters <- length(sequence_of_cluster)
  random_intercept <- stats::rnorm(n_clusters, 0, assumptions$cluster_sd)
  cluster_id <- rep(seq_len(n_clusters), each = n_per)
  period <- rep(seq_len(n_per), times = n_clusters)
  sequence_idx <- sequence_of_cluster[cluster_id]
  sp_index <- (sequence_idx - 1L) * n_per + period
  baseline_index <- cbind(sequence_idx, period)
  observed <- as.logical(sequence_period$observed[sp_index]); obs_idx <- which(observed)
  n <- rep(NA_integer_, length(cluster_id))
  n_obs <- .resolve_sample_size(assumptions$n_per_cluster_period, sequence_idx[obs_idx], period[obs_idx], n_seq, n_per)
  if (anyNA(n_obs) || any(n_obs < 1L)) stop("All observed cluster-period sample sizes must be positive integers.", call. = FALSE)
  n[obs_idx] <- as.integer(n_obs)
  wa <- sequence_period$a_effect_weight[sp_index]; wb <- sequence_period$b_effect_weight[sp_index]; wab <- sequence_period$ab_effect_weight[sp_index]
  ca <- assumptions$treatment_effect_a * wa; cb <- assumptions$treatment_effect_b * wb; cab <- assumptions$interaction_effect * wab
  time_effect <- if (inherits(design, "sw_batched_design") &&
                     inherits(assumptions, "sw_batched_assumptions")) {
    .resolve_batched_time_effect_vector(
      design = design, assumptions = assumptions,
      sequence_idx = sequence_idx, period = period
    )
  } else {
    .expand_period_effects(assumptions$period_effects, n_per)[period]
  }
  eta <- baseline[baseline_index] + time_effect + random_intercept[cluster_id] + ca + cb + cab
  probability <- stats::plogis(eta)
  events <- rep(NA_integer_, length(probability)); events[obs_idx] <- stats::rbinom(length(obs_idx), n[obs_idx], probability[obs_idx])
  history_columns <- setdiff(names(sequence_period), c("sequence_idx", "sequence", "period"))
  history <- sequence_period[sp_index, history_columns, drop = FALSE]
  out <- data.frame(cluster_id = cluster_id, sequence = design$sequence_names[sequence_idx], sequence_idx = sequence_idx, period = period,
                    history, n = as.integer(n), events = as.integer(events), true_prob = probability,
                    true_time_effect = time_effect,
                    true_contribution_a = ca, true_contribution_b = cb, true_contribution_ab = cab,
                    stringsAsFactors = FALSE, check.names = FALSE)
  out <- .append_batched_time_columns(out, design)
  out <- out[order(out$cluster_id, out$period), , drop = FALSE]
  if (!include_unobserved) out <- out[out$observed, , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "sequence_levels") <- design$sequence_names; attr(out, "component_assumptions") <- assumptions
  attr(out, "component_design") <- design; attr(out, "observation_mask") <- .component_observed_matrix(design)
  out
}
