#' Audit a component-based stepped-wedge design
#'
#' Checks observed treatment-state support, overlap, resource counts,
#' fixed-effect rank, and formal estimability. Structurally unobserved cells are
#' excluded from these calculations but retained for latent exposure history.
#'
#' @param design An [sw_component_design()] object.
#' @param assumptions Optional [sw_component_assumptions()] object.
#' @param contrasts Requested standard contrasts.
#' @param include_interaction Include an A-by-B interaction.
#' @param adjust_sequence Include sequence fixed effects.
#' @param period_effect One of `"categorical"`, `"linear"`, or `"none"`.
#' @return An object of class `"sw_component_audit"`.
#' @export
audit_component_design <- function(
  design, assumptions = NULL, contrasts = NULL, include_interaction = TRUE,
  adjust_sequence = FALSE,
  period_effect = c("categorical", "linear", "none")
) {
  if (!inherits(design, "sw_component_design")) stop("`design` must be an sw_component_design object.", call. = FALSE)
  if (!is.null(assumptions) && !inherits(assumptions, "sw_component_assumptions")) stop("`assumptions` must be NULL or an sw_component_assumptions object.", call. = FALSE)
  period_effect <- match.arg(period_effect)
  include_interaction <- isTRUE(include_interaction)
  contrasts <- .validate_component_contrasts(contrasts, include_interaction)

  full_grid <- .component_sequence_period(design, assumptions)
  grid <- full_grid[full_grid$observed, , drop = FALSE]
  if (!nrow(grid)) stop("The design has no observed sequence-period cells.", call. = FALSE)

  rhs <- c("a_effect_weight", "b_effect_weight")
  if (include_interaction) rhs <- c(rhs, "ab_effect_weight")
  if (period_effect == "categorical") rhs <- c(rhs, "factor(period)") else if (period_effect == "linear") rhs <- c(rhs, "period")
  if (isTRUE(adjust_sequence) && length(unique(grid$sequence)) > 1L) rhs <- c(rhs, "factor(sequence)")
  formula <- stats::as.formula(paste("~", paste(rhs, collapse = " + ")))
  model_matrix <- stats::model.matrix(formula, data = grid)
  qr_matrix <- qr(model_matrix); full_rank <- qr_matrix$rank == ncol(model_matrix)

  contrast_matrix <- .component_contrast_matrix(include_interaction)
  term_names <- c(a = "a_effect_weight", b = "b_effect_weight", ab = "ab_effect_weight")
  estimability <- lapply(contrasts, function(name) {
    linear <- rep(0, ncol(model_matrix)); names(linear) <- colnames(model_matrix)
    weights <- contrast_matrix[name, ]
    for (component in names(weights)) {
      term <- term_names[[component]]
      if (weights[[component]] != 0 && term %in% names(linear)) linear[[term]] <- weights[[component]] else if (weights[[component]] != 0) return(FALSE)
    }
    .component_is_estimable(linear, model_matrix)
  })
  estimability_table <- data.frame(
    contrast = contrasts,
    label = unname(.component_contrast_labels()[contrasts]),
    estimable = unlist(estimability, use.names = FALSE),
    stringsAsFactors = FALSE
  )

  states_by_period <- split(as.character(grid$state), grid$period)
  periods_with <- function(required) as.integer(names(states_by_period)[vapply(states_by_period, function(x) all(required %in% unique(x)), logical(1))])
  overlap <- list(
    control_a = periods_with(c("Control", "A")), control_b = periods_with(c("Control", "B")),
    control_ab = periods_with(c("Control", "A+B")), a_b = periods_with(c("A", "B")),
    a_ab = periods_with(c("A", "A+B")), b_ab = periods_with(c("B", "A+B")),
    all_four = periods_with(c("Control", "A", "B", "A+B"))
  )

  state_counts <- table(factor(grid$state, levels = design$stage_names), factor(grid$period, levels = seq_len(design$n_periods)))
  weighted_grid <- grid
  weighted_grid$cluster_weight <- design$clusters_per_sequence[weighted_grid$sequence_idx]
  cluster_weighted_state_counts <- stats::xtabs(cluster_weight ~ factor(state, levels = design$stage_names) + factor(period, levels = seq_len(design$n_periods)), data = weighted_grid)
  component_variation <- c(A = length(unique(grid$a_effect_weight)) > 1L,
                           B = length(unique(grid$b_effect_weight)) > 1L,
                           interaction = length(unique(grid$ab_effect_weight)) > 1L)

  observed_matrix <- .component_observed_matrix(design)
  observed_period_counts <- colSums(observed_matrix)
  empty_calendar_periods <- which(observed_period_counts == 0L)
  observed_cluster_periods <- sum(design$clusters_per_sequence * rowSums(observed_matrix))
  calendar_cluster_periods <- design$n_clusters * design$n_periods
  messages <- character()

  required_state_map <- list(
    A_vs_control = c("Control", "A"), B_vs_control = c("Control", "B"),
    B_vs_A = c("A", "B"),
    AB_vs_control = c("Control", "A+B"), AB_vs_A = c("A", "A+B"),
    AB_vs_B = c("B", "A+B"), interaction = c("Control", "A", "B", "A+B")
  )
  required_states <- unique(unlist(required_state_map[contrasts]))
  missing_required_states <- setdiff(required_states, unique(as.character(grid$state)))
  if (length(missing_required_states)) messages <- c(messages, paste0("No observed cells occur under state(s) required by the requested contrasts: ", paste(missing_required_states, collapse = ", "), "."))
  if (length(empty_calendar_periods)) messages <- c(messages, paste0("No outcomes are observed in calendar period(s): ", paste(empty_calendar_periods, collapse = ", "), "."))
  if (!full_rank) messages <- c(messages, sprintf("The observed-data fixed-effect design matrix has rank %d of %d.", qr_matrix$rank, ncol(model_matrix)))
  nonestimable <- estimability_table$contrast[!estimability_table$estimable]
  if (length(nonestimable)) messages <- c(messages, paste0("Non-estimable requested contrast(s): ", paste(nonestimable, collapse = ", "), "."))

  overlap_requirements <- list(
    A_vs_control = list(key="control_a", states="Control and A-only"),
    B_vs_control = list(key="control_b", states="Control and B-only"),
    B_vs_A = list(key="a_b", states="A-only and B-only"),
    AB_vs_control = list(key="control_ab", states="Control and A+B"),
    AB_vs_A = list(key="a_ab", states="A-only and A+B"),
    AB_vs_B = list(key="b_ab", states="B-only and A+B"),
    interaction = list(key="all_four", states="all four treatment states")
  )
  for (contrast in contrasts) {
    req <- overlap_requirements[[contrast]]
    if (!is.null(req) && !length(overlap[[req$key]])) messages <- c(messages, paste0("No calendar period contains ", req$states, " among observed cells for contrast `", contrast, "`."))
  }

  if (!is.null(assumptions)) {
    if (design$has_withdrawal_a && assumptions$carryover_periods_a == 0L) messages <- c(messages, "A is withdrawn in the latent schedule and is assumed to lose its effect immediately.")
    if (design$has_withdrawal_b && assumptions$carryover_periods_b == 0L) messages <- c(messages, "B is withdrawn in the latent schedule and is assumed to lose its effect immediately.")
    if (!design$has_withdrawal_a && assumptions$carryover_periods_a > 0L) messages <- c(messages, "A carryover was specified, but A is never withdrawn.")
    if (!design$has_withdrawal_b && assumptions$carryover_periods_b > 0L) messages <- c(messages, "B carryover was specified, but B is never withdrawn.")
  }

  structure(list(
    full_rank = full_rank, rank = qr_matrix$rank, n_columns = ncol(model_matrix), formula = formula,
    estimability = estimability_table, all_requested_estimable = all(estimability_table$estimable),
    state_counts = state_counts, cluster_weighted_state_counts = cluster_weighted_state_counts,
    overlap_periods = overlap, component_variation = component_variation,
    observed_sequence_periods = sum(observed_matrix), missing_sequence_periods = sum(!observed_matrix),
    observed_cluster_periods = as.integer(observed_cluster_periods),
    missing_cluster_periods = as.integer(calendar_cluster_periods - observed_cluster_periods),
    calendar_cluster_periods = as.integer(calendar_cluster_periods),
    observed_periods_per_sequence = rowSums(observed_matrix),
    observed_sequences_per_period = observed_period_counts,
    empty_calendar_periods = empty_calendar_periods, incomplete = any(!observed_matrix),
    withdrawals = c(A = design$withdrawal_count_a, B = design$withdrawal_count_b),
    restarts = c(A = design$restart_count_a, B = design$restart_count_b),
    messages = unique(messages), design = design, assumptions = assumptions
  ), class = "sw_component_audit")
}

#' @param x An `sw_component_audit` object.
#' @param ... Unused.
#' @rdname audit_component_design
#' @export
print.sw_component_audit <- function(x, ...) {
  cat("<sw_component_audit>\n")
  cat(sprintf("  observed-data fixed-effect rank: %d of %d (%s)\n", x$rank, x$n_columns, if (x$full_rank) "full" else "deficient"))
  cat(sprintf("  observed cluster-periods: %d of %d; missing: %d\n", x$observed_cluster_periods, x$calendar_cluster_periods, x$missing_cluster_periods))
  cat("  requested contrasts:\n"); print(x$estimability, row.names = FALSE)
  cat(sprintf("  withdrawals: A = %d, B = %d; restarts: A = %d, B = %d\n", x$withdrawals[["A"]], x$withdrawals[["B"]], x$restarts[["A"]], x$restarts[["B"]]))
  if (length(x$messages)) { cat("  cautions:\n"); cat(paste0("  - ", x$messages, collapse = "\n"), "\n") } else cat("  no structural cautions detected\n")
  invisible(x)
}
