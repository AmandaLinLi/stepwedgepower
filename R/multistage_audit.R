#' Audit identifiability of an asynchronous cumulative stepped-wedge design
#'
#' Checks whether a `Control -> A -> A+B` schedule provides the concurrent
#' comparisons needed to distinguish intervention effects from calendar time.
#' It also flags two common threats to interpretation: adding B in one common
#' calendar period, and adding B after exactly the same duration of A exposure
#' in every sequence. The latter can make an incremental B effect difficult to
#' distinguish from maturation of A when A itself may change with exposure time.
#'
#' @param design An [sw_multistage_design()] object.
#' @param delay_a,delay_b Non-negative integer delays used to form the default
#'   analysis indicators. These should match the values in
#'   [sw_multistage_assumptions()].
#' @param adjust_sequence Logical; include sequence fixed effects in the
#'   fixed-effect rank check.
#'
#' @return An object of class `"sw_multistage_audit"` containing overlap
#'   periods, rank diagnostics, schedule flags, state counts, and explanatory
#'   messages.
#' @examples
#' design <- sw_multistage_design(
#'   clusters_per_sequence = rep(4, 4),
#'   a_start = c(2, 3, 4, 5),
#'   b_start = c(5, 7, 6, 8),
#'   n_periods = 8
#' )
#' audit_multistage_design(design, delay_a = 1, delay_b = 1)
#' @export
audit_multistage_design <- function(
  design,
  delay_a = 0L,
  delay_b = 0L,
  adjust_sequence = FALSE
) {
  if (!inherits(design, "sw_multistage_design")) {
    stop("`design` must be an sw_multistage_design object.", call. = FALSE)
  }
  layout <- .multistage_sequence_period(design, delay_a, delay_b)

  periods <- seq_len(design$n_periods)
  active_states <- split(as.integer(layout$state), layout$period)
  control_a_periods <- periods[vapply(
    active_states,
    function(x) all(c(1L, 2L) %in% unique(x)),
    logical(1)
  )]
  a_ab_periods <- periods[vapply(
    active_states,
    function(x) all(c(2L, 3L) %in% unique(x)),
    logical(1)
  )]
  all_three_periods <- periods[vapply(
    active_states,
    function(x) all(1:3 %in% unique(x)),
    logical(1)
  )]

  effective_a_periods <- periods[vapply(periods, function(p) {
    d <- layout[layout$period == p & layout$b_effective == 0L, , drop = FALSE]
    length(unique(d$a_effective)) > 1L
  }, logical(1))]
  effective_b_periods <- periods[vapply(periods, function(p) {
    d <- layout[layout$period == p & layout$a_effective == 1L, , drop = FALSE]
    length(unique(d$b_effective)) > 1L
  }, logical(1))]

  has_a_variation <- length(unique(layout$a_effective)) > 1L
  has_b_variation <- length(unique(layout$b_effective)) > 1L

  rhs <- c("a_effective")
  if (has_b_variation) rhs <- c(rhs, "b_effective")
  rhs <- c(rhs, "factor(period)")
  if (isTRUE(adjust_sequence) && design$n_sequences > 1L) {
    rhs <- c(rhs, "factor(sequence)")
  }
  fixed_formula <- stats::as.formula(
    paste("~", paste(rhs, collapse = " + "))
  )
  model_matrix <- stats::model.matrix(fixed_formula, data = layout)
  fixed_rank <- qr(model_matrix)$rank
  full_rank <- fixed_rank == ncol(model_matrix)

  finite_b <- is.finite(design$b_start)
  finite_lags <- design$a_to_b_lag[finite_b]
  constant_a_to_b_lag <- length(finite_lags) > 1L &&
    length(unique(finite_lags)) == 1L
  common_b_calendar_period <- sum(finite_b) > 1L &&
    length(unique(design$b_start[finite_b])) == 1L
  staggered_b <- sum(finite_b) > 1L &&
    length(unique(design$b_start[finite_b])) > 1L

  messages <- character()
  if (!length(control_a_periods)) {
    messages <- c(
      messages,
      "No calendar period contains both Control and A-only sequences."
    )
  }
  if (design$has_b && !length(a_ab_periods)) {
    messages <- c(
      messages,
      "No calendar period contains both A-only and A+B sequences."
    )
  }
  if (!length(effective_a_periods)) {
    messages <- c(
      messages,
      "The delayed A indicator has no concurrent treated-control contrast."
    )
  }
  if (design$has_b && !length(effective_b_periods)) {
    messages <- c(
      messages,
      "The delayed B indicator has no concurrent A-only versus A+B contrast."
    )
  }
  if (common_b_calendar_period) {
    messages <- c(
      messages,
      paste(
        "All sequences add B in the same calendar period;",
        "the B effect may be aliased with calendar time."
      )
    )
  }
  if (constant_a_to_b_lag) {
    messages <- c(
      messages,
      paste(
        "Every sequence adds B after the same duration of A exposure;",
        "the B effect may be confounded with maturation of A."
      )
    )
  }
  if (!full_rank) {
    messages <- c(
      messages,
      "The default fixed-effect design matrix is not full rank."
    )
  }
  if (!has_a_variation) {
    messages <- c(messages, "The schedule contains no estimable A contrast.")
  }
  if (design$has_b && !has_b_variation) {
    messages <- c(messages, "The schedule contains no estimable incremental B contrast.")
  }

  state_counts <- as.data.frame(
    table(period = layout$period, state = layout$state),
    stringsAsFactors = FALSE
  )
  state_counts <- state_counts[state_counts$Freq > 0L, , drop = FALSE]

  structure(
    list(
      full_rank = full_rank,
      fixed_rank = fixed_rank,
      fixed_columns = ncol(model_matrix),
      has_a_variation = has_a_variation,
      has_b_variation = has_b_variation,
      control_a_periods = control_a_periods,
      a_ab_periods = a_ab_periods,
      all_three_periods = all_three_periods,
      effective_a_periods = effective_a_periods,
      effective_b_periods = effective_b_periods,
      constant_a_to_b_lag = constant_a_to_b_lag,
      common_b_calendar_period = common_b_calendar_period,
      staggered_b = staggered_b,
      state_counts = state_counts,
      messages = unique(messages),
      delay_a = delay_a,
      delay_b = delay_b,
      design = design,
      fixed_formula = fixed_formula
    ),
    class = "sw_multistage_audit"
  )
}

#' @param x An `sw_multistage_audit` object.
#' @param ... Unused.
#' @rdname audit_multistage_design
#' @export
print.sw_multistage_audit <- function(x, ...) {
  cat("<sw_multistage_audit>\n")
  cat(sprintf("  fixed-effect rank: %d of %d (%s)\n",
              x$fixed_rank, x$fixed_columns,
              if (x$full_rank) "full rank" else "rank deficient"))
  cat("  Control/A overlap periods:",
      if (length(x$control_a_periods)) paste(x$control_a_periods, collapse = ", ") else "none",
      "\n")
  if (x$design$has_b) {
    cat("  A/A+B overlap periods:",
        if (length(x$a_ab_periods)) paste(x$a_ab_periods, collapse = ", ") else "none",
        "\n")
    cat("  staggered B starts:", if (x$staggered_b) "yes" else "no", "\n")
  }
  if (length(x$messages)) {
    cat("  cautions:\n")
    cat(paste0("    - ", x$messages, collapse = "\n"), "\n")
  } else {
    cat("  no structural cautions detected\n")
  }
  invisible(x)
}
