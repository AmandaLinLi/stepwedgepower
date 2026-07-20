#' Audit a component-based stepped-wedge design
#'
#' Examines structural support for Control, A, B, and A+B contrasts before a
#' simulation study is run. The audit reports treatment-state overlap by
#' calendar period, withdrawals and restarts, fixed-effect rank, and formal
#' estimability of the requested contrasts under the default analysis model.
#'
#' @param design An [sw_component_design()] object.
#' @param assumptions Optional [sw_component_assumptions()] object. Supplying it
#'   makes the rank check use the planned wash-in and carryover weights.
#' @param contrasts Requested standard contrasts.
#' @param include_interaction Logical; include an A-by-B interaction term.
#' @param adjust_sequence Logical; include sequence fixed effects in the audit.
#' @param period_effect One of `"categorical"`, `"linear"`, or `"none"`.
#'
#' @return An object of class `"sw_component_audit"`.
#' @examples
#' state <- rbind(
#'   S1 = c(0, 1, 1, 3, 3, 2),
#'   S2 = c(0, 0, 2, 2, 3, 3),
#'   S3 = c(0, 0, 1, 1, 3, 3),
#'   S4 = c(0, 0, 2, 3, 3, 3)
#' )
#' d <- sw_component_design(rep(3, 4), state = state)
#' audit_component_design(d)
#' @export
audit_component_design <- function(
  design,
  assumptions = NULL,
  contrasts = NULL,
  include_interaction = TRUE,
  adjust_sequence = FALSE,
  period_effect = c("categorical", "linear", "none")
) {
  if (!inherits(design, "sw_component_design")) {
    stop("`design` must be an sw_component_design object.", call. = FALSE)
  }
  if (!is.null(assumptions) &&
      !inherits(assumptions, "sw_component_assumptions")) {
    stop("`assumptions` must be NULL or an sw_component_assumptions object.",
         call. = FALSE)
  }
  period_effect <- match.arg(period_effect)
  include_interaction <- isTRUE(include_interaction)
  contrasts <- .validate_component_contrasts(contrasts, include_interaction)

  grid <- .component_sequence_period(design, assumptions)
  rhs <- c("a_effect_weight", "b_effect_weight")
  if (include_interaction) rhs <- c(rhs, "ab_effect_weight")
  if (period_effect == "categorical") {
    rhs <- c(rhs, "factor(period)")
  } else if (period_effect == "linear") {
    rhs <- c(rhs, "period")
  }
  if (isTRUE(adjust_sequence) && design$n_sequences > 1L) {
    rhs <- c(rhs, "factor(sequence)")
  }
  formula <- stats::as.formula(paste("~", paste(rhs, collapse = " + ")))
  model_matrix <- stats::model.matrix(formula, data = grid)
  qr_matrix <- qr(model_matrix)
  full_rank <- qr_matrix$rank == ncol(model_matrix)

  contrast_matrix <- .component_contrast_matrix(include_interaction)
  term_names <- c(a = "a_effect_weight", b = "b_effect_weight",
                  ab = "ab_effect_weight")
  estimability <- lapply(contrasts, function(name) {
    linear <- rep(0, ncol(model_matrix))
    names(linear) <- colnames(model_matrix)
    weights <- contrast_matrix[name, ]
    for (component in names(weights)) {
      term <- term_names[[component]]
      if (weights[[component]] != 0 && term %in% names(linear)) {
        linear[[term]] <- weights[[component]]
      } else if (weights[[component]] != 0 && !term %in% names(linear)) {
        return(FALSE)
      }
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
  periods_with <- function(required) {
    as.integer(names(states_by_period)[vapply(
      states_by_period,
      function(x) all(required %in% unique(x)),
      logical(1)
    )])
  }
  overlap <- list(
    control_a = periods_with(c("Control", "A")),
    control_b = periods_with(c("Control", "B")),
    control_ab = periods_with(c("Control", "A+B")),
    a_b = periods_with(c("A", "B")),
    a_ab = periods_with(c("A", "A+B")),
    b_ab = periods_with(c("B", "A+B")),
    all_four = periods_with(c("Control", "A", "B", "A+B"))
  )

  state_counts <- table(
    factor(grid$state, levels = design$stage_names),
    grid$period
  )
  component_variation <- c(
    A = length(unique(grid$a_effect_weight)) > 1L,
    B = length(unique(grid$b_effect_weight)) > 1L,
    interaction = length(unique(grid$ab_effect_weight)) > 1L
  )

  messages <- character()
  state_present <- c(
    Control = design$has_control,
    A = design$has_a_only,
    B = design$has_b_only,
    `A+B` = design$has_ab
  )
  missing_states <- names(state_present)[!state_present]
  if (length(missing_states)) {
    messages <- c(
      messages,
      paste0("No assigned observations occur under: ",
             paste(missing_states, collapse = ", "), ".")
    )
  }
  if (!full_rank) {
    messages <- c(
      messages,
      sprintf("The default fixed-effect design matrix has rank %d of %d.",
              qr_matrix$rank, ncol(model_matrix))
    )
  }
  nonestimable <- estimability_table$contrast[!estimability_table$estimable]
  if (length(nonestimable)) {
    messages <- c(
      messages,
      paste0("Non-estimable requested contrast(s): ",
             paste(nonestimable, collapse = ", "), ".")
    )
  }
  if (!length(overlap$control_a)) {
    messages <- c(messages,
                  "No calendar period contains both Control and A-only sequences.")
  }
  if (!length(overlap$control_b)) {
    messages <- c(messages,
                  "No calendar period contains both Control and B-only sequences.")
  }
  if (!length(overlap$a_ab)) {
    messages <- c(messages,
                  "No calendar period contains both A-only and A+B sequences.")
  }
  if (!length(overlap$b_ab)) {
    messages <- c(messages,
                  "No calendar period contains both B-only and A+B sequences.")
  }

  if (!is.null(assumptions)) {
    if (design$has_withdrawal_a &&
        assumptions$carryover_periods_a == 0L) {
      messages <- c(
        messages,
        "A is withdrawn in the schedule and is assumed to lose its effect immediately."
      )
    }
    if (design$has_withdrawal_b &&
        assumptions$carryover_periods_b == 0L) {
      messages <- c(
        messages,
        "B is withdrawn in the schedule and is assumed to lose its effect immediately."
      )
    }
    if (!design$has_withdrawal_a && assumptions$carryover_periods_a > 0L) {
      messages <- c(messages,
                    "A carryover was specified, but A is never withdrawn.")
    }
    if (!design$has_withdrawal_b && assumptions$carryover_periods_b > 0L) {
      messages <- c(messages,
                    "B carryover was specified, but B is never withdrawn.")
    }
  }

  structure(
    list(
      full_rank = full_rank,
      rank = qr_matrix$rank,
      n_columns = ncol(model_matrix),
      formula = formula,
      estimability = estimability_table,
      all_requested_estimable = all(estimability_table$estimable),
      state_counts = state_counts,
      overlap_periods = overlap,
      component_variation = component_variation,
      withdrawals = c(A = design$withdrawal_count_a,
                      B = design$withdrawal_count_b),
      restarts = c(A = design$restart_count_a,
                   B = design$restart_count_b),
      messages = unique(messages),
      design = design,
      assumptions = assumptions
    ),
    class = "sw_component_audit"
  )
}

#' @param x An `sw_component_audit` object.
#' @param ... Unused.
#' @rdname audit_component_design
#' @export
print.sw_component_audit <- function(x, ...) {
  cat("<sw_component_audit>\n")
  cat(sprintf("  fixed-effect rank: %d of %d (%s)\n",
              x$rank, x$n_columns, if (x$full_rank) "full" else "deficient"))
  cat("  requested contrasts:\n")
  print(x$estimability, row.names = FALSE)
  cat(sprintf("  withdrawals: A = %d, B = %d; restarts: A = %d, B = %d\n",
              x$withdrawals[["A"]], x$withdrawals[["B"]],
              x$restarts[["A"]], x$restarts[["B"]]))
  if (length(x$messages)) {
    cat("  cautions:\n")
    cat(paste0("  - ", x$messages, collapse = "\n"), "\n")
  } else {
    cat("  no structural cautions detected\n")
  }
  invisible(x)
}
