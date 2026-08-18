#' Audit a classic or batched stepped-wedge design
#'
#' Audits observed treatment support, batch initiation, fixed-effect rank, and
#' estimability under a calendar-time, time-on-trial, or separate-time analysis
#' model. Structurally unobserved cells are excluded from the model matrix.
#'
#' @param design An [sw_batched_design()] object.
#' @param assumptions Optional [sw_component_assumptions()] or
#'   [sw_batched_assumptions()] object.
#' @param time_model Planned analysis time parameterization.
#' @param contrasts Requested standard component contrasts.
#' @param include_interaction Include the A-by-B interaction. Defaults to
#'   `FALSE`; use `TRUE` for four-state designs with adequate support.
#' @param adjust_sequence Include fixed sequence effects.
#' @return An object of class `sw_batched_audit`.
#' @export
audit_batched_design <- function(
  design,
  assumptions = NULL,
  time_model = c("calendar", "time_on_trial", "separate"),
  contrasts = NULL,
  include_interaction = FALSE,
  adjust_sequence = FALSE
) {
  if (!inherits(design, "sw_batched_design")) {
    stop("`design` must be an sw_batched_design object.", call. = FALSE)
  }
  if (!is.null(assumptions) &&
      !inherits(assumptions, "sw_component_assumptions")) {
    stop(
      "`assumptions` must be NULL or inherit from sw_component_assumptions.",
      call. = FALSE
    )
  }
  time_model <- .match_batched_time_model(time_model)
  if (is.null(contrasts)) {
    contrasts <- .batched_default_contrasts(design, include_interaction)
  }
  contrasts <- .validate_component_contrasts(contrasts, include_interaction)

  full_grid <- .component_sequence_period(design, assumptions)
  full_grid <- .append_batched_time_columns(full_grid, design)
  grid <- full_grid[full_grid$observed, , drop = FALSE]
  if (!nrow(grid)) {
    stop("The design has no observed sequence-period cells.", call. = FALSE)
  }

  formula <- .batched_fixed_formula(
    data = grid,
    contrasts = contrasts,
    include_interaction = include_interaction,
    time_model = time_model,
    adjust_sequence = adjust_sequence,
    include_response = FALSE
  )
  model_matrix <- tryCatch(
    stats::model.matrix(formula, data = grid),
    error = function(e) e
  )
  if (inherits(model_matrix, "error")) {
    stop(
      "Could not construct the batched fixed-effect model matrix: ",
      conditionMessage(model_matrix),
      call. = FALSE
    )
  }
  qr_matrix <- qr(model_matrix)
  full_rank <- qr_matrix$rank == ncol(model_matrix)

  contrast_matrix <- .component_contrast_matrix(include_interaction)
  term_names <- c(
    a = "a_effect_weight",
    b = "b_effect_weight",
    ab = "ab_effect_weight"
  )
  estimability <- vapply(contrasts, function(name) {
    linear <- stats::setNames(rep(0, ncol(model_matrix)), colnames(model_matrix))
    weights <- contrast_matrix[name, ]
    for (component in names(weights)) {
      if (weights[[component]] == 0) next
      term <- term_names[[component]]
      if (!term %in% names(linear)) return(FALSE)
      linear[[term]] <- weights[[component]]
    }
    .component_is_estimable(linear, model_matrix)
  }, logical(1))
  estimability_table <- data.frame(
    contrast = contrasts,
    label = unname(.component_contrast_labels()[contrasts]),
    estimable = estimability,
    stringsAsFactors = FALSE
  )

  base_audit <- audit_component_design(
    design = design,
    assumptions = assumptions,
    contrasts = contrasts,
    include_interaction = include_interaction,
    adjust_sequence = FALSE,
    period_effect = "none"
  )
  messages <- base_audit$messages
  messages <- messages[
    !grepl("^The observed-data fixed-effect design matrix", messages) &
      !grepl("^Non-estimable requested contrast", messages)
  ]
  if (!full_rank) {
    messages <- c(
      messages,
      sprintf(
        "The %s-time fixed-effect design matrix has rank %d of %d.",
        gsub("_", "-", time_model), qr_matrix$rank, ncol(model_matrix)
      )
    )
  }
  nonestimable <- estimability_table$contrast[!estimability_table$estimable]
  if (length(nonestimable)) {
    messages <- c(
      messages,
      paste0(
        "Non-estimable requested contrast(s) under the ",
        gsub("_", "-", time_model), " time model: ",
        paste(nonestimable, collapse = ", "), "."
      )
    )
  }

  batch_table <- data.frame(
    batch = design$batch_names,
    start_period = design$batch_start_period,
    delay_from_first = design$batch_delay,
    gap_from_previous = design$batch_gap_from_previous,
    end_period = design$batch_end_period,
    sequences = design$sequences_per_batch,
    clusters = design$clusters_per_batch,
    stringsAsFactors = FALSE
  )
  grid$cluster_weight <- design$clusters_per_sequence[grid$sequence_idx]
  time_support <- stats::xtabs(
    cluster_weight ~
      factor(batch, levels = design$batch_names) + factor(time_on_trial),
    data = grid
  )

  interpretation <- switch(
    time_model,
    calendar = paste(
      "Calendar-period effects are shared across batches; this assumes a",
      "common secular trend on the calendar scale."
    ),
    time_on_trial = paste(
      "Time-on-trial effects are shared across batches and indexed from each",
      "batch's initiation."
    ),
    separate = paste(
      "Each batch has its own categorical time-on-trial profile, allowing",
      "batch-specific secular effects."
    )
  )

  structure(
    list(
      time_model = time_model,
      interpretation = interpretation,
      formula = formula,
      full_rank = full_rank,
      rank = qr_matrix$rank,
      n_columns = ncol(model_matrix),
      estimability = estimability_table,
      all_requested_estimable = all(estimability),
      batch_table = batch_table,
      time_support = time_support,
      base_component_audit = base_audit,
      observed_cluster_periods = base_audit$observed_cluster_periods,
      calendar_cluster_periods = base_audit$calendar_cluster_periods,
      missing_cluster_periods = base_audit$missing_cluster_periods,
      messages = unique(messages),
      design = design,
      assumptions = assumptions
    ),
    class = "sw_batched_audit"
  )
}

#' @param x An `sw_batched_audit` object.
#' @param ... Unused.
#' @rdname audit_batched_design
#' @export
print.sw_batched_audit <- function(x, ...) {
  cat("<sw_batched_audit>\n")
  cat("  time model:", x$time_model, "\n")
  cat("  ", x$interpretation, "\n", sep = "")
  cat(sprintf(
    "  fixed-effect rank: %d of %d (%s)\n",
    x$rank, x$n_columns, if (x$full_rank) "full" else "deficient"
  ))
  cat(sprintf(
    "  observed cluster-periods: %d of %d; structurally unobserved: %d\n",
    x$observed_cluster_periods,
    x$calendar_cluster_periods,
    x$missing_cluster_periods
  ))
  cat("  batches:\n")
  print(x$batch_table, row.names = FALSE)
  cat("  requested contrasts:\n")
  print(x$estimability, row.names = FALSE)
  if (length(x$messages)) {
    cat("  cautions:\n")
    cat(paste0("  - ", x$messages, collapse = "\n"), "\n")
  } else {
    cat("  no structural cautions detected\n")
  }
  invisible(x)
}
