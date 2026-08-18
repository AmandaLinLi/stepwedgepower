#' Specify assumptions for a batched stepped-wedge trial
#'
#' Extends [sw_component_assumptions()] with an explicit data-generating time
#' model. The supported parameterizations are:
#'
#' * `"calendar"`: one secular-effect profile indexed by calendar period;
#' * `"time_on_trial"`: one profile indexed by relative time since batch
#'   initiation and shared across batches;
#' * `"separate"`: one time-on-trial profile for each batch.
#'
#' `time_effects` are specified on the model's log-odds scale. For the separate
#' time model, supply a numeric matrix with one row per batch or a list with one
#' numeric vector per batch. Batch names may be used as row or list names.
#'
#' @param ... Arguments passed to [sw_component_assumptions()]. Do not supply
#'   `period_effects`; use `time_effects` instead.
#' @param time_model Data-generating time parameterization: `"calendar"`,
#'   `"time_on_trial"`, or `"separate"`.
#' @param time_effects Time-effect profile on the log-odds scale, or `NULL` for
#'   no time effect.
#'
#' @return An object of class `sw_batched_assumptions`, inheriting from
#'   `sw_component_assumptions`.
#' @examples
#' assumptions <- sw_batched_assumptions(
#'   baseline_prob = 0.15,
#'   treatment_or_a = 1.35,
#'   treatment_or_b = 1.25,
#'   interaction_mode = "none",
#'   icc = 0.05,
#'   n_per_cluster_period = 25,
#'   time_model = "calendar",
#'   time_effects = log(seq(1, 1.08, length.out = 5))
#' )
#' assumptions
#' @export
sw_batched_assumptions <- function(
  ...,
  time_model = c("calendar", "time_on_trial", "separate"),
  time_effects = NULL
) {
  time_model <- .match_batched_time_model(time_model)
  dots <- list(...)
  if ("period_effects" %in% names(dots) && !is.null(dots$period_effects)) {
    stop(
      "Use `time_effects` rather than `period_effects` in ",
      "`sw_batched_assumptions()`.",
      call. = FALSE
    )
  }
  dots$period_effects <- NULL
  base <- do.call(sw_component_assumptions, dots)
  base$time_model <- time_model
  base$time_effects <- .validate_batched_time_effects(
    time_model, time_effects
  )
  class(base) <- unique(c("sw_batched_assumptions", class(base)))
  base
}

#' @param x An `sw_batched_assumptions` object.
#' @param ... Unused.
#' @rdname sw_batched_assumptions
#' @export
print.sw_batched_assumptions <- function(x, ...) {
  cat("<sw_batched_assumptions>\n")
  cat("  generating time model:", x$time_model, "\n")
  if (is.null(x$time_effects)) {
    cat("  time effects: none\n")
  } else if (is.matrix(x$time_effects)) {
    cat(sprintf(
      "  time effects: %d batch profile(s) by %d time point(s)\n",
      nrow(x$time_effects), ncol(x$time_effects)
    ))
  } else if (is.list(x$time_effects)) {
    cat(
      "  time effects: separate profiles with lengths ",
      paste(vapply(x$time_effects, length, integer(1)), collapse = ", "),
      "\n", sep = ""
    )
  } else {
    cat(
      "  time effects:",
      paste(sprintf("%.3f", x$time_effects), collapse = ", "), "\n"
    )
  }
  cat(sprintf("  A component OR = %.3f\n", x$treatment_or_a))
  cat(sprintf("  B component OR = %.3f\n", x$treatment_or_b))
  cat(sprintf("  A-by-B interaction OR = %.3f\n", x$interaction_or))
  cat(sprintf("  latent ICC = %.3f\n", x$icc))
  invisible(x)
}
