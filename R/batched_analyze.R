#' Fit a classic or batched stepped-wedge analysis model
#'
#' Fits the component-based binomial mixed model under one of the three time
#' parameterizations used for batched stepped-wedge designs:
#'
#' * `"calendar"`: categorical calendar-period effects shared across batches;
#' * `"time_on_trial"`: categorical relative-time effects shared across
#'   batches;
#' * `"separate"`: batch-specific categorical time-on-trial effects.
#'
#' The separate time model uses one categorical batch-by-time-on-trial factor,
#' giving each batch its own time profile and supporting unequal observed local
#' lengths. Estimation, component contrasts, multiplicity adjustment, and fit
#' diagnostics are delegated to [fit_component_model()].
#'
#' @param data Aggregated cluster-period data from [simulate_batched_swcrt()].
#' @param time_model Analysis time parameterization.
#' @param formula Optional custom mixed-model formula. When supplied, it
#'   overrides the formula generated from `time_model`.
#' @param outcome Length-two character vector `c(events, n)`.
#' @param contrasts Standard component contrasts. When `NULL`, contrasts are
#'   selected from observed treatment states when the originating design is
#'   available as an attribute.
#' @param include_interaction Fit an A-by-B interaction. Defaults to `FALSE`
#'   because common cumulative SWD/BSWD schedules contain Control, A, and A+B
#'   but no B-only state; set to `TRUE` only when the interaction is supported.
#' @param multiplicity Multiplicity method.
#' @param multiplicity_family Contrasts forming the multiplicity family.
#' @param adjust_sequence Include a fixed sequence effect.
#' @param link Binomial link.
#' @param nAGQ Adaptive Gauss-Hermite quadrature points.
#' @param alpha Two-sided significance level.
#' @return The result from [fit_component_model()] with batched-time metadata.
#' @export
fit_batched_model <- function(
  data,
  time_model = c("calendar", "time_on_trial", "separate"),
  formula = NULL,
  outcome = c("events", "n"),
  contrasts = NULL,
  include_interaction = FALSE,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL,
  adjust_sequence = FALSE,
  link = c("logit", "identity"),
  nAGQ = 1,
  alpha = 0.05
) {
  time_model <- .match_batched_time_model(time_model)
  design <- attr(data, "component_design")
  if (inherits(design, "sw_batched_design")) {
    data <- .append_batched_time_columns(data, design)
  }
  .check_required_columns(
    data,
    c("batch", "calendar_period", "time_on_trial", "sequence", "cluster_id")
  )
  if (!"batch_period" %in% names(data)) {
    data$batch_period <- ifelse(
      is.na(data$time_on_trial),
      NA_character_,
      paste0(data$batch, ":", data$time_on_trial)
    )
  }

  if (is.null(contrasts)) {
    if (inherits(design, "sw_batched_design")) {
      contrasts <- .batched_default_contrasts(design, include_interaction)
    } else {
      contrasts <- rownames(.component_contrast_matrix(include_interaction))
    }
  }
  contrasts <- .validate_component_contrasts(contrasts, include_interaction)

  if (is.null(formula)) {
    formula <- .batched_fixed_formula(
      data = data,
      contrasts = contrasts,
      include_interaction = include_interaction,
      time_model = time_model,
      adjust_sequence = adjust_sequence,
      include_response = TRUE,
      outcome = outcome,
      cluster = "cluster_id"
    )
  }

  out <- fit_component_model(
    data = data,
    formula = formula,
    outcome = outcome,
    include_interaction = include_interaction,
    contrasts = contrasts,
    multiplicity = match.arg(multiplicity),
    multiplicity_family = multiplicity_family,
    link = match.arg(link),
    nAGQ = nAGQ,
    alpha = alpha
  )
  out$time_model <- time_model
  out$batched_formula <- formula
  class(out) <- unique(c("sw_batched_fit", class(out)))
  out
}
