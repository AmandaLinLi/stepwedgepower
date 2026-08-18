#' Simulation-based power for a classic or batched stepped-wedge design
#'
#' Evaluates component contrasts under an analysis model using calendar time,
#' shared time on trial, or separate batch-specific time effects. Simulation
#' uses the time model stored in [sw_batched_assumptions()], while
#' `analysis_time_model` controls the fitted model. This permits deliberate
#' evaluation of time-model misspecification.
#'
#' @param design An [sw_batched_design()] object.
#' @param assumptions An [sw_batched_assumptions()] object.
#' @param analysis_time_model Time parameterization used in the fitted model.
#' @inheritParams power_component_swcrt
#' @return An object of class `sw_batched_power`, inheriting from
#'   `sw_component_power`.
#' @examples
#' \donttest{
#' d <- sw_batched_design(
#'   c(2, 2, 3, 3),
#'   rbind(
#'     c("0", "1", "1+2", "1+2", "-"),
#'     c("0", "0", "1", "1+2", "-"),
#'     c("-", "0", "1", "1+2", "1+2"),
#'     c("-", "0", "0", "1", "1+2")
#'   ),
#'   batch = c(1, 1, 2, 2)
#' )
#' a <- sw_batched_assumptions(
#'   baseline_prob = 0.15, treatment_or_a = 1.35,
#'   treatment_or_b = 1.25, interaction_mode = "none",
#'   icc = 0.05, n_per_cluster_period = 25,
#'   time_model = "calendar"
#' )
#' p <- power_batched_swcrt(
#'   d, a, nsim = 20,
#'   contrasts = c("A_vs_control", "AB_vs_A", "AB_vs_control"),
#'   include_interaction = FALSE, seed = 1, warn_on_design = FALSE
#' )
#' p
#' }
#' @export
power_batched_swcrt <- function(
  design,
  assumptions,
  analysis_time_model = c("calendar", "time_on_trial", "separate"),
  nsim = 1000,
  alpha = 0.05,
  contrasts = NULL,
  include_interaction = FALSE,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL,
  fit_link = c("logit", "identity"),
  nAGQ = 1,
  analysis_args = list(),
  n_cores = 1,
  seed = NULL,
  check_design = TRUE,
  warn_on_design = TRUE
) {
  if (!inherits(design, "sw_batched_design")) {
    stop("`design` must be an sw_batched_design object.", call. = FALSE)
  }
  if (!inherits(assumptions, "sw_batched_assumptions")) {
    stop(
      "`assumptions` must be an sw_batched_assumptions object.",
      call. = FALSE
    )
  }
  analysis_time_model <- .match_batched_time_model(analysis_time_model)
  if (is.null(contrasts)) {
    contrasts <- .batched_default_contrasts(design, include_interaction)
  }
  contrasts <- .validate_component_contrasts(contrasts, include_interaction)

  if (!is.list(analysis_args)) {
    stop("`analysis_args` must be a named list.", call. = FALSE)
  }
  audit <- if (isTRUE(check_design)) {
    audit_batched_design(
      design = design,
      assumptions = assumptions,
      time_model = analysis_time_model,
      contrasts = contrasts,
      include_interaction = include_interaction,
      adjust_sequence = isTRUE(analysis_args$adjust_sequence)
    )
  } else {
    structure(
      list(messages = character(), all_requested_estimable = NA),
      class = "sw_batched_audit"
    )
  }
  if (isTRUE(warn_on_design) && length(audit$messages)) {
    warning(
      "Batched-design audit cautions:\n- ",
      paste(audit$messages, collapse = "\n- "),
      call. = FALSE
    )
  }

  formula <- analysis_args$formula
  if (is.null(formula)) {
    prototype <- .component_sequence_period(design, assumptions)
    prototype <- .append_batched_time_columns(prototype, design)
    prototype <- prototype[prototype$observed, , drop = FALSE]
    prototype$events <- 0L
    prototype$n <- 1L
    prototype$cluster_id <- prototype$sequence_idx
    formula <- .batched_fixed_formula(
      data = prototype,
      contrasts = contrasts,
      include_interaction = include_interaction,
      time_model = analysis_time_model,
      adjust_sequence = isTRUE(analysis_args$adjust_sequence),
      include_response = TRUE
    )
    analysis_args$formula <- formula
  }
  # The formula already encodes the chosen time and sequence structure.
  analysis_args$adjust_sequence <- NULL
  analysis_args$period_effect <- NULL

  out <- power_component_swcrt(
    design = design,
    assumptions = assumptions,
    nsim = nsim,
    alpha = alpha,
    contrasts = contrasts,
    include_interaction = include_interaction,
    multiplicity = match.arg(multiplicity),
    multiplicity_family = multiplicity_family,
    fit_link = match.arg(fit_link),
    nAGQ = nAGQ,
    analysis_args = analysis_args,
    n_cores = n_cores,
    seed = seed,
    check_design = FALSE,
    warn_on_design = FALSE
  )
  out$design_audit <- audit
  out$generating_time_model <- assumptions$time_model
  out$analysis_time_model <- analysis_time_model
  out$batched_formula <- formula
  out$call <- match.call()
  class(out) <- unique(c("sw_batched_power", class(out)))
  out
}

#' Global-null rejection rates for a batched stepped-wedge design
#'
#' @inheritParams power_batched_swcrt
#' @return An `sw_batched_power` object generated under the global null.
#' @rdname power_batched_swcrt
#' @export
type1_batched_swcrt <- function(
  design,
  assumptions,
  analysis_time_model = c("calendar", "time_on_trial", "separate"),
  nsim = 1000,
  alpha = 0.05,
  contrasts = NULL,
  include_interaction = FALSE,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL,
  fit_link = c("logit", "identity"),
  nAGQ = 1,
  analysis_args = list(),
  n_cores = 1,
  seed = NULL,
  check_design = TRUE,
  warn_on_design = TRUE
) {
  if (!inherits(assumptions, "sw_batched_assumptions")) {
    stop(
      "`assumptions` must be an sw_batched_assumptions object.",
      call. = FALSE
    )
  }
  null <- assumptions
  null$treatment_effect_a <- 0
  null$treatment_or_a <- 1
  null$treatment_effect_b <- 0
  null$treatment_or_b <- 1
  null$interaction_effect <- 0
  null$interaction_or <- 1
  out <- power_batched_swcrt(
    design = design,
    assumptions = null,
    analysis_time_model = .match_batched_time_model(analysis_time_model),
    nsim = nsim,
    alpha = alpha,
    contrasts = contrasts,
    include_interaction = include_interaction,
    multiplicity = match.arg(multiplicity),
    multiplicity_family = multiplicity_family,
    fit_link = match.arg(fit_link),
    nAGQ = nAGQ,
    analysis_args = analysis_args,
    n_cores = n_cores,
    seed = seed,
    check_design = check_design,
    warn_on_design = warn_on_design
  )
  out$global_null <- TRUE
  out$call <- match.call()
  out
}

#' @param x An `sw_batched_power` object.
#' @param ... Unused.
#' @rdname power_batched_swcrt
#' @export
print.sw_batched_power <- function(x, ...) {
  cat("<sw_batched_power>\n")
  cat("  generating time model:", x$generating_time_model, "\n")
  cat("  analysis time model:", x$analysis_time_model, "\n")
  y <- x
  class(y) <- setdiff(class(y), "sw_batched_power")
  print(y)
  invisible(x)
}
