#' Simulate a classic or batched stepped-wedge trial
#'
#' Simulates aggregated binary outcomes from a [sw_batched_design()] using a
#' [sw_batched_assumptions()] object. The returned data include `batch`,
#' `calendar_period`, and `time_on_trial` in addition to the component-based
#' treatment-history columns.
#'
#' @param design An [sw_batched_design()] object.
#' @param assumptions An [sw_batched_assumptions()] object.
#' @param seed Optional random seed.
#' @param include_unobserved Retain latent unobserved rows with missing
#'   outcomes.
#' @return A data frame of cluster-period outcomes.
#' @export
simulate_batched_swcrt <- function(
  design,
  assumptions,
  seed = NULL,
  include_unobserved = FALSE
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
  simulate_component_swcrt(
    design = design,
    assumptions = assumptions,
    seed = seed,
    include_unobserved = include_unobserved
  )
}
