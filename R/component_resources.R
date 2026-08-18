#' Summarize resources for a component-based design
#'
#' Counts calendar and observed sequence-periods and cluster-periods. For a
#' structurally incomplete design, cells marked as unobserved are omitted from
#' observed-resource totals. When `assumptions` is supplied, the function also
#' calculates the total number of individual observations implied by the
#' cluster-period sample-size specification.
#'
#' @param design An [sw_component_design()] object.
#' @param assumptions Optional [sw_component_assumptions()] object.
#'
#' @return A one-row data frame with calendar and observed resource totals.
#' @examples
#' design <- sw_incomplete_component_design(
#'   clusters_per_sequence = c(2, 2, 3, 3),
#'   state = rbind(
#'     c("0", "1", "1+2", "1+2", "-"),
#'     c("0", "0", "1", "1+2", "-"),
#'     c("-", "0", "1", "1+2", "1+2"),
#'     c("-", "0", "0", "1", "1+2")
#'   )
#' )
#' component_resource_summary(design)
#' @export
component_resource_summary <- function(design, assumptions = NULL) {
  if (!inherits(design, "sw_component_design")) {
    stop("`design` must be an sw_component_design object.", call. = FALSE)
  }
  if (!is.null(assumptions) &&
      !inherits(assumptions, "sw_component_assumptions")) {
    stop("`assumptions` must be NULL or an sw_component_assumptions object.",
         call. = FALSE)
  }

  observed <- .component_observed_matrix(design)
  calendar_sequence_periods <- design$n_sequences * design$n_periods
  observed_sequence_periods <- sum(observed)
  calendar_cluster_periods <- design$n_clusters * design$n_periods
  observed_cluster_periods <- .component_observed_cluster_period_count(design)

  total <- if (is.null(assumptions)) {
    NA_real_
  } else {
    .component_total_sample_size(design, assumptions)
  }

  data.frame(
    n_sequences = as.integer(design$n_sequences),
    n_clusters = as.integer(design$n_clusters),
    n_periods = as.integer(design$n_periods),
    calendar_sequence_periods = as.integer(calendar_sequence_periods),
    observed_sequence_periods = as.integer(observed_sequence_periods),
    missing_sequence_periods = as.integer(
      calendar_sequence_periods - observed_sequence_periods
    ),
    calendar_cluster_periods = as.integer(calendar_cluster_periods),
    observed_cluster_periods = as.integer(observed_cluster_periods),
    missing_cluster_periods = as.integer(
      calendar_cluster_periods - observed_cluster_periods
    ),
    observed_fraction = observed_cluster_periods / calendar_cluster_periods,
    total_individual_observations = total,
    stringsAsFactors = FALSE
  )
}
