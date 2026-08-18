#' Compare two-state and cumulative three-state stepped-wedge designs
#'
#' Runs [power_multistage_swcrt()] for two candidate schedules, commonly a
#' `Control -> A` design and an asynchronous `Control -> A -> A+B` design. The
#' function reports the operating characteristics side by side and checks
#' whether the designs use the same number of clusters, periods, and
#' cluster-period observations.
#'
#' When the same `seed` is used, both designs start from the same reproducible
#' simulation streams. This can reduce simulation noise in a like-for-like
#' comparison when their dimensions match.
#'
#' @param design_two,design_three Two [sw_multistage_design()] objects. The
#'   first would normally have `b_start = Inf` for every sequence.
#' @param assumptions_two A [sw_multistage_assumptions()] object for the first
#'   design.
#' @param assumptions_three Assumptions for the second design. Defaults to
#'   `assumptions_two`.
#' @param labels Two display labels.
#' @inheritParams power_multistage_swcrt
#'
#' @return An object of class `"sw_multistage_comparison"` containing both
#'   power objects, a stacked comparison table, a combined fit-diagnostics
#'   table, and a resource check.
#' @examples
#' \donttest{
#' three <- sw_multistage_design(
#'   clusters_per_sequence = rep(4, 4),
#'   a_start = c(2, 3, 4, 5), b_start = c(5, 7, 6, 8),
#'   n_periods = 8
#' )
#' two <- sw_multistage_design(
#'   clusters_per_sequence = rep(4, 4),
#'   a_start = c(2, 3, 4, 5), b_start = rep(Inf, 4),
#'   n_periods = 8
#' )
#' a <- sw_multistage_assumptions(
#'   baseline_prob = 0.15, treatment_or_a = 1.4,
#'   incremental_or_b = 1.3, delay_a = 1, delay_b = 1,
#'   icc = 0.05, n_per_cluster_period = 30
#' )
#' compare_multistage_designs(two, three, a, nsim = 100, seed = 1)
#' }
#' @export
compare_multistage_designs <- function(
  design_two,
  design_three,
  assumptions_two,
  assumptions_three = assumptions_two,
  labels = c("Control -> A", "Control -> A -> A+B"),
  nsim = 1000,
  alpha = 0.05,
  multiplicity = c("holm", "bonferroni", "none"),
  fit_link = c("logit", "identity"),
  nAGQ = 1,
  analysis_args = list(),
  n_cores = 1,
  seed = NULL,
  check_design = TRUE,
  warn_on_design = TRUE
) {
  if (!inherits(design_two, "sw_multistage_design") ||
      !inherits(design_three, "sw_multistage_design")) {
    stop("Both designs must be sw_multistage_design objects.", call. = FALSE)
  }
  if (!inherits(assumptions_two, "sw_multistage_assumptions") ||
      !inherits(assumptions_three, "sw_multistage_assumptions")) {
    stop("Both assumptions objects must be sw_multistage_assumptions objects.",
         call. = FALSE)
  }
  if (length(labels) != 2L || anyNA(labels) || any(!nzchar(labels))) {
    stop("`labels` must contain two non-empty labels.", call. = FALSE)
  }
  multiplicity <- match.arg(multiplicity)
  fit_link <- match.arg(fit_link)

  first <- power_multistage_swcrt(
    design = design_two,
    assumptions = assumptions_two,
    nsim = nsim,
    alpha = alpha,
    multiplicity = multiplicity,
    fit_link = fit_link,
    nAGQ = nAGQ,
    analysis_args = analysis_args,
    n_cores = n_cores,
    seed = seed,
    check_design = check_design,
    warn_on_design = warn_on_design
  )
  second <- power_multistage_swcrt(
    design = design_three,
    assumptions = assumptions_three,
    nsim = nsim,
    alpha = alpha,
    multiplicity = multiplicity,
    fit_link = fit_link,
    nAGQ = nAGQ,
    analysis_args = analysis_args,
    n_cores = n_cores,
    seed = seed,
    check_design = check_design,
    warn_on_design = warn_on_design
  )

  comparison <- rbind(
    data.frame(design = labels[1], first$power_table,
               stringsAsFactors = FALSE),
    data.frame(design = labels[2], second$power_table,
               stringsAsFactors = FALSE)
  )
  rownames(comparison) <- NULL

  fit_diagnostics <- rbind(
    data.frame(design = labels[1], first$fit_diagnostics,
               stringsAsFactors = FALSE),
    data.frame(design = labels[2], second$fit_diagnostics,
               stringsAsFactors = FALSE)
  )
  rownames(fit_diagnostics) <- NULL

  total_sample <- c(
    .multistage_total_sample_size(design_two, assumptions_two),
    .multistage_total_sample_size(design_three, assumptions_three)
  )
  resource_check <- data.frame(
    design = labels,
    n_sequences = c(design_two$n_sequences, design_three$n_sequences),
    n_clusters = c(design_two$n_clusters, design_three$n_clusters),
    n_periods = c(design_two$n_periods, design_three$n_periods),
    cluster_period_rows = c(
      design_two$n_clusters * design_two$n_periods,
      design_three$n_clusters * design_three$n_periods
    ),
    total_individual_observations = total_sample,
    stringsAsFactors = FALSE
  )
  equal_resources <-
    length(unique(resource_check$n_clusters)) == 1L &&
    length(unique(resource_check$n_periods)) == 1L &&
    length(unique(resource_check$cluster_period_rows)) == 1L
  equal_total_sample <- if (all(is.finite(total_sample))) {
    length(unique(total_sample)) == 1L
  } else {
    NA
  }

  structure(
    list(
      comparison = comparison,
      fit_diagnostics = fit_diagnostics,
      resource_check = resource_check,
      equal_structural_resources = equal_resources,
      equal_total_sample = equal_total_sample,
      two_state = first,
      three_state = second,
      labels = labels,
      call = match.call()
    ),
    class = "sw_multistage_comparison"
  )
}

.multistage_total_sample_size <- function(design, assumptions) {
  spec <- assumptions$n_per_cluster_period
  if (is.function(spec)) return(NA_real_)
  cluster_sequence <- rep(
    seq_len(design$n_sequences),
    times = design$clusters_per_sequence
  )
  sequence_idx <- rep(cluster_sequence, each = design$n_periods)
  period <- rep(seq_len(design$n_periods), times = design$n_clusters)
  n <- .resolve_sample_size(
    spec, sequence_idx, period, design$n_sequences, design$n_periods
  )
  if (anyNA(n)) NA_real_ else sum(n)
}

#' @param x An `sw_multistage_comparison` object.
#' @param ... Unused.
#' @rdname compare_multistage_designs
#' @export
print.sw_multistage_comparison <- function(x, ...) {
  cat("<sw_multistage_comparison>\n")
  cat("Structural resource check\n")
  print(x$resource_check, row.names = FALSE)
  cat("  equal clusters/periods:",
      if (x$equal_structural_resources) "yes" else "no", "\n")
  cat("  equal total observations:",
      if (is.na(x$equal_total_sample)) "not evaluated" else
        if (x$equal_total_sample) "yes" else "no", "\n\n")

  display <- x$comparison[, c(
    "design", "label", "conditional_power", "failure_aware_power",
    "n_evaluable"
  ), drop = FALSE]
  names(display) <- c(
    "design", "test", "conditional", "failure_aware", "evaluable"
  )
  print(display, row.names = FALSE, digits = 3)
  cat("\nFit diagnostics by design\n")
  diagnostic_display <- x$fit_diagnostics[, c(
    "design", "label", "count", "rate"
  ), drop = FALSE]
  names(diagnostic_display) <- c("design", "category", "count", "rate")
  print(diagnostic_display, row.names = FALSE, digits = 3)
  cat(
    "  Note: singular fit is a non-exclusive flag and may overlap with ",
    "successful fit.\n",
    sep = ""
  )
  invisible(x)
}
