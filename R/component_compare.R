#' Compare component-based stepped-wedge designs
#'
#' Runs [power_component_swcrt()] for two or more candidate Control/A/B/A+B
#' schedules and places their operating characteristics and resource totals in
#' a common table. Calendar and observed cluster-period totals are reported
#' separately, allowing complete and structurally incomplete designs to be
#' compared under equal observed resources. The same simulation seed is used
#' for each design.
#'
#' @param designs A named or unnamed list of [sw_component_design()] objects.
#' @param assumptions One [sw_component_assumptions()] object used for every
#'   design, or a list with one assumptions object per design.
#' @param labels Optional display labels. Names of `designs` are used when
#'   available.
#' @inheritParams power_component_swcrt
#'
#' @return An object of class `"sw_component_comparison"`. The
#'   `fit_diagnostics` element combines the detailed fit-category counts from
#'   every candidate design.
#' @examples
#' \donttest{
#' four <- sw_component_design(
#'   rep(3, 4),
#'   state = rbind(
#'     c(0, 1, 1, 3, 3), c(0, 0, 2, 2, 3),
#'     c(0, 0, 1, 3, 3), c(0, 0, 2, 3, 3)
#'   )
#' )
#' additive <- four
#' a <- sw_component_assumptions(
#'   baseline_prob = 0.15, treatment_or_a = 1.5,
#'   treatment_or_b = 1.4, interaction_or = 1.1,
#'   icc = 0.05, n_per_cluster_period = 30
#' )
#' compare_component_designs(
#'   list(`Four-state design` = four, `Same design` = additive),
#'   a, nsim = 10, seed = 1, warn_on_design = FALSE
#' )
#' }
#' @export
compare_component_designs <- function(
  designs,
  assumptions,
  labels = NULL,
  nsim = 1000,
  alpha = 0.05,
  contrasts = NULL,
  include_interaction = TRUE,
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
  if (!is.list(designs) || length(designs) < 2L ||
      !all(vapply(designs, inherits, logical(1), what = "sw_component_design"))) {
    stop("`designs` must be a list of at least two sw_component_design objects.",
         call. = FALSE)
  }
  n_designs <- length(designs)

  if (inherits(assumptions, "sw_component_assumptions")) {
    assumptions <- rep(list(assumptions), n_designs)
  }
  if (!is.list(assumptions) || length(assumptions) != n_designs ||
      !all(vapply(assumptions, inherits, logical(1),
                  what = "sw_component_assumptions"))) {
    stop("`assumptions` must be one assumptions object or one per design.",
         call. = FALSE)
  }

  if (is.null(labels)) {
    labels <- names(designs)
    if (is.null(labels) || any(!nzchar(labels))) {
      labels <- paste0("Design ", seq_len(n_designs))
    }
  }
  if (length(labels) != n_designs || anyNA(labels) || any(!nzchar(labels)) ||
      anyDuplicated(labels)) {
    stop("`labels` must contain one unique, non-empty label per design.",
         call. = FALSE)
  }

  multiplicity <- match.arg(multiplicity)
  fit_link <- match.arg(fit_link)
  runs <- lapply(seq_len(n_designs), function(i) {
    power_component_swcrt(
      design = designs[[i]],
      assumptions = assumptions[[i]],
      nsim = nsim,
      alpha = alpha,
      contrasts = contrasts,
      include_interaction = include_interaction,
      multiplicity = multiplicity,
      multiplicity_family = multiplicity_family,
      fit_link = fit_link,
      nAGQ = nAGQ,
      analysis_args = analysis_args,
      n_cores = n_cores,
      seed = seed,
      check_design = check_design,
      warn_on_design = warn_on_design
    )
  })

  comparison <- do.call(rbind, lapply(seq_len(n_designs), function(i) {
    data.frame(
      design = labels[i], runs[[i]]$power_table,
      stringsAsFactors = FALSE
    )
  }))
  rownames(comparison) <- NULL

  fit_diagnostics <- do.call(rbind, lapply(seq_len(n_designs), function(i) {
    data.frame(
      design = labels[i], runs[[i]]$fit_diagnostics,
      stringsAsFactors = FALSE
    )
  }))
  rownames(fit_diagnostics) <- NULL

  resource_rows <- lapply(seq_len(n_designs), function(i) {
    component_resource_summary(designs[[i]], assumptions[[i]])
  })
  resource_check <- do.call(rbind, resource_rows)
  resource_check <- data.frame(
    design = labels,
    resource_check,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Retain the original field name for backward compatibility. It counts all
  # calendar cluster-period rows, whereas `observed_cluster_periods` excludes
  # structurally unobserved cells.
  resource_check$cluster_period_rows <-
    resource_check$calendar_cluster_periods
  resource_check <- resource_check[, c(
    "design", "n_sequences", "n_clusters", "n_periods",
    "cluster_period_rows", "observed_cluster_periods",
    "missing_cluster_periods", "observed_sequence_periods",
    "missing_sequence_periods", "observed_fraction",
    "total_individual_observations"
  ), drop = FALSE]

  total_sample <- resource_check$total_individual_observations
  equal_structural_resources <-
    length(unique(resource_check$n_clusters)) == 1L &&
    length(unique(resource_check$n_periods)) == 1L &&
    length(unique(resource_check$cluster_period_rows)) == 1L
  equal_observed_cluster_periods <-
    length(unique(resource_check$observed_cluster_periods)) == 1L
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
      equal_structural_resources = equal_structural_resources,
      equal_observed_cluster_periods = equal_observed_cluster_periods,
      equal_total_sample = equal_total_sample,
      runs = stats::setNames(runs, labels),
      labels = labels,
      call = match.call()
    ),
    class = "sw_component_comparison"
  )
}

#' @param x An `sw_component_comparison` object.
#' @param ... Unused.
#' @rdname compare_component_designs
#' @export
print.sw_component_comparison <- function(x, ...) {
  cat("<sw_component_comparison>\n")
  cat("Resource check\n")
  print(x$resource_check, row.names = FALSE)
  cat("  equal calendar structure:",
      if (x$equal_structural_resources) "yes" else "no", "\n")
  cat("  equal observed cluster-periods:",
      if (isTRUE(x$equal_observed_cluster_periods)) "yes" else "no", "\n")
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
