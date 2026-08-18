#' Compare classic and batched stepped-wedge designs
#'
#' Runs [power_batched_swcrt()] for two or more batch-aware designs and combines
#' operating characteristics, fit diagnostics, time models, and resource
#' summaries. A scalar `time_models` value is recycled; otherwise provide one
#' analysis time model per design.
#'
#' @param designs Named or unnamed list of [sw_batched_design()] objects.
#' @param assumptions One [sw_batched_assumptions()] object or one per design.
#' @param time_models One analysis time model or one per design.
#' @param labels Optional display labels.
#' @inheritParams power_batched_swcrt
#' @return An object of class `sw_batched_comparison`.
#' @export
compare_batched_designs <- function(
  designs,
  assumptions,
  time_models = "calendar",
  labels = NULL,
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
  if (!is.list(designs) || length(designs) < 2L ||
      !all(vapply(designs, inherits, logical(1), what = "sw_batched_design"))) {
    stop(
      "`designs` must be a list of at least two sw_batched_design objects.",
      call. = FALSE
    )
  }
  n_designs <- length(designs)
  if (inherits(assumptions, "sw_batched_assumptions")) {
    assumptions <- rep(list(assumptions), n_designs)
  }
  if (!is.list(assumptions) || length(assumptions) != n_designs ||
      !all(vapply(
        assumptions, inherits, logical(1), what = "sw_batched_assumptions"
      ))) {
    stop(
      "`assumptions` must be one batched assumptions object or one per design.",
      call. = FALSE
    )
  }
  if (length(time_models) == 1L) time_models <- rep(time_models, n_designs)
  if (length(time_models) != n_designs) {
    stop("Supply one `time_models` value per design.", call. = FALSE)
  }
  time_models <- vapply(
    time_models, .match_batched_time_model, character(1)
  )

  if (is.null(labels)) {
    labels <- names(designs)
    if (is.null(labels) || any(!nzchar(labels))) {
      labels <- paste0("Design ", seq_len(n_designs))
    }
  }
  if (length(labels) != n_designs || anyNA(labels) || any(!nzchar(labels)) ||
      anyDuplicated(labels)) {
    stop(
      "`labels` must be unique, non-empty, and one per design.",
      call. = FALSE
    )
  }

  multiplicity <- match.arg(multiplicity)
  fit_link <- match.arg(fit_link)
  runs <- lapply(seq_len(n_designs), function(i) {
    power_batched_swcrt(
      design = designs[[i]],
      assumptions = assumptions[[i]],
      analysis_time_model = time_models[i],
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
      design = labels[i],
      analysis_time_model = time_models[i],
      runs[[i]]$power_table,
      stringsAsFactors = FALSE
    )
  }))
  rownames(comparison) <- NULL
  fit_diagnostics <- do.call(rbind, lapply(seq_len(n_designs), function(i) {
    data.frame(
      design = labels[i],
      analysis_time_model = time_models[i],
      runs[[i]]$fit_diagnostics,
      stringsAsFactors = FALSE
    )
  }))
  rownames(fit_diagnostics) <- NULL

  resource_check <- do.call(rbind, lapply(seq_len(n_designs), function(i) {
    data.frame(
      design = labels[i],
      analysis_time_model = time_models[i],
      component_resource_summary(designs[[i]], assumptions[[i]]),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }))
  rownames(resource_check) <- NULL
  total_sample <- resource_check$total_individual_observations
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
      equal_observed_cluster_periods = equal_observed_cluster_periods,
      equal_total_sample = equal_total_sample,
      runs = stats::setNames(runs, labels),
      labels = labels,
      time_models = stats::setNames(time_models, labels),
      call = match.call()
    ),
    class = "sw_batched_comparison"
  )
}

#' Compare a classic SWD with a BSWD
#'
#' Thin convenience wrapper around [compare_batched_designs()].
#'
#' @param swd A classic SWD represented by [sw_batched_design()].
#' @param bswd A batched stepped-wedge design.
#' @param assumptions One batched assumptions object or a length-two list.
#' @param swd_time_model,bswd_time_model Analysis time model for each design.
#' @param ... Additional arguments passed to [compare_batched_designs()].
#' @return An `sw_batched_comparison` object.
#' @rdname compare_batched_designs
#' @export
compare_swd_bswd <- function(
  swd,
  bswd,
  assumptions,
  swd_time_model = "calendar",
  bswd_time_model = "calendar",
  ...
) {
  compare_batched_designs(
    designs = list(SWD = swd, BSWD = bswd),
    assumptions = assumptions,
    time_models = c(swd_time_model, bswd_time_model),
    labels = c("SWD", "BSWD"),
    ...
  )
}

#' Compare the three BSWD time models
#'
#' Evaluates the same design under calendar-time, time-on-trial, and/or separate
#' time analysis models using identical simulation seeds.
#'
#' @param design An [sw_batched_design()] object.
#' @param assumptions A [sw_batched_assumptions()] object.
#' @param time_models Analysis time models to compare.
#' @param ... Additional arguments passed to [compare_batched_designs()].
#' @return An `sw_batched_comparison` object.
#' @rdname compare_batched_designs
#' @export
compare_batched_time_models <- function(
  design,
  assumptions,
  time_models = c("calendar", "time_on_trial", "separate"),
  ...
) {
  time_models <- vapply(
    time_models, .match_batched_time_model, character(1)
  )
  labels <- c(
    calendar = "Calendar time",
    time_on_trial = "Time on trial",
    separate = "Separate time"
  )[time_models]
  compare_batched_designs(
    designs = rep(list(design), length(time_models)),
    assumptions = assumptions,
    time_models = time_models,
    labels = unname(labels),
    ...
  )
}

#' @param x An `sw_batched_comparison` object.
#' @param ... Unused.
#' @rdname compare_batched_designs
#' @export
print.sw_batched_comparison <- function(x, ...) {
  cat("<sw_batched_comparison>\n")
  cat("Resource check\n")
  print(x$resource_check, row.names = FALSE)
  cat(
    "  equal observed cluster-periods:",
    if (x$equal_observed_cluster_periods) "yes" else "no", "\n"
  )
  cat(
    "  equal total observations:",
    if (is.na(x$equal_total_sample)) {
      "not evaluated"
    } else if (x$equal_total_sample) {
      "yes"
    } else {
      "no"
    },
    "\n\n"
  )
  display <- x$comparison[, c(
    "design", "analysis_time_model", "label", "conditional_power",
    "failure_aware_power", "n_evaluable"
  ), drop = FALSE]
  names(display) <- c(
    "design", "time_model", "test", "conditional", "failure_aware",
    "evaluable"
  )
  print(display, row.names = FALSE, digits = 3)
  invisible(x)
}
