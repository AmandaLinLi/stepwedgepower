#' Power across a grid of design and assumption values
#'
#' Runs [power_swcrt()] across the Cartesian product of one or more varying
#' parameters, holding everything else fixed. This is the usual sensitivity
#' analysis for a trial proposal: power as a function of the number of clusters,
#' the assumed effect size, the ICC, and the cluster-period sample size.
#'
#' Any argument of [sw_assumptions()] may be varied by passing a vector in
#' `vary`, and the number of clusters per sequence may be varied by passing
#' `clusters_per_sequence` as a *list* of vectors. Each grid cell is an
#' independent simulation run.
#'
#' Replication is reproducible under parallelism: each cell is assigned its own
#' seed derived from `seed`, so results do not depend on `n_cores`.
#'
#' @param design An [sw_design()] object giving the fixed part of the design.
#' @param assumptions An [sw_assumptions()] object giving the fixed assumptions.
#'   Values named in `vary` override the corresponding fields.
#' @param vary A named list of vectors to vary. Names may be any
#'   [sw_assumptions()] argument (e.g. `treatment_or`, `icc`,
#'   `n_per_cluster_period`) and/or `clusters_per_sequence`, which must then be
#'   a list of integer vectors.
#' @param nsim Number of simulations per grid cell.
#' @param alpha Two-sided significance level.
#' @param n_cores Number of cores. `1` runs serially; values above 1 use
#'   [parallel::mclapply()] on Unix-alikes and a PSOCK cluster elsewhere.
#' @param seed Optional seed. Each cell derives a distinct seed from it.
#' @param ... Passed to [power_swcrt()] (e.g. `fit_link`, `nAGQ`,
#'   `analysis_args`).
#'
#' @return An object of class `"sw_power_grid"`: a data frame of results with one
#'   row per grid cell (the varied values plus `power`, `mcse`, `conf_low`,
#'   `conf_high`, `failure_rate`, `bias`, `coverage`), carrying the full
#'   [sw_power] objects in an attribute.
#' @examples
#' \donttest{
#' design <- sw_design(clusters_per_sequence = c(6, 6, 6, 6),
#'                     crossover_period = c(2, 3, 4, 5), n_periods = 5)
#' base <- sw_assumptions(baseline_prob = 0.05, treatment_or = 2, icc = 0.05,
#'                        n_per_cluster_period = 20)
#' grid <- power_grid(
#'   design, base,
#'   vary = list(treatment_or = c(1.5, 2.0), icc = c(0.01, 0.05)),
#'   nsim = 20, seed = 1
#' )
#' summary(grid)
#' }
#' @export
power_grid <- function(
  design,
  assumptions,
  vary,
  nsim = 500,
  alpha = 0.05,
  n_cores = 1,
  seed = NULL,
  ...
) {
  if (!inherits(design, "sw_design")) {
    stop("`design` must be an sw_design object.", call. = FALSE)
  }
  if (!inherits(assumptions, "sw_assumptions")) {
    stop("`assumptions` must be an sw_assumptions object.", call. = FALSE)
  }
  if (!is.list(vary) || is.null(names(vary)) || any(names(vary) == "")) {
    stop("`vary` must be a named list of vectors.", call. = FALSE)
  }

  assumption_args <- names(formals(sw_assumptions))
  allowed <- c(assumption_args, "clusters_per_sequence")
  unknown <- setdiff(names(vary), allowed)
  if (length(unknown)) {
    stop("Cannot vary unknown parameter(s): ",
         paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  if ("clusters_per_sequence" %in% names(vary) &&
      !is.list(vary$clusters_per_sequence)) {
    stop("To vary `clusters_per_sequence`, supply a list of integer vectors.",
         call. = FALSE)
  }

  # Cartesian product over indices, so list-valued entries work too.
  idx_grid <- expand.grid(
    lapply(vary, function(v) seq_along(v)),
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  n_cells <- nrow(idx_grid)

  # Distinct, order-independent seed per cell.
  cell_seeds <- if (is.null(seed)) {
    rep(list(NULL), n_cells)
  } else {
    as.list(seed + seq_len(n_cells) - 1L)
  }

  run_cell <- function(i) {
    cell <- lapply(names(vary), function(nm) vary[[nm]][[idx_grid[i, nm]]])
    names(cell) <- names(vary)

    cell_design <- design
    if (!is.null(cell$clusters_per_sequence)) {
      cell_design <- sw_design(
        clusters_per_sequence = cell$clusters_per_sequence,
        treatment = design$treatment,
        sequence_names = design$sequence_names
      )
    }

    # Rebuild assumptions from the fixed object, overriding the varied fields.
    a_args <- list(
      outcome = assumptions$outcome,
      baseline_logit = assumptions$baseline_logit,
      treatment_effect = assumptions$treatment_effect,
      cluster_sd = assumptions$cluster_sd,
      period_effects = assumptions$period_effects,
      n_per_cluster_period = assumptions$n_per_cluster_period
    )
    for (nm in setdiff(names(cell), "clusters_per_sequence")) {
      a_args[[nm]] <- cell[[nm]]
      # Overriding one parameterisation must clear the conflicting one.
      if (nm == "baseline_prob") a_args$baseline_logit <- NULL
      if (nm == "baseline_logit") a_args$baseline_prob <- NULL
      if (nm == "treatment_or") a_args$treatment_effect <- NULL
      if (nm == "treatment_effect") a_args$treatment_or <- NULL
      if (nm == "icc") a_args$cluster_sd <- NULL
      if (nm == "cluster_sd") a_args$icc <- NULL
    }
    cell_assumptions <- do.call(sw_assumptions, a_args)

    power_swcrt(
      design = cell_design, assumptions = cell_assumptions,
      nsim = nsim, alpha = alpha, seed = cell_seeds[[i]], ...
    )
  }

  results <- .grid_lapply(seq_len(n_cells), run_cell, n_cores = n_cores)

  # Assemble the summary data frame.
  label_col <- function(nm) {
    vals <- vary[[nm]]
    vapply(seq_len(n_cells), function(i) {
      v <- vals[[idx_grid[i, nm]]]
      if (length(v) == 1L) format(v) else paste(v, collapse = "/")
    }, character(1))
  }
  out <- data.frame(lapply(stats::setNames(names(vary), names(vary)), label_col),
                    stringsAsFactors = FALSE)
  # Keep genuinely scalar varied parameters numeric for plotting/sorting.
  for (nm in names(vary)) {
    vals <- vary[[nm]]
    if (!is.list(vals) && is.numeric(vals)) {
      out[[nm]] <- vals[idx_grid[[nm]]]
    }
  }

  out$power <- vapply(results, function(r) r$power, numeric(1))
  out$mcse <- vapply(results, function(r) r$mcse, numeric(1))
  out$conf_low <- vapply(results, function(r) r$conf.int[1], numeric(1))
  out$conf_high <- vapply(results, function(r) r$conf.int[2], numeric(1))
  out$failure_rate <- vapply(results, function(r) r$failure_rate, numeric(1))
  out$bias <- vapply(results, function(r) r$bias, numeric(1))
  out$coverage <- vapply(results, function(r) r$coverage, numeric(1))

  structure(
    out,
    class = c("sw_power_grid", "data.frame"),
    runs = results,
    vary = vary,
    nsim = nsim,
    alpha = alpha,
    call = match.call()
  )
}

# Serial or parallel lapply. Kept internal so power_grid() stays readable.
.grid_lapply <- function(x, fun, n_cores = 1) {
  n_cores <- max(1L, as.integer(n_cores))
  if (n_cores == 1L) {
    return(lapply(x, fun))
  }
  n_cores <- min(n_cores, parallel::detectCores(logical = FALSE), length(x))
  if (.Platform$OS.type != "windows") {
    return(parallel::mclapply(x, fun, mc.cores = n_cores))
  }
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, requireNamespace("stepwedgepower", quietly = TRUE))
  parallel::parLapply(cl, x, fun)
}

#' @param x An `sw_power_grid` object.
#' @param ... Unused.
#' @rdname power_grid
#' @export
print.sw_power_grid <- function(x, ...) {
  cat("<sw_power_grid>\n")
  cat(sprintf("  %d cells, %d simulations each, alpha = %.3f\n",
              nrow(x), attr(x, "nsim"), attr(x, "alpha")))
  cat("  varying:", paste(names(attr(x, "vary")), collapse = ", "), "\n\n")
  print(as.data.frame(x), row.names = FALSE, digits = 3)
  invisible(x)
}

#' @param object An `sw_power_grid` object.
#' @rdname power_grid
#' @export
summary.sw_power_grid <- function(object, ...) {
  print.sw_power_grid(object)
  ok <- !is.na(object$power)
  if (any(ok)) {
    best <- which.max(replace(object$power, !ok, -Inf))
    cat(sprintf("\n  highest power: %.3f", object$power[best]))
    labs <- vapply(names(attr(object, "vary")),
                   function(nm) paste0(nm, " = ", object[[nm]][best]),
                   character(1))
    cat(" at ", paste(labs, collapse = ", "), "\n", sep = "")
    fr <- max(object$failure_rate, na.rm = TRUE)
    if (fr > 0.05) {
      cat(sprintf("  note: fit-failure rate reaches %.1f%% in some cells\n",
                  100 * fr))
    }
  }
  invisible(object)
}

#' @rdname power_grid
#' @export
plot.sw_power_grid <- function(x, ...) {
  vary <- attr(x, "vary")
  numeric_vars <- names(vary)[vapply(
    names(vary),
    function(nm) !is.list(vary[[nm]]) && is.numeric(vary[[nm]]) &&
      length(vary[[nm]]) > 1L,
    logical(1)
  )]
  if (length(numeric_vars) == 0) {
    stop("plot() needs at least one numeric varied parameter.", call. = FALSE)
  }

  xvar <- numeric_vars[1]
  others <- setdiff(names(vary), xvar)
  groups <- if (length(others)) {
    interaction(as.data.frame(x)[others], drop = TRUE, sep = ", ")
  } else {
    factor(rep("all", nrow(x)))
  }
  lev <- levels(groups)
  cols <- grDevices::hcl.colors(max(2L, length(lev)), "Dark 3")[seq_along(lev)]

  graphics::plot(
    range(x[[xvar]]), c(0, 1), type = "n",
    xlab = xvar, ylab = "Power",
    main = sprintf("Power sensitivity (%d simulations per cell)", attr(x, "nsim"))
  )
  graphics::abline(h = c(0.8, 0.9), col = "grey85", lty = 3)

  for (i in seq_along(lev)) {
    sub <- x[groups == lev[i], , drop = FALSE]
    sub <- sub[order(sub[[xvar]]), , drop = FALSE]
    graphics::arrows(
      sub[[xvar]], sub$conf_low, sub[[xvar]], sub$conf_high,
      length = 0.03, angle = 90, code = 3, col = cols[i]
    )
    graphics::lines(sub[[xvar]], sub$power, col = cols[i], lwd = 2)
    graphics::points(sub[[xvar]], sub$power, col = cols[i], pch = 19)
  }
  if (length(others)) {
    graphics::legend("bottomright", legend = lev, col = cols, lwd = 2,
                     pch = 19, bty = "n",
                     title = paste(others, collapse = ", "))
  }
  invisible(x)
}
