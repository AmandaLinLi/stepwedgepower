#' Construct a stepped-wedge design
#'
#' Builds a stepped-wedge design object describing how clusters are grouped into
#' sequences and when each sequence crosses from control to intervention. The
#' core of the object is a 0/1 treatment matrix with one row per sequence and
#' one column per period.
#'
#' A design may be specified in either of two ways:
#'
#' * **By crossover period.** Supply `crossover_period[s]`, the period at which
#'   sequence `s` switches on (and stays on). Periods before the earliest
#'   crossover are baseline periods, so more than one baseline period is
#'   naturally supported.
#' * **By an explicit `treatment` matrix** of 0/1 indicators (rows = sequences,
#'   columns = periods). This permits incomplete, custom, or simultaneously
#'   crossing schedules.
#'
#' Clusters are assigned to sequences via `clusters_per_sequence`, which may be
#' unequal across sequences.
#'
#' @param clusters_per_sequence Integer vector; number of clusters in each
#'   sequence.
#' @param crossover_period Integer vector, one per sequence, giving the period at
#'   which each sequence turns on. Use `Inf` (or a value greater than
#'   `n_periods`) for a sequence that never crosses. Ignored if `treatment` is
#'   supplied.
#' @param n_periods Number of periods. If `NULL`, inferred from
#'   `crossover_period` or the columns of `treatment`.
#' @param treatment Optional 0/1 matrix (sequences x periods). Overrides
#'   `crossover_period`.
#' @param sequence_names Optional labels for sequences.
#'
#' @return An object of class `"sw_design"`.
#' @examples
#' # Classic complete design: 4 sequences, 5 periods, one crosses each period.
#' sw_design(
#'   clusters_per_sequence = c(10, 10, 10, 10),
#'   crossover_period = c(2, 3, 4, 5),
#'   n_periods = 5
#' )
#'
#' # Custom schedule via an explicit matrix (two baseline periods).
#' m <- matrix(c(0, 0, 1, 1, 1,
#'               0, 0, 0, 1, 1,
#'               0, 0, 0, 0, 1), nrow = 3, byrow = TRUE)
#' sw_design(clusters_per_sequence = c(8, 10, 12), treatment = m)
#' @export
sw_design <- function(
  clusters_per_sequence,
  crossover_period = NULL,
  n_periods = NULL,
  treatment = NULL,
  sequence_names = NULL
) {
  if (!is.numeric(clusters_per_sequence) ||
      any(clusters_per_sequence < 1) ||
      any(clusters_per_sequence != round(clusters_per_sequence))) {
    stop("`clusters_per_sequence` must be positive integers.", call. = FALSE)
  }
  n_sequences <- length(clusters_per_sequence)

  if (is.null(treatment)) {
    if (is.null(crossover_period)) {
      stop("Supply either `crossover_period` or `treatment`.", call. = FALSE)
    }
    if (length(crossover_period) != n_sequences) {
      stop("`crossover_period` must have one entry per sequence (length ",
           n_sequences, ").", call. = FALSE)
    }
    finite_cross <- crossover_period[is.finite(crossover_period)]
    if (is.null(n_periods)) {
      if (length(finite_cross) == 0) {
        stop("Cannot infer `n_periods`; supply it explicitly.", call. = FALSE)
      }
      n_periods <- max(finite_cross)
    }
    treatment <- matrix(0L, nrow = n_sequences, ncol = n_periods)
    for (s in seq_len(n_sequences)) {
      cp <- crossover_period[s]
      if (is.finite(cp) && cp <= n_periods) treatment[s, cp:n_periods] <- 1L
    }
  } else {
    treatment <- as.matrix(treatment)
    if (!all(treatment %in% c(0, 1))) {
      stop("`treatment` must contain only 0 and 1.", call. = FALSE)
    }
    if (nrow(treatment) != n_sequences) {
      stop("`treatment` must have one row per sequence (", n_sequences, ").",
           call. = FALSE)
    }
    storage.mode(treatment) <- "integer"
    if (is.null(n_periods)) {
      n_periods <- ncol(treatment)
    } else if (n_periods != ncol(treatment)) {
      stop("`n_periods` must equal ncol(treatment).", call. = FALSE)
    }
    crossover_period <- apply(treatment, 1L, function(r) {
      w <- which(r == 1L); if (length(w) == 0) Inf else min(w)
    })
  }

  if (is.null(sequence_names)) {
    sequence_names <- paste0("Sequence ", seq_len(n_sequences))
  }
  if (length(sequence_names) != n_sequences) {
    stop("`sequence_names` must have length ", n_sequences, ".", call. = FALSE)
  }
  rownames(treatment) <- sequence_names
  colnames(treatment) <- paste0("Period ", seq_len(n_periods))

  structure(
    list(
      treatment = treatment,
      n_sequences = n_sequences,
      n_periods = n_periods,
      clusters_per_sequence = as.integer(clusters_per_sequence),
      n_clusters = sum(clusters_per_sequence),
      crossover_period = crossover_period,
      sequence_names = sequence_names
    ),
    class = "sw_design"
  )
}

#' @param x An `sw_design` object.
#' @param ... Unused.
#' @rdname sw_design
#' @export
print.sw_design <- function(x, ...) {
  cat("<sw_design>\n")
  cat(sprintf("  %d sequences, %d periods, %d clusters total\n",
              x$n_sequences, x$n_periods, x$n_clusters))
  cat("  clusters per sequence:",
      paste(x$clusters_per_sequence, collapse = ", "), "\n")
  cat("  treatment schedule (rows = sequences, cols = periods):\n")
  print(x$treatment)
  invisible(x)
}
