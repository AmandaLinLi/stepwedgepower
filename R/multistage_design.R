#' Construct an asynchronous cumulative-intervention stepped-wedge design
#'
#' Builds a design in which clusters move through the ordered treatment states
#' `Control -> A -> A+B`. Sequences may start intervention A and add
#' intervention B in different calendar periods. B is therefore interpreted as
#' an add-on intervention delivered only after A has begun; the design contains
#' no B-only condition.
#'
#' The design can be specified either by sequence-specific start periods or by
#' an explicit state matrix. In a state matrix, rows are sequences, columns are
#' periods, and entries are `0` (Control), `1` (A), or `2` (A+B). Every row must
#' be non-decreasing and must contain at least one A-only period before A+B.
#'
#' @param clusters_per_sequence Positive integer vector giving the number of
#'   clusters assigned to each sequence.
#' @param a_start Integer vector giving the first period under intervention A
#'   for each sequence. Use `Inf` for a sequence that never starts A. Ignored
#'   when `state` is supplied.
#' @param b_start Integer vector giving the first period under A+B for each
#'   sequence. Each finite value must be strictly later than the corresponding
#'   `a_start`. Use `Inf` for a sequence that never adds B. Ignored when `state`
#'   is supplied.
#' @param n_periods Number of periods. If `NULL`, inferred from the largest
#'   finite start period or the number of columns in `state`.
#' @param state Optional integer matrix with values `0`, `1`, and `2`, one row
#'   per sequence and one column per period. This overrides `a_start` and
#'   `b_start`.
#' @param sequence_names Optional unique labels for the sequences.
#'
#' @return An object of class `"sw_multistage_design"`.
#' @examples
#' design <- sw_multistage_design(
#'   clusters_per_sequence = rep(6, 5),
#'   a_start = c(2, 3, 4, 5, 6),
#'   b_start = c(6, 7, 8, 9, 10),
#'   n_periods = 10
#' )
#' design
#' @export
sw_multistage_design <- function(
  clusters_per_sequence,
  a_start = NULL,
  b_start = NULL,
  n_periods = NULL,
  state = NULL,
  sequence_names = NULL
) {
  if (!is.numeric(clusters_per_sequence) ||
      length(clusters_per_sequence) < 1L ||
      anyNA(clusters_per_sequence) ||
      any(!is.finite(clusters_per_sequence)) ||
      any(clusters_per_sequence < 1) ||
      any(clusters_per_sequence != round(clusters_per_sequence))) {
    stop("`clusters_per_sequence` must contain positive integers.",
         call. = FALSE)
  }
  clusters_per_sequence <- as.integer(clusters_per_sequence)
  n_sequences <- length(clusters_per_sequence)

  if (is.null(sequence_names)) {
    sequence_names <- paste0("Sequence ", seq_len(n_sequences))
  }
  if (length(sequence_names) != n_sequences ||
      anyNA(sequence_names) || any(!nzchar(as.character(sequence_names))) ||
      anyDuplicated(sequence_names)) {
    stop("`sequence_names` must contain one unique, non-empty label per sequence.",
         call. = FALSE)
  }
  sequence_names <- as.character(sequence_names)

  if (!is.null(state)) {
    state <- as.matrix(state)
    if (nrow(state) != n_sequences) {
      stop("`state` must have one row per sequence (", n_sequences, ").",
           call. = FALSE)
    }
    if (anyNA(state) || !all(state %in% 0:2)) {
      stop("`state` must contain only 0 (Control), 1 (A), and 2 (A+B).",
           call. = FALSE)
    }
    storage.mode(state) <- "integer"

    if (is.null(n_periods)) {
      n_periods <- ncol(state)
    } else if (length(n_periods) != 1L || !is.finite(n_periods) ||
               n_periods < 1 || n_periods != round(n_periods) ||
               n_periods != ncol(state)) {
      stop("`n_periods` must be a positive integer equal to ncol(state).",
           call. = FALSE)
    }
    n_periods <- as.integer(n_periods)

    for (s in seq_len(n_sequences)) {
      row_s <- state[s, ]
      if (any(diff(row_s) < 0L)) {
        stop("Every row of `state` must be non-decreasing (Control -> A -> A+B).",
             call. = FALSE)
      }
      first_b <- which(row_s == 2L)[1]
      if (!is.na(first_b)) {
        if (first_b == 1L || !any(row_s[seq_len(first_b - 1L)] == 1L)) {
          stop("Every sequence entering A+B must first contribute at least one A-only period.",
               call. = FALSE)
        }
      }
    }

    a_start <- apply(state, 1L, function(x) {
      w <- which(x >= 1L)
      if (length(w)) min(w) else Inf
    })
    b_start <- apply(state, 1L, function(x) {
      w <- which(x == 2L)
      if (length(w)) min(w) else Inf
    })
  } else {
    if (is.null(a_start)) {
      stop("Supply either `a_start` or an explicit `state` matrix.",
           call. = FALSE)
    }
    if (length(a_start) != n_sequences) {
      stop("`a_start` must have one value per sequence.", call. = FALSE)
    }
    if (is.null(b_start)) b_start <- rep(Inf, n_sequences)
    if (length(b_start) != n_sequences) {
      stop("`b_start` must have one value per sequence.", call. = FALSE)
    }

    a_start <- .validate_multistage_start(a_start, "a_start")
    b_start <- .validate_multistage_start(b_start, "b_start")

    finite_starts <- c(a_start[is.finite(a_start)],
                       b_start[is.finite(b_start)])
    if (is.null(n_periods)) {
      if (!length(finite_starts)) {
        stop("Cannot infer `n_periods`; supply it explicitly.", call. = FALSE)
      }
      n_periods <- max(finite_starts)
    }
    if (length(n_periods) != 1L || !is.finite(n_periods) ||
        n_periods < 1 || n_periods != round(n_periods)) {
      stop("`n_periods` must be one positive integer.", call. = FALSE)
    }
    n_periods <- as.integer(n_periods)

    if (any(is.finite(a_start) & a_start > n_periods) ||
        any(is.finite(b_start) & b_start > n_periods)) {
      stop("Finite start periods cannot exceed `n_periods`.", call. = FALSE)
    }
    if (any(is.finite(b_start) & !is.finite(a_start))) {
      stop("A sequence cannot add B without first starting A.", call. = FALSE)
    }
    if (any(is.finite(b_start) & b_start <= a_start)) {
      stop("Every finite `b_start` must be strictly later than `a_start`.",
           call. = FALSE)
    }

    state <- matrix(0L, nrow = n_sequences, ncol = n_periods)
    for (s in seq_len(n_sequences)) {
      if (is.finite(a_start[s])) {
        state[s, a_start[s]:n_periods] <- 1L
      }
      if (is.finite(b_start[s])) {
        state[s, b_start[s]:n_periods] <- 2L
      }
    }
  }

  rownames(state) <- sequence_names
  colnames(state) <- paste0("Period ", seq_len(n_periods))
  a_matrix <- (state >= 1L) * 1L
  b_matrix <- (state == 2L) * 1L
  dimnames(a_matrix) <- dimnames(state)
  dimnames(b_matrix) <- dimnames(state)

  lag_ab <- rep(NA_real_, n_sequences)
  finite_b <- is.finite(b_start)
  lag_ab[finite_b] <- b_start[finite_b] - a_start[finite_b]

  structure(
    list(
      state = state,
      intervention_a = a_matrix,
      intervention_b = b_matrix,
      n_sequences = n_sequences,
      n_periods = n_periods,
      clusters_per_sequence = clusters_per_sequence,
      n_clusters = sum(clusters_per_sequence),
      a_start = as.numeric(a_start),
      b_start = as.numeric(b_start),
      a_to_b_lag = lag_ab,
      has_b = any(finite_b),
      sequence_names = sequence_names,
      stage_names = c("Control", "A", "A+B")
    ),
    class = "sw_multistage_design"
  )
}

.validate_multistage_start <- function(x, name) {
  if (!is.numeric(x) || anyNA(x) ||
      any(is.finite(x) & (x < 1 | x != round(x))) ||
      any(is.nan(x)) || any(x == -Inf)) {
    stop("`", name, "` must contain positive integer periods or Inf.",
         call. = FALSE)
  }
  as.numeric(x)
}

# Internal sequence-period representation shared by simulation and auditing.
.multistage_sequence_period <- function(design, delay_a = 0L, delay_b = 0L) {
  if (!inherits(design, "sw_multistage_design")) {
    stop("`design` must be an sw_multistage_design object.", call. = FALSE)
  }
  delay_a <- .validate_multistage_delay(delay_a, "delay_a")
  delay_b <- .validate_multistage_delay(delay_b, "delay_b")

  grid <- expand.grid(
    sequence_idx = seq_len(design$n_sequences),
    period = seq_len(design$n_periods),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  grid <- grid[order(grid$sequence_idx, grid$period), , drop = FALSE]
  idx <- cbind(grid$sequence_idx, grid$period)
  grid$sequence <- design$sequence_names[grid$sequence_idx]
  grid$state_code <- as.integer(design$state[idx])
  grid$state <- factor(
    design$stage_names[grid$state_code + 1L],
    levels = design$stage_names,
    ordered = TRUE
  )
  grid$a_active <- as.integer(design$intervention_a[idx])
  grid$b_active <- as.integer(design$intervention_b[idx])

  a_start <- design$a_start[grid$sequence_idx]
  b_start <- design$b_start[grid$sequence_idx]
  grid$a_exposure_time <- ifelse(
    grid$a_active == 1L,
    grid$period - a_start + 1,
    0
  )
  grid$b_exposure_time <- ifelse(
    grid$b_active == 1L,
    grid$period - b_start + 1,
    0
  )
  grid$a_exposure_time <- as.integer(grid$a_exposure_time)
  grid$b_exposure_time <- as.integer(grid$b_exposure_time)
  grid$a_effective <- as.integer(grid$a_exposure_time > delay_a)
  grid$b_effective <- as.integer(grid$b_exposure_time > delay_b)
  rownames(grid) <- NULL
  grid
}

.validate_multistage_delay <- function(x, name) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) ||
      x < 0 || x != round(x)) {
    stop("`", name, "` must be one non-negative integer.", call. = FALSE)
  }
  as.integer(x)
}

#' @param x An `sw_multistage_design` object.
#' @param ... Unused.
#' @rdname sw_multistage_design
#' @export
print.sw_multistage_design <- function(x, ...) {
  cat("<sw_multistage_design>\n")
  cat(sprintf("  %d sequences, %d periods, %d clusters total\n",
              x$n_sequences, x$n_periods, x$n_clusters))
  cat("  clusters per sequence:",
      paste(x$clusters_per_sequence, collapse = ", "), "\n")
  cat("  asynchronous cumulative schedule:\n")
  labels <- matrix(
    x$stage_names[as.vector(x$state) + 1L],
    nrow = nrow(x$state), ncol = ncol(x$state),
    dimnames = dimnames(x$state)
  )
  print(labels, quote = FALSE)
  invisible(x)
}
