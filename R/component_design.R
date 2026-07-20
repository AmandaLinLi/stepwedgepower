#' Construct a component-based stepped-wedge design
#'
#' Creates a stepped-wedge or rollout design with the four treatment states
#' `Control`, `A`, `B`, and `A+B`. The state schedule may be asynchronous and
#' need not be monotone. Consequently, the design can represent ordinary
#' rollouts, two-path factorial rollouts (`Control -> A -> A+B` and
#' `Control -> B -> A+B`), withdrawal (`A+B -> B`), and later reintroduction of
#' either component.
#'
#' The state matrix uses the component code `0 = Control`, `1 = A`, `2 = B`,
#' and `3 = A+B`. Character labels such as `"Control"`, `"A"`, `"B"`,
#' `"AB"`, and `"A+B"` are also accepted. Alternatively, users may supply
#' separate binary matrices for assignment to A and B.
#'
#' @param clusters_per_sequence Positive integer vector giving the number of
#'   clusters assigned to each sequence.
#' @param state Optional sequence-by-period matrix containing numeric codes
#'   `0:3` or treatment labels. Supply either `state` or both `component_a` and
#'   `component_b`.
#' @param component_a,component_b Optional binary sequence-by-period assignment
#'   matrices for A and B.
#' @param sequence_names Optional unique sequence labels.
#'
#' @return An object of class `"sw_component_design"`.
#' @examples
#' state <- rbind(
#'   S1 = c("Control", "A", "A", "A+B", "A+B", "B"),
#'   S2 = c("Control", "Control", "B", "B", "A+B", "A+B"),
#'   S3 = c("Control", "Control", "A", "A", "A+B", "A+B")
#' )
#' design <- sw_component_design(
#'   clusters_per_sequence = c(4, 4, 4),
#'   state = state
#' )
#' design
#' @export
sw_component_design <- function(
  clusters_per_sequence,
  state = NULL,
  component_a = NULL,
  component_b = NULL,
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

  supplied_state <- !is.null(state)
  supplied_components <- !is.null(component_a) || !is.null(component_b)
  if (supplied_state && supplied_components) {
    stop("Supply either `state` or the two component matrices, not both.",
         call. = FALSE)
  }
  if (!supplied_state && !supplied_components) {
    stop("Supply `state` or both `component_a` and `component_b`.",
         call. = FALSE)
  }

  if (supplied_state) {
    state_code <- .normalize_component_state_matrix(state)
    if (nrow(state_code) != n_sequences) {
      stop("`state` must have one row per sequence (", n_sequences, ").",
           call. = FALSE)
    }
    a_matrix <- as.integer(state_code %in% c(1L, 3L))
    b_matrix <- as.integer(state_code %in% c(2L, 3L))
    dim(a_matrix) <- dim(state_code)
    dim(b_matrix) <- dim(state_code)
  } else {
    if (is.null(component_a) || is.null(component_b)) {
      stop("Supply both `component_a` and `component_b`.", call. = FALSE)
    }
    a_matrix <- .validate_component_assignment_matrix(
      component_a, "component_a", n_sequences
    )
    b_matrix <- .validate_component_assignment_matrix(
      component_b, "component_b", n_sequences
    )
    if (!identical(dim(a_matrix), dim(b_matrix))) {
      stop("`component_a` and `component_b` must have identical dimensions.",
           call. = FALSE)
    }
    state_code <- a_matrix + 2L * b_matrix
  }

  n_periods <- ncol(state_code)
  if (n_periods < 1L) {
    stop("The schedule must contain at least one period.", call. = FALSE)
  }

  if (is.null(sequence_names)) {
    inherited_names <- rownames(state_code)
    if (!is.null(inherited_names) &&
        length(inherited_names) == n_sequences &&
        all(nzchar(inherited_names)) && !anyDuplicated(inherited_names)) {
      sequence_names <- inherited_names
    } else {
      sequence_names <- paste0("Sequence ", seq_len(n_sequences))
    }
  }
  if (length(sequence_names) != n_sequences || anyNA(sequence_names) ||
      any(!nzchar(as.character(sequence_names))) ||
      anyDuplicated(as.character(sequence_names))) {
    stop("`sequence_names` must contain one unique, non-empty label per sequence.",
         call. = FALSE)
  }
  sequence_names <- as.character(sequence_names)

  rownames(state_code) <- sequence_names
  colnames(state_code) <- paste0("Period ", seq_len(n_periods))
  dimnames(a_matrix) <- dimnames(state_code)
  dimnames(b_matrix) <- dimnames(state_code)

  stage_names <- c("Control", "A", "B", "A+B")
  paths <- apply(state_code, 1L, function(x) {
    paste(stage_names[x + 1L], collapse = " -> ")
  })

  withdrawal_a <- .component_transition_count(a_matrix, from = 1L, to = 0L)
  withdrawal_b <- .component_transition_count(b_matrix, from = 1L, to = 0L)
  restart_a <- .component_restart_count(a_matrix)
  restart_b <- .component_restart_count(b_matrix)

  structure(
    list(
      state = state_code,
      component_a = a_matrix,
      component_b = b_matrix,
      n_sequences = n_sequences,
      n_periods = n_periods,
      clusters_per_sequence = clusters_per_sequence,
      n_clusters = sum(clusters_per_sequence),
      sequence_names = sequence_names,
      stage_names = stage_names,
      state_codes = stats::setNames(0:3, stage_names),
      paths = paths,
      has_control = any(state_code == 0L),
      has_a_only = any(state_code == 1L),
      has_b_only = any(state_code == 2L),
      has_ab = any(state_code == 3L),
      withdrawal_count_a = withdrawal_a,
      withdrawal_count_b = withdrawal_b,
      restart_count_a = restart_a,
      restart_count_b = restart_b,
      has_withdrawal_a = withdrawal_a > 0L,
      has_withdrawal_b = withdrawal_b > 0L,
      has_restart_a = restart_a > 0L,
      has_restart_b = restart_b > 0L
    ),
    class = "sw_component_design"
  )
}

.normalize_component_state_matrix <- function(state) {
  state <- as.matrix(state)
  if (length(state) < 1L || nrow(state) < 1L || ncol(state) < 1L) {
    stop("`state` must be a non-empty matrix.", call. = FALSE)
  }
  if (anyNA(state)) {
    stop("`state` cannot contain missing values.", call. = FALSE)
  }

  if (is.numeric(state) || is.integer(state)) {
    if (any(!is.finite(state)) || any(state != round(state)) ||
        any(!state %in% 0:3)) {
      stop("Numeric `state` values must be 0 (Control), 1 (A), 2 (B), or 3 (A+B).",
           call. = FALSE)
    }
    out <- matrix(as.integer(state), nrow = nrow(state), ncol = ncol(state))
    dimnames(out) <- dimnames(state)
    return(out)
  }

  raw <- trimws(as.character(state))
  key <- toupper(gsub("[[:space:]_&+-]", "", raw))
  mapping <- c(
    "0" = 0L, "C" = 0L, "CONTROL" = 0L, "NONE" = 0L,
    "1" = 1L, "A" = 1L,
    "2" = 2L, "B" = 2L,
    "3" = 3L, "AB" = 3L, "BOTH" = 3L
  )
  unknown <- setdiff(unique(key), names(mapping))
  if (length(unknown)) {
    stop(
      "Unrecognized treatment state label(s): ",
      paste(unique(raw[key %in% unknown]), collapse = ", "),
      ". Use Control, A, B, A+B, or codes 0:3.",
      call. = FALSE
    )
  }
  out <- matrix(
    unname(mapping[key]),
    nrow = nrow(state), ncol = ncol(state),
    dimnames = dimnames(state)
  )
  storage.mode(out) <- "integer"
  out
}

.validate_component_assignment_matrix <- function(x, name, n_sequences) {
  x <- as.matrix(x)
  if (nrow(x) != n_sequences || ncol(x) < 1L) {
    stop("`", name, "` must have one row per sequence and at least one period.",
         call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x)) || any(!x %in% c(0, 1))) {
    stop("`", name, "` must contain only 0 and 1.", call. = FALSE)
  }
  out <- matrix(as.integer(x), nrow = nrow(x), ncol = ncol(x))
  dimnames(out) <- dimnames(x)
  out
}

.component_transition_count <- function(x, from, to) {
  if (ncol(x) < 2L) return(0L)
  sum(x[, -ncol(x), drop = FALSE] == from &
        x[, -1L, drop = FALSE] == to)
}

.component_restart_count <- function(x) {
  if (ncol(x) < 2L) return(0L)
  total <- 0L
  for (s in seq_len(nrow(x))) {
    row_s <- x[s, ]
    active_seen <- FALSE
    for (p in seq_along(row_s)) {
      if (row_s[p] == 1L && p > 1L && row_s[p - 1L] == 0L && active_seen) {
        total <- total + 1L
      }
      if (row_s[p] == 1L) active_seen <- TRUE
    }
  }
  total
}

#' Convert a cumulative A/A+B design to a component design
#'
#' Maps an [sw_multistage_design()] object to the general four-state component
#' representation. The old A+B code `2` is converted to the component code `3`.
#'
#' @param design An `sw_multistage_design` or `sw_component_design` object.
#' @return An `sw_component_design` object.
#' @examples
#' old <- sw_multistage_design(
#'   clusters_per_sequence = c(3, 3),
#'   a_start = c(2, 3), b_start = c(4, 5), n_periods = 5
#' )
#' as_component_design(old)
#' @export
as_component_design <- function(design) {
  if (inherits(design, "sw_component_design")) return(design)
  if (!inherits(design, "sw_multistage_design")) {
    stop("`design` must be an sw_multistage_design or sw_component_design object.",
         call. = FALSE)
  }
  state <- design$state
  state[state == 2L] <- 3L
  sw_component_design(
    clusters_per_sequence = design$clusters_per_sequence,
    state = state,
    sequence_names = design$sequence_names
  )
}

#' @param x An `sw_component_design` object.
#' @param ... Unused.
#' @rdname sw_component_design
#' @export
print.sw_component_design <- function(x, ...) {
  cat("<sw_component_design>\n")
  cat(sprintf("  %d sequences, %d periods, %d clusters total\n",
              x$n_sequences, x$n_periods, x$n_clusters))
  cat("  clusters per sequence:",
      paste(x$clusters_per_sequence, collapse = ", "), "\n")
  cat("  states present:",
      paste(x$stage_names[vapply(0:3, function(z) any(x$state == z), logical(1))],
            collapse = ", "), "\n")
  cat(sprintf("  withdrawals: A = %d, B = %d; restarts: A = %d, B = %d\n",
              x$withdrawal_count_a, x$withdrawal_count_b,
              x$restart_count_a, x$restart_count_b))
  labels <- matrix(
    x$stage_names[x$state + 1L],
    nrow = x$n_sequences,
    dimnames = dimnames(x$state)
  )
  print(labels, quote = FALSE)
  invisible(x)
}
