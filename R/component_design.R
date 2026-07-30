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
#' `"AB"`, `"A+B"`, and `"1+2"` are also accepted. Alternatively, users may
#' supply separate binary matrices for assignment to A and B.
#'
#' Structurally unobserved sequence-period cells can be supplied through
#' `observed`. Their latent treatment states are retained so wash-in,
#' withdrawal, carryover, and restart histories remain well defined, but the
#' cells do not contribute outcomes or fitted-model rows. For schedules written
#' directly with dashes, use [sw_incomplete_component_design()].
#'
#' @param clusters_per_sequence Positive integer vector giving the number of
#'   clusters assigned to each sequence.
#' @param state Optional sequence-by-period matrix containing numeric codes
#'   `0:3` or treatment labels. Supply either `state` or both `component_a` and
#'   `component_b`.
#' @param component_a,component_b Optional binary sequence-by-period assignment
#'   matrices for A and B.
#' @param sequence_names Optional unique sequence labels.
#' @param observed Optional logical or 0/1 sequence-by-period matrix. `FALSE`
#'   marks a structurally unobserved cell. The default observes every cell.
#'
#' @return An object of class `"sw_component_design"`.
#' @examples
#' state <- rbind(
#'   S1 = c("Control", "A", "A", "A+B", "A+B", "B"),
#'   S2 = c("Control", "Control", "B", "B", "A+B", "A+B"),
#'   S3 = c("Control", "Control", "A", "A", "A+B", "A+B")
#' )
#' design <- sw_component_design(c(4, 4, 4), state = state)
#' design
#' @export
sw_component_design <- function(
  clusters_per_sequence,
  state = NULL,
  component_a = NULL,
  component_b = NULL,
  sequence_names = NULL,
  observed = NULL
) {
  if (!is.numeric(clusters_per_sequence) || length(clusters_per_sequence) < 1L ||
      anyNA(clusters_per_sequence) || any(!is.finite(clusters_per_sequence)) ||
      any(clusters_per_sequence < 1) ||
      any(clusters_per_sequence != round(clusters_per_sequence))) {
    stop("`clusters_per_sequence` must contain positive integers.", call. = FALSE)
  }
  clusters_per_sequence <- as.integer(clusters_per_sequence)
  n_sequences <- length(clusters_per_sequence)

  supplied_state <- !is.null(state)
  supplied_components <- !is.null(component_a) || !is.null(component_b)
  if (supplied_state && supplied_components) {
    stop("Supply either `state` or the two component matrices, not both.", call. = FALSE)
  }
  if (!supplied_state && !supplied_components) {
    stop("Supply `state` or both `component_a` and `component_b`.", call. = FALSE)
  }

  if (supplied_state) {
    state_code <- .normalize_component_state_matrix(state)
    if (nrow(state_code) != n_sequences) {
      stop("`state` must have one row per sequence (", n_sequences, ").", call. = FALSE)
    }
    a_matrix <- as.integer(state_code %in% c(1L, 3L))
    b_matrix <- as.integer(state_code %in% c(2L, 3L))
    dim(a_matrix) <- dim(state_code)
    dim(b_matrix) <- dim(state_code)
  } else {
    if (is.null(component_a) || is.null(component_b)) {
      stop("Supply both `component_a` and `component_b`.", call. = FALSE)
    }
    a_matrix <- .validate_component_assignment_matrix(component_a, "component_a", n_sequences)
    b_matrix <- .validate_component_assignment_matrix(component_b, "component_b", n_sequences)
    if (!identical(dim(a_matrix), dim(b_matrix))) {
      stop("`component_a` and `component_b` must have identical dimensions.", call. = FALSE)
    }
    state_code <- a_matrix + 2L * b_matrix
  }

  n_periods <- ncol(state_code)
  if (n_periods < 1L) stop("The schedule must contain at least one period.", call. = FALSE)

  if (is.null(sequence_names)) {
    inherited_names <- rownames(state_code)
    if (!is.null(inherited_names) && length(inherited_names) == n_sequences &&
        all(nzchar(inherited_names)) && !anyDuplicated(inherited_names)) {
      sequence_names <- inherited_names
    } else {
      sequence_names <- paste0("Sequence ", seq_len(n_sequences))
    }
  }
  if (length(sequence_names) != n_sequences || anyNA(sequence_names) ||
      any(!nzchar(as.character(sequence_names))) ||
      anyDuplicated(as.character(sequence_names))) {
    stop("`sequence_names` must contain one unique, non-empty label per sequence.", call. = FALSE)
  }
  sequence_names <- as.character(sequence_names)

  rownames(state_code) <- sequence_names
  colnames(state_code) <- paste0("Period ", seq_len(n_periods))
  dimnames(a_matrix) <- dimnames(state_code)
  dimnames(b_matrix) <- dimnames(state_code)
  observed_matrix <- .validate_component_observation_matrix(
    observed, n_sequences, n_periods, dimnames(state_code)
  )

  stage_names <- c("Control", "A", "B", "A+B")
  latent_paths <- apply(state_code, 1L, function(x) paste(stage_names[x + 1L], collapse = " -> "))
  display_labels <- matrix(stage_names[state_code + 1L], nrow = n_sequences,
                           ncol = n_periods, dimnames = dimnames(state_code))
  display_labels[!observed_matrix] <- "-"
  observed_paths <- apply(display_labels, 1L, paste, collapse = " -> ")

  withdrawal_a <- .component_transition_count(a_matrix, 1L, 0L)
  withdrawal_b <- .component_transition_count(b_matrix, 1L, 0L)
  restart_a <- .component_restart_count(a_matrix)
  restart_b <- .component_restart_count(b_matrix)
  observed_states <- state_code[observed_matrix]
  observed_periods_per_sequence <- rowSums(observed_matrix)
  n_observed_sequence_periods <- sum(observed_matrix)
  n_calendar_sequence_periods <- n_sequences * n_periods
  n_observed_cluster_periods <- sum(clusters_per_sequence * observed_periods_per_sequence)
  n_calendar_cluster_periods <- sum(clusters_per_sequence) * n_periods

  structure(list(
    state = state_code,
    component_a = a_matrix,
    component_b = b_matrix,
    observed = observed_matrix,
    display_state = display_labels,
    n_sequences = n_sequences,
    n_periods = n_periods,
    clusters_per_sequence = clusters_per_sequence,
    n_clusters = sum(clusters_per_sequence),
    sequence_names = sequence_names,
    stage_names = stage_names,
    state_codes = stats::setNames(0:3, stage_names),
    paths = latent_paths,
    observed_paths = observed_paths,
    has_control = any(observed_states == 0L),
    has_a_only = any(observed_states == 1L),
    has_b_only = any(observed_states == 2L),
    has_ab = any(observed_states == 3L),
    latent_has_control = any(state_code == 0L),
    latent_has_a_only = any(state_code == 1L),
    latent_has_b_only = any(state_code == 2L),
    latent_has_ab = any(state_code == 3L),
    observed_periods_per_sequence = as.integer(observed_periods_per_sequence),
    n_observed_sequence_periods = as.integer(n_observed_sequence_periods),
    n_missing_sequence_periods = as.integer(n_calendar_sequence_periods - n_observed_sequence_periods),
    n_observed_cluster_periods = as.integer(n_observed_cluster_periods),
    n_missing_cluster_periods = as.integer(n_calendar_cluster_periods - n_observed_cluster_periods),
    n_calendar_sequence_periods = as.integer(n_calendar_sequence_periods),
    n_calendar_cluster_periods = as.integer(n_calendar_cluster_periods),
    is_incomplete = any(!observed_matrix),
    latent_state_inferred = FALSE,
    inferred_latent_cells = matrix(FALSE, n_sequences, n_periods, dimnames = dimnames(state_code)),
    withdrawal_count_a = withdrawal_a,
    withdrawal_count_b = withdrawal_b,
    restart_count_a = restart_a,
    restart_count_b = restart_b,
    has_withdrawal_a = withdrawal_a > 0L,
    has_withdrawal_b = withdrawal_b > 0L,
    has_restart_a = restart_a > 0L,
    has_restart_b = restart_b > 0L
  ), class = "sw_component_design")
}

.normalize_component_state_matrix <- function(state) {
  state <- as.matrix(state)
  if (length(state) < 1L || nrow(state) < 1L || ncol(state) < 1L) {
    stop("`state` must be a non-empty matrix.", call. = FALSE)
  }
  if (anyNA(state)) {
    stop("`state` cannot contain missing values. Use `sw_incomplete_component_design()` for schedules containing dashes.", call. = FALSE)
  }
  if (is.numeric(state) || is.integer(state)) {
    if (any(!is.finite(state)) || any(state != round(state)) || any(!state %in% 0:3)) {
      stop("Numeric `state` values must be 0 (Control), 1 (A), 2 (B), or 3 (A+B).", call. = FALSE)
    }
    out <- matrix(as.integer(state), nrow = nrow(state), ncol = ncol(state))
    dimnames(out) <- dimnames(state)
    return(out)
  }
  raw <- trimws(as.character(state))
  key <- toupper(gsub("[[:space:]_&+-]", "", raw))
  mapping <- c("0"=0L, "C"=0L, "CONTROL"=0L, "NONE"=0L,
               "1"=1L, "A"=1L, "2"=2L, "B"=2L,
               "3"=3L, "12"=3L, "AB"=3L, "BOTH"=3L)
  unknown <- setdiff(unique(key), names(mapping))
  if (length(unknown)) {
    raw_unknown <- unique(raw[key %in% unknown])
    stop("Unrecognized treatment state label(s): ", paste(raw_unknown, collapse = ", "),
         ". Use Control, A, B, A+B, 1+2, or codes 0:3.", call. = FALSE)
  }
  out <- matrix(unname(mapping[key]), nrow = nrow(state), ncol = ncol(state), dimnames = dimnames(state))
  storage.mode(out) <- "integer"
  out
}

.validate_component_assignment_matrix <- function(x, name, n_sequences) {
  x <- as.matrix(x)
  if (nrow(x) != n_sequences || ncol(x) < 1L) {
    stop("`", name, "` must have one row per sequence and at least one period.", call. = FALSE)
  }
  if (anyNA(x) || any(!is.finite(x)) || any(!x %in% c(0, 1))) {
    stop("`", name, "` must contain only 0 and 1.", call. = FALSE)
  }
  out <- matrix(as.integer(x), nrow = nrow(x), ncol = ncol(x)); dimnames(out) <- dimnames(x); out
}

.validate_component_observation_matrix <- function(observed, n_sequences, n_periods, dimnames_out = NULL) {
  if (is.null(observed)) {
    out <- matrix(TRUE, n_sequences, n_periods)
  } else {
    observed <- as.matrix(observed)
    if (!identical(dim(observed), c(n_sequences, n_periods))) {
      stop("`observed` must have dimensions ", n_sequences, " x ", n_periods, ", matching the treatment schedule.", call. = FALSE)
    }
    if (anyNA(observed)) stop("`observed` cannot contain missing values.", call. = FALSE)
    if (is.logical(observed)) {
      out <- observed
    } else if (is.numeric(observed) || is.integer(observed)) {
      if (any(!is.finite(observed)) || any(!observed %in% c(0, 1))) stop("Numeric `observed` values must be 0 or 1.", call. = FALSE)
      out <- observed == 1
    } else stop("`observed` must be a logical or 0/1 matrix.", call. = FALSE)
  }
  if (any(rowSums(out) == 0L)) stop("Every sequence must contain at least one observed period.", call. = FALSE)
  dimnames(out) <- dimnames_out; storage.mode(out) <- "logical"; out
}

.component_observed_matrix <- function(design) {
  if (!is.null(design$observed)) return(design$observed)
  matrix(TRUE, design$n_sequences, design$n_periods, dimnames = dimnames(design$state))
}

.component_transition_count <- function(x, from, to) {
  if (ncol(x) < 2L) return(0L)
  sum(x[, -ncol(x), drop = FALSE] == from & x[, -1L, drop = FALSE] == to)
}

.component_restart_count <- function(x) {
  if (ncol(x) < 2L) return(0L)
  total <- 0L
  for (s in seq_len(nrow(x))) {
    row_s <- x[s, ]; active_seen <- FALSE
    for (p in seq_along(row_s)) {
      if (row_s[p] == 1L && p > 1L && row_s[p-1L] == 0L && active_seen) total <- total + 1L
      if (row_s[p] == 1L) active_seen <- TRUE
    }
  }
  total
}

#' Construct a component design with structurally missing periods
#'
#' Creates an [sw_component_design()] from a manuscript-style schedule in which
#' dashes, blanks, or `NA` mean that no outcome is observed. Leading missing
#' cells inherit the first observed state and trailing missing cells inherit the
#' last observed state. Internal gaps require `latent_state`.
#'
#' @param clusters_per_sequence Positive integer vector.
#' @param state Schedule containing treatment labels and missing markers.
#' @param latent_state Optional complete latent treatment-state matrix.
#' @param sequence_names Optional sequence labels.
#' @param missing Character values treated as structurally missing.
#' @return An `sw_component_design` object with an observation mask.
#' @examples
#' bswd <- sw_incomplete_component_design(c(2, 2, 3, 3), rbind(
#'   c("0", "1", "1+2", "1+2", "-"),
#'   c("0", "0", "1", "1+2", "-"),
#'   c("-", "0", "1", "1+2", "1+2"),
#'   c("-", "0", "0", "1", "1+2")
#' ))
#' @export
sw_incomplete_component_design <- function(
  clusters_per_sequence, state, latent_state = NULL, sequence_names = NULL,
  missing = c("-", "--", "\u2013", "\u2014", "", "NA", ".")
) {
  raw <- as.matrix(state)
  if (length(raw) < 1L || nrow(raw) < 1L || ncol(raw) < 1L) stop("`state` must be a non-empty matrix.", call. = FALSE)
  missing_mask <- .component_missing_mask(raw, missing)
  observed <- !missing_mask
  if (any(rowSums(observed) == 0L)) stop("Every sequence must contain at least one observed treatment state.", call. = FALSE)
  parse_matrix <- raw
  if (is.numeric(parse_matrix) || is.integer(parse_matrix)) parse_matrix[missing_mask] <- 0 else parse_matrix[missing_mask] <- "Control"
  observed_codes <- .normalize_component_state_matrix(parse_matrix)

  if (!is.null(latent_state)) {
    latent_codes <- .normalize_component_state_matrix(latent_state)
    if (!identical(dim(latent_codes), dim(observed_codes))) stop("`latent_state` must have the same dimensions as `state`.", call. = FALSE)
    disagreement <- observed & latent_codes != observed_codes
    if (any(disagreement)) stop("`latent_state` disagrees with an observed state.", call. = FALSE)
  } else {
    latent_codes <- observed_codes
    for (s in seq_len(nrow(observed))) {
      op <- which(observed[s, ]); first <- min(op); last <- max(op)
      if (any(!observed[s, seq.int(first, last)])) stop("Sequence ", s, " contains an internal missing period. Supply a complete `latent_state` matrix.", call. = FALSE)
      if (first > 1L) latent_codes[s, seq_len(first - 1L)] <- latent_codes[s, first]
      if (last < ncol(latent_codes)) latent_codes[s, seq.int(last + 1L, ncol(latent_codes))] <- latent_codes[s, last]
    }
  }
  if (is.null(sequence_names)) {
    sequence_names <- rownames(raw)
    if (is.null(sequence_names) || any(!nzchar(sequence_names)) || anyDuplicated(sequence_names)) sequence_names <- NULL
  }
  out <- sw_component_design(clusters_per_sequence, state = latent_codes,
                             sequence_names = sequence_names, observed = observed)
  out$latent_state_inferred <- is.null(latent_state) && any(missing_mask)
  out$inferred_latent_cells <- if (is.null(latent_state)) missing_mask else matrix(FALSE, nrow(missing_mask), ncol(missing_mask))
  dimnames(out$inferred_latent_cells) <- dimnames(out$state)
  out
}

.component_missing_mask <- function(state, missing) {
  state <- as.matrix(state)
  if (is.numeric(state) || is.integer(state)) return(is.na(state))
  raw <- trimws(as.character(state)); m <- toupper(trimws(as.character(missing)))
  matrix(is.na(state) | toupper(raw) %in% m, nrow(state), ncol(state), dimnames = dimnames(state))
}

#' Convert a cumulative A/A+B design to a component design
#' @param design An `sw_multistage_design` or `sw_component_design` object.
#' @return An `sw_component_design` object.
#' @export
as_component_design <- function(design) {
  if (inherits(design, "sw_component_design")) return(design)
  if (!inherits(design, "sw_multistage_design")) stop("`design` must be an sw_multistage_design or sw_component_design object.", call. = FALSE)
  state <- design$state; state[state == 2L] <- 3L
  sw_component_design(design$clusters_per_sequence, state = state, sequence_names = design$sequence_names)
}

#' @param x An `sw_component_design` object.
#' @param ... Unused.
#' @rdname sw_component_design
#' @export
print.sw_component_design <- function(x, ...) {
  observed <- .component_observed_matrix(x)
  ocp <- sum(x$clusters_per_sequence * rowSums(observed)); ccp <- x$n_clusters * x$n_periods
  cat("<sw_component_design>\n")
  cat(sprintf("  %d sequences, %d calendar periods, %d clusters total\n", x$n_sequences, x$n_periods, x$n_clusters))
  cat("  clusters per sequence:", paste(x$clusters_per_sequence, collapse = ", "), "\n")
  cat(sprintf("  observed cluster-periods: %d of %d%s\n", ocp, ccp, if (any(!observed)) " (structurally incomplete)" else ""))
  observed_states <- x$state[observed]
  cat("  observed states present:", paste(x$stage_names[vapply(0:3, function(z) any(observed_states == z), logical(1))], collapse = ", "), "\n")
  cat(sprintf("  withdrawals: A = %d, B = %d; restarts: A = %d, B = %d\n", x$withdrawal_count_a, x$withdrawal_count_b, x$restart_count_a, x$restart_count_b))
  labels <- matrix(x$stage_names[x$state + 1L], nrow = x$n_sequences, dimnames = dimnames(x$state)); labels[!observed] <- "-"
  print(labels, quote = FALSE); invisible(x)
}
