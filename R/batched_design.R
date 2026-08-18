#' Construct a classic or batched stepped-wedge design
#'
#' Creates a batch-aware extension of [sw_incomplete_component_design()]. A
#' batched stepped-wedge design (BSWD) groups sequences into batches that begin
#' trial participation at staggered calendar periods. The object records both
#' calendar period and relative time since batch initiation (time on trial).
#'
#' A dash (`"-"`), blank, or `NA` in `state` denotes a structurally
#' unobserved cluster-period. It is not recoded as Control. Leading and trailing
#' latent states are inferred by [sw_incomplete_component_design()] when they
#' are unambiguous; use `latent_state` for internal gaps.
#'
#' All sequences assigned to the same batch must share the same first observed
#' calendar period. An irregular delayed-start structure can still be
#' represented by assigning sequences with different starts to different
#' batches.
#'
#' @param clusters_per_sequence Positive integer vector giving clusters per
#'   sequence.
#' @param state Sequence-by-calendar-period treatment schedule. Accepted labels
#'   include `0`, `1`, `2`, `1+2`, `Control`, `A`, `B`, and `A+B`.
#' @param batch Optional batch label per sequence. When `NULL`, batches are
#'   inferred from each sequence's first observed calendar period.
#' @param latent_state Optional complete latent treatment-state matrix.
#' @param sequence_names Optional sequence labels.
#' @param missing Character markers treated as structurally unobserved.
#'
#' @return An object of class `sw_batched_design`, inheriting from
#'   `sw_component_design`.
#' @examples
#' bswd <- sw_batched_design(
#'   clusters_per_sequence = c(2, 2, 3, 3),
#'   state = rbind(
#'     `Group 1` = c("0", "1", "1+2", "1+2", "-"),
#'     `Group 2` = c("0", "0", "1", "1+2", "-"),
#'     `Group 3` = c("-", "0", "1", "1+2", "1+2"),
#'     `Group 4` = c("-", "0", "0", "1", "1+2")
#'   ),
#'   batch = c("Batch 1", "Batch 1", "Batch 2", "Batch 2")
#' )
#' bswd
#' @export
sw_batched_design <- function(
  clusters_per_sequence,
  state,
  batch = NULL,
  latent_state = NULL,
  sequence_names = NULL,
  missing = c("-", "--", "\u2013", "\u2014", "", "NA", ".")
) {
  base <- sw_incomplete_component_design(
    clusters_per_sequence = clusters_per_sequence,
    state = state,
    latent_state = latent_state,
    sequence_names = sequence_names,
    missing = missing
  )
  .attach_batched_metadata(base, batch)
}

#' Convert a component design to a batch-aware design
#'
#' @param design An [sw_component_design()] object.
#' @param batch Optional batch label per sequence. When `NULL`, batches are
#'   inferred from first observed calendar periods.
#' @return An `sw_batched_design` object.
#' @rdname sw_batched_design
#' @export
as_batched_design <- function(design, batch = NULL) {
  if (!inherits(design, "sw_component_design")) {
    stop("`design` must be an sw_component_design object.", call. = FALSE)
  }
  .attach_batched_metadata(design, batch)
}

.attach_batched_metadata <- function(design, batch = NULL) {
  observed <- .component_observed_matrix(design)
  first_observed <- apply(observed, 1L, function(x) min(which(x)))
  last_observed <- apply(observed, 1L, function(x) max(which(x)))

  batch_inferred <- is.null(batch)
  if (batch_inferred) {
    start_levels <- sort(unique(first_observed))
    batch <- paste0("Batch ", match(first_observed, start_levels))
  }
  if (length(batch) != design$n_sequences || anyNA(batch)) {
    stop("`batch` must contain one non-missing label per sequence.", call. = FALSE)
  }
  batch <- trimws(as.character(batch))
  if (any(!nzchar(batch))) {
    stop("`batch` labels must be non-empty.", call. = FALSE)
  }

  input_batch_names <- unique(batch)
  input_batch_start <- vapply(input_batch_names, function(label) {
    starts <- unique(first_observed[batch == label])
    if (length(starts) != 1L) {
      stop(
        "All sequences in batch `", label,
        "` must share the same first observed calendar period. ",
        "Assign different batch labels to sequences with different starts.",
        call. = FALSE
      )
    }
    starts
  }, integer(1))
  chronological <- order(input_batch_start, match(input_batch_names, unique(batch)))
  batch_names <- input_batch_names[chronological]
  batch_idx <- match(batch, batch_names)

  batch_start <- integer(length(batch_names))
  batch_end <- integer(length(batch_names))
  for (b in seq_along(batch_names)) {
    members <- which(batch_idx == b)
    batch_start[b] <- unique(first_observed[members])
    batch_end[b] <- max(last_observed[members])
  }
  names(batch_start) <- names(batch_end) <- batch_names

  calendar_period <- matrix(
    rep(seq_len(design$n_periods), each = design$n_sequences),
    nrow = design$n_sequences,
    ncol = design$n_periods,
    dimnames = dimnames(design$state)
  )
  time_on_trial <- matrix(
    NA_integer_, design$n_sequences, design$n_periods,
    dimnames = dimnames(design$state)
  )
  for (s in seq_len(design$n_sequences)) {
    local <- seq_len(design$n_periods) - batch_start[batch_idx[s]] + 1L
    local[local < 1L] <- NA_integer_
    time_on_trial[s, ] <- local
  }
  if (any(is.na(time_on_trial[observed]))) {
    stop(
      "Every observed cell must occur on or after its batch initiation.",
      call. = FALSE
    )
  }

  sequences_per_batch <- tabulate(batch_idx, nbins = length(batch_names))
  clusters_per_batch <- vapply(
    seq_along(batch_names),
    function(b) sum(design$clusters_per_sequence[batch_idx == b]),
    numeric(1)
  )
  names(sequences_per_batch) <- names(clusters_per_batch) <- batch_names

  design$batch <- batch
  design$batch_idx <- as.integer(batch_idx)
  design$batch_names <- batch_names
  design$n_batches <- length(batch_names)
  design$batch_inferred <- batch_inferred
  design$batch_start_period <- as.integer(batch_start)
  names(design$batch_start_period) <- batch_names
  design$batch_end_period <- as.integer(batch_end)
  names(design$batch_end_period) <- batch_names
  design$batch_delay <- as.integer(batch_start - min(batch_start))
  names(design$batch_delay) <- batch_names
  design$batch_gap_from_previous <- c(0L, as.integer(diff(batch_start)))
  names(design$batch_gap_from_previous) <- batch_names
  design$sequences_per_batch <- as.integer(sequences_per_batch)
  names(design$sequences_per_batch) <- batch_names
  design$clusters_per_batch <- as.integer(clusters_per_batch)
  names(design$clusters_per_batch) <- batch_names
  design$calendar_period <- calendar_period
  design$time_on_trial <- time_on_trial
  design$design_type <- if (length(unique(batch_start)) == 1L) {
    "classic_swd"
  } else {
    "bswd"
  }
  class(design) <- unique(c("sw_batched_design", class(design)))
  design
}

#' @param x An `sw_batched_design` object.
#' @param ... Unused.
#' @rdname sw_batched_design
#' @export
print.sw_batched_design <- function(x, ...) {
  cat("<sw_batched_design>\n")
  cat(sprintf(
    "  type: %s; %d batch(es), %d sequence(s), %d calendar period(s)\n",
    if (x$design_type == "classic_swd") "classic SWD" else "BSWD",
    x$n_batches, x$n_sequences, x$n_periods
  ))
  batch_table <- data.frame(
    batch = x$batch_names,
    start = x$batch_start_period,
    delay_from_first = x$batch_delay,
    gap_from_previous = x$batch_gap_from_previous,
    sequences = x$sequences_per_batch,
    clusters = x$clusters_per_batch,
    stringsAsFactors = FALSE
  )
  print(batch_table, row.names = FALSE)
  cat(sprintf(
    "  observed cluster-periods: %d of %d\n",
    x$n_observed_cluster_periods, x$n_calendar_cluster_periods
  ))
  print(x$display_state, quote = FALSE)
  invisible(x)
}
