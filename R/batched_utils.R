# Internal helpers for batched stepped-wedge designs -------------------------

.batched_time_models <- function() {
  c("calendar", "time_on_trial", "separate")
}

.match_batched_time_model <- function(time_model) {
  match.arg(time_model, .batched_time_models())
}

.batched_default_contrasts <- function(design, include_interaction = TRUE) {
  if (!inherits(design, "sw_component_design")) {
    stop("`design` must inherit from sw_component_design.", call. = FALSE)
  }

  states <- unique(as.integer(design$state[.component_observed_matrix(design)]))
  out <- character()
  if (all(c(0L, 1L) %in% states)) out <- c(out, "A_vs_control")
  if (all(c(0L, 2L) %in% states)) out <- c(out, "B_vs_control")
  if (all(c(1L, 2L) %in% states)) out <- c(out, "B_vs_A")
  if (all(c(0L, 3L) %in% states)) out <- c(out, "AB_vs_control")
  if (all(c(1L, 3L) %in% states)) out <- c(out, "AB_vs_A")
  if (all(c(2L, 3L) %in% states)) out <- c(out, "AB_vs_B")
  if (isTRUE(include_interaction) && all(0:3 %in% states)) {
    out <- c(out, "interaction")
  }

  if (!length(out)) {
    stop(
      "No standard component contrast is supported by the observed states.",
      call. = FALSE
    )
  }
  unique(out)
}

.batched_required_components <- function(contrasts, include_interaction = TRUE) {
  contrasts <- .validate_component_contrasts(contrasts, include_interaction)
  matrix <- .component_contrast_matrix(include_interaction)
  needed <- colSums(abs(matrix[contrasts, , drop = FALSE])) > 0
  names(needed)[needed]
}

.batched_time_terms <- function(time_model, n_batches = 1L) {
  time_model <- .match_batched_time_model(time_model)
  if (time_model == "calendar") {
    return("factor(calendar_period)")
  }
  if (time_model == "time_on_trial") {
    return("factor(time_on_trial)")
  }
  if (n_batches <= 1L) {
    return("factor(time_on_trial)")
  }
  # One categorical level per observed batch-by-local-time cell gives each
  # batch its own time profile and remains stable when observed lengths differ.
  "factor(batch_period)"
}

.batched_fixed_formula <- function(
  data,
  contrasts,
  include_interaction = TRUE,
  time_model = c("calendar", "time_on_trial", "separate"),
  adjust_sequence = FALSE,
  include_response = FALSE,
  outcome = c("events", "n"),
  cluster = "cluster_id"
) {
  time_model <- .match_batched_time_model(time_model)
  contrasts <- .validate_component_contrasts(contrasts, include_interaction)
  required_columns <- c(
    "a_effect_weight", "b_effect_weight", "ab_effect_weight",
    "calendar_period", "time_on_trial", "batch", "batch_period", "sequence"
  )
  if (isTRUE(include_response)) {
    required_columns <- c(required_columns, outcome, cluster)
  }
  .check_required_columns(data, unique(required_columns))

  needed <- .batched_required_components(contrasts, include_interaction)
  component_terms <- c(
    a = "a_effect_weight",
    b = "b_effect_weight",
    ab = "ab_effect_weight"
  )[needed]

  # Include varying nuisance component terms even when they are not themselves
  # part of a requested contrast. This preserves the intended component model.
  varying <- c(
    a = length(unique(data$a_effect_weight)) > 1L,
    b = length(unique(data$b_effect_weight)) > 1L,
    ab = isTRUE(include_interaction) &&
      length(unique(data$ab_effect_weight)) > 1L
  )
  nuisance_terms <- c(
    a = "a_effect_weight",
    b = "b_effect_weight",
    ab = "ab_effect_weight"
  )[varying]
  component_terms <- unique(c(component_terms, nuisance_terms))

  rhs <- c(
    unname(component_terms),
    .batched_time_terms(time_model, length(unique(data$batch)))
  )
  if (isTRUE(adjust_sequence) && length(unique(data$sequence)) > 1L) {
    rhs <- c(rhs, "factor(sequence)")
  }
  if (isTRUE(include_response)) {
    lhs <- sprintf(
      "cbind(%s, %s - %s)", outcome[1L], outcome[2L], outcome[1L]
    )
    rhs <- c(rhs, sprintf("(1 | %s)", cluster))
    return(stats::as.formula(paste(lhs, "~", paste(rhs, collapse = " + "))))
  }
  stats::as.formula(paste("~", paste(rhs, collapse = " + ")))
}

.append_batched_time_columns <- function(data, design) {
  if (!inherits(design, "sw_batched_design")) return(data)
  .check_required_columns(data, c("sequence_idx", "period"))
  index <- cbind(as.integer(data$sequence_idx), as.integer(data$period))
  data$batch <- design$batch[as.integer(data$sequence_idx)]
  data$batch_idx <- design$batch_idx[as.integer(data$sequence_idx)]
  data$calendar_period <- as.integer(data$period)
  data$time_on_trial <- as.integer(design$time_on_trial[index])
  data$batch_period <- ifelse(
    is.na(data$time_on_trial),
    NA_character_,
    paste0(data$batch, ":", data$time_on_trial)
  )
  data
}

.validate_batched_time_effects <- function(time_model, time_effects) {
  time_model <- .match_batched_time_model(time_model)
  if (is.null(time_effects)) return(NULL)

  if (time_model %in% c("calendar", "time_on_trial")) {
    if (!is.numeric(time_effects) || !length(time_effects) ||
        anyNA(time_effects) || any(!is.finite(time_effects))) {
      stop(
        "`time_effects` must be a finite numeric vector for the ",
        time_model, " time model.",
        call. = FALSE
      )
    }
    return(as.numeric(time_effects))
  }

  if (is.data.frame(time_effects)) time_effects <- as.matrix(time_effects)
  if (is.matrix(time_effects)) {
    if (!is.numeric(time_effects) || !length(time_effects) ||
        anyNA(time_effects) || any(!is.finite(time_effects))) {
      stop(
        "Matrix `time_effects` must contain finite numeric values.",
        call. = FALSE
      )
    }
    storage.mode(time_effects) <- "double"
    return(time_effects)
  }
  if (is.list(time_effects)) {
    if (!length(time_effects) || any(vapply(
      time_effects,
      function(x) !is.numeric(x) || !length(x) || anyNA(x) ||
        any(!is.finite(x)),
      logical(1)
    ))) {
      stop(
        "List `time_effects` must contain non-empty finite numeric vectors.",
        call. = FALSE
      )
    }
    return(lapply(time_effects, as.numeric))
  }

  stop(
    "For `time_model = \"separate\"`, `time_effects` must be a numeric ",
    "matrix, data frame, or list with one profile per batch.",
    call. = FALSE
  )
}

.resolve_profile_values <- function(profile, index, label) {
  if (is.null(profile)) return(rep(0, length(index)))
  if (length(profile) == 1L) return(rep(as.numeric(profile), length(index)))
  positive <- !is.na(index) & index > 0L
  if (any(index[positive] > length(profile))) {
    stop(
      "The ", label, " time-effect profile is shorter than the required ",
      "time index.",
      call. = FALSE
    )
  }
  out <- rep(0, length(index))
  out[positive] <- as.numeric(profile[index[positive]])
  out
}

.resolve_batched_time_effect_vector <- function(
  design,
  assumptions,
  sequence_idx,
  period
) {
  if (!inherits(design, "sw_batched_design")) {
    stop("`design` must be an sw_batched_design object.", call. = FALSE)
  }
  if (!inherits(assumptions, "sw_batched_assumptions")) {
    stop("`assumptions` must be an sw_batched_assumptions object.", call. = FALSE)
  }
  time_model <- assumptions$time_model
  effects <- assumptions$time_effects
  sequence_idx <- as.integer(sequence_idx)
  period <- as.integer(period)
  observed <- .component_observed_matrix(design)[cbind(sequence_idx, period)]

  if (time_model == "calendar") {
    calendar_index <- period
    calendar_index[!observed] <- NA_integer_
    return(.resolve_profile_values(effects, calendar_index, "calendar"))
  }

  local <- design$time_on_trial[cbind(sequence_idx, period)]
  local[!observed] <- NA_integer_
  if (time_model == "time_on_trial") {
    return(.resolve_profile_values(effects, local, "time-on-trial"))
  }

  if (is.null(effects)) return(rep(0, length(period)))
  batch_idx <- design$batch_idx[sequence_idx]
  out <- numeric(length(period))

  if (is.matrix(effects)) {
    if (!is.null(rownames(effects))) {
      row_index <- match(design$batch_names, rownames(effects))
      if (anyNA(row_index)) {
        stop(
          "Row names of separate `time_effects` must include every batch: ",
          paste(design$batch_names, collapse = ", "), ".",
          call. = FALSE
        )
      }
      effects <- effects[row_index, , drop = FALSE]
    } else if (nrow(effects) != design$n_batches) {
      stop(
        "Separate `time_effects` must have one row per batch (",
        design$n_batches, ").",
        call. = FALSE
      )
    }
    for (b in seq_len(design$n_batches)) {
      rows <- which(batch_idx == b)
      out[rows] <- .resolve_profile_values(
        effects[b, ], local[rows],
        paste0("separate profile for batch ", design$batch_names[b])
      )
    }
    return(out)
  }

  profiles <- effects
  if (!is.null(names(profiles)) && all(nzchar(names(profiles)))) {
    order <- match(design$batch_names, names(profiles))
    if (anyNA(order)) {
      stop(
        "Names of separate `time_effects` must include every batch: ",
        paste(design$batch_names, collapse = ", "), ".",
        call. = FALSE
      )
    }
    profiles <- profiles[order]
  } else if (length(profiles) != design$n_batches) {
    stop(
      "Separate `time_effects` must contain one profile per batch (",
      design$n_batches, ").",
      call. = FALSE
    )
  }
  for (b in seq_len(design$n_batches)) {
    rows <- which(batch_idx == b)
    out[rows] <- .resolve_profile_values(
      profiles[[b]], local[rows],
      paste0("separate profile for batch ", design$batch_names[b])
    )
  }
  out
}
