# Internal model-fitting diagnostic helpers ---------------------------------

.fit_status_levels <- function() {
  c(
    "hard_glmm_error",
    "nonconverged_fit",
    "converged_nonfinite_contrast",
    "successful_fit"
  )
}

.fit_status_labels <- function() {
  c(
    hard_glmm_error = "Hard GLMM error",
    nonconverged_fit = "Nonconverged fit",
    converged_nonfinite_contrast =
      "Converged but non-finite contrast",
    singular_fit = "Singular fit",
    successful_fit = "Successful fit"
  )
}

.collapse_diagnostic_messages <- function(x) {
  if (is.null(x) || !length(x)) return(NA_character_)
  x <- as.character(unlist(x, use.names = FALSE))
  x <- trimws(x)
  x <- unique(x[!is.na(x) & nzchar(x)])
  if (!length(x)) NA_character_ else paste(x, collapse = " | ")
}

.partition_lme4_messages <- function(messages) {
  messages <- as.character(unlist(messages, use.names = FALSE))
  messages <- unique(
    trimws(messages[!is.na(messages) & nzchar(messages)])
  )
  singular_message <- grepl("singular", messages, ignore.case = TRUE)
  list(
    convergence = messages[!singular_message],
    advisory = messages[singular_message],
    all = messages
  )
}

.extract_lme4_convergence <- function(fit) {
  optimizer_code <- tryCatch(
    fit@optinfo$conv$opt,
    error = function(e) NULL
  )
  optimizer_numeric <- suppressWarnings(
    as.numeric(unlist(optimizer_code, use.names = FALSE))
  )
  optimizer_numeric <- optimizer_numeric[is.finite(optimizer_numeric)]
  optimizer_failed <- length(optimizer_numeric) > 0L &&
    any(optimizer_numeric != 0)

  lme4_messages <- tryCatch(
    fit@optinfo$conv$lme4$messages,
    error = function(e) NULL
  )
  # lme4 may place the advisory message "boundary (singular) fit" in the
  # same message slot as true convergence diagnostics. Singularity is a
  # separate, non-exclusive property in this package and must not by itself
  # cause an otherwise converged fit to be classified as nonconverged.
  partitioned_messages <- .partition_lme4_messages(lme4_messages)
  lme4_messages <- partitioned_messages$all
  convergence_messages <- partitioned_messages$convergence
  advisory_messages <- partitioned_messages$advisory

  messages <- convergence_messages
  if (optimizer_failed) {
    messages <- c(
      paste0(
        "Optimizer convergence code: ",
        paste(unique(optimizer_numeric), collapse = ", ")
      ),
      messages
    )
  }

  list(
    converged = !optimizer_failed && !length(convergence_messages),
    optimizer_code = if (length(optimizer_numeric)) {
      paste(unique(optimizer_numeric), collapse = ",")
    } else {
      NA_character_
    },
    messages = unique(messages),
    advisory_messages = unique(advisory_messages),
    all_lme4_messages = unique(lme4_messages)
  )
}

.classify_analysis_fit <- function(hard_error, converged, p_values) {
  if (isTRUE(hard_error)) return("hard_glmm_error")
  if (!isTRUE(converged)) return("nonconverged_fit")
  if (!length(p_values) || any(!is.finite(as.numeric(p_values)))) {
    return("converged_nonfinite_contrast")
  }
  "successful_fit"
}

.analysis_evaluability_counts <- function(
  finite_result,
  converged,
  hard_error,
  singular
) {
  finite_result <- as.logical(finite_result)
  converged <- as.logical(converged)
  hard_error <- as.logical(hard_error)
  singular <- as.logical(singular)

  n <- length(finite_result)
  if (length(converged) != n || length(hard_error) != n ||
      length(singular) != n) {
    stop("Internal diagnostic vectors must have equal lengths.", call. = FALSE)
  }

  hard <- hard_error %in% TRUE
  nonconverged <- !hard & !(converged %in% TRUE)
  nonfinite <- !hard & (converged %in% TRUE) & !(finite_result %in% TRUE)
  successful <- !hard & (converged %in% TRUE) & (finite_result %in% TRUE)
  singular_flag <- singular %in% TRUE

  list(
    hard_glmm_error = hard,
    nonconverged_fit = nonconverged,
    converged_nonfinite_contrast = nonfinite,
    singular_fit = singular_flag,
    successful_fit = successful,
    n_hard_glmm_error = sum(hard),
    n_nonconverged_fit = sum(nonconverged),
    n_converged_nonfinite_contrast = sum(nonfinite),
    n_singular_fit = sum(singular_flag),
    n_singular_successful_fit = sum(singular_flag & successful),
    n_successful_fit = sum(successful)
  )
}

.make_replicate_diagnostics <- function(
  fit_status,
  singular,
  has_warning,
  nonfinite_contrasts,
  error_message,
  convergence_message,
  optimizer_code = rep(NA_character_, length(fit_status))
) {
  fit_status <- as.character(fit_status)
  n <- length(fit_status)
  expected <- .fit_status_levels()
  if (anyNA(fit_status) || any(!fit_status %in% expected)) {
    stop("Internal fit statuses are invalid.", call. = FALSE)
  }

  normalize_character <- function(x) {
    if (length(x) != n) {
      stop("Internal diagnostic vectors must have equal lengths.", call. = FALSE)
    }
    x <- as.character(x)
    x[!nzchar(x) | is.na(x)] <- NA_character_
    x
  }

  if (length(singular) != n || length(has_warning) != n) {
    stop("Internal diagnostic vectors must have equal lengths.", call. = FALSE)
  }

  data.frame(
    simulation = seq_len(n),
    fit_status = fit_status,
    hard_glmm_error = fit_status == "hard_glmm_error",
    nonconverged_fit = fit_status == "nonconverged_fit",
    converged_nonfinite_contrast =
      fit_status == "converged_nonfinite_contrast",
    singular_fit = as.logical(singular),
    singular_assessed = !is.na(singular),
    successful_fit = fit_status == "successful_fit",
    warning_present = as.logical(has_warning),
    nonfinite_contrasts = normalize_character(nonfinite_contrasts),
    optimizer_code = normalize_character(optimizer_code),
    error_message = normalize_character(error_message),
    convergence_message = normalize_character(convergence_message),
    stringsAsFactors = FALSE
  )
}

.summarize_fit_diagnostics <- function(replicate_diagnostics) {
  required <- c(
    "fit_status", "singular_fit", "successful_fit",
    "hard_glmm_error", "nonconverged_fit",
    "converged_nonfinite_contrast"
  )
  missing <- setdiff(required, names(replicate_diagnostics))
  if (length(missing)) {
    stop(
      "Internal replicate diagnostics are missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  nsim <- nrow(replicate_diagnostics)
  labels <- .fit_status_labels()
  categories <- c(
    "hard_glmm_error",
    "nonconverged_fit",
    "converged_nonfinite_contrast",
    "singular_fit",
    "successful_fit"
  )
  counts <- c(
    sum(replicate_diagnostics$hard_glmm_error %in% TRUE),
    sum(replicate_diagnostics$nonconverged_fit %in% TRUE),
    sum(replicate_diagnostics$converged_nonfinite_contrast %in% TRUE),
    sum(replicate_diagnostics$singular_fit %in% TRUE),
    sum(replicate_diagnostics$successful_fit %in% TRUE)
  )

  data.frame(
    category = categories,
    label = unname(labels[categories]),
    count = as.integer(counts),
    rate = if (nsim) counts / nsim else NA_real_,
    denominator = nsim,
    classification = c(
      rep("Primary mutually exclusive status", 3),
      "Additional non-exclusive fit property",
      "Primary mutually exclusive status"
    ),
    definition = c(
      "glmer() returned no fitted model because an error was raised.",
      paste(
        "A model object was returned, but the optimizer or lme4",
        "convergence checks did not pass."
      ),
      paste(
        "The fit converged, but at least one requested raw contrast",
        "had a non-finite p-value."
      ),
      paste(
        "lme4::isSingular() returned TRUE. This flag can overlap",
        "with a successful fit and is not automatically a failure."
      ),
      paste(
        "The fit converged and every requested raw contrast had a",
        "finite p-value; singular fits may be included."
      )
    ),
    stringsAsFactors = FALSE
  )
}
