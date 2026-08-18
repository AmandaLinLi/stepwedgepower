# Internal helpers for the stepwedgepower benchmark suite.
#
# These functions live under inst/benchmarks rather than R/ so they do not
# expand the package's public API. They are intended to be sourced by the
# benchmark scripts and the reproducible benchmark vignette.

.sp_bench_default_dir <- function() {
  configured <- getOption("stepwedgepower.benchmark_dir")
  if (!is.null(configured) && nzchar(configured)) {
    return(normalizePath(configured, mustWork = TRUE))
  }

  installed <- system.file("benchmarks", package = "stepwedgepower")
  if (nzchar(installed)) {
    return(normalizePath(installed, mustWork = TRUE))
  }

  candidates <- c(
    file.path(getwd(), "inst", "benchmarks"),
    file.path(getwd(), "..", "inst", "benchmarks"),
    getwd()
  )
  existing <- candidates[dir.exists(candidates)]
  if (!length(existing)) {
    stop(
      "Could not locate the stepwedgepower benchmark directory. ",
      "Set options(stepwedgepower.benchmark_dir = <path>).",
      call. = FALSE
    )
  }
  normalizePath(existing[1L], mustWork = TRUE)
}

.sp_bench_reference_path <- function(file, benchmark_dir = .sp_bench_default_dir()) {
  path <- file.path(benchmark_dir, "reference", file)
  if (!file.exists(path)) {
    stop("Benchmark reference file not found: ", path, call. = FALSE)
  }
  path
}

.sp_bench_read_reference <- function(file, benchmark_dir = .sp_bench_default_dir()) {
  utils::read.csv(
    .sp_bench_reference_path(file, benchmark_dir),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.sp_bench_make_output_dir <- function(output_dir) {
  if (is.null(output_dir) || !nzchar(output_dir)) {
    return(NULL)
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  normalizePath(output_dir, mustWork = TRUE)
}

.sp_bench_write_csv <- function(x, file, output_dir) {
  if (is.null(output_dir)) return(invisible(NULL))
  utils::write.csv(x, file.path(output_dir, file), row.names = FALSE)
  invisible(file.path(output_dir, file))
}

.sp_bench_capture <- function(expr) {
  warnings <- character()
  error_message <- NA_character_
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      error_message <<- conditionMessage(e)
      NULL
    }
  )
  list(
    value = value,
    error = error_message,
    warnings = unique(warnings)
  )
}

.sp_bench_extract_power <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (is.numeric(x) && length(x) == 1L && is.finite(x)) {
    return(as.numeric(x))
  }
  if (is.list(x) && !is.null(x$Power) &&
      is.numeric(x$Power) && length(x$Power) == 1L) {
    return(as.numeric(x$Power))
  }
  if (is.list(x) && !is.null(x$pwrGLM) &&
      is.numeric(x$pwrGLM) && length(x$pwrGLM) == 1L) {
    return(as.numeric(x$pwrGLM))
  }
  flattened <- suppressWarnings(unlist(x, recursive = TRUE, use.names = TRUE))
  if (!length(flattened)) return(NA_real_)
  numeric_values <- suppressWarnings(as.numeric(flattened))
  names(numeric_values) <- names(flattened)
  candidates <- grep(
    "(^|\\.)Power$|(^|\\.)power$|pwrGLM$|(^|\\.)pwr$",
    names(numeric_values),
    value = TRUE
  )
  for (candidate in candidates) {
    value <- numeric_values[[candidate]]
    if (length(value) == 1L && is.finite(value)) return(as.numeric(value))
  }
  finite <- numeric_values[is.finite(numeric_values)]
  if (length(finite) == 1L) return(as.numeric(finite))
  NA_real_
}

.sp_bench_package_version <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) return(NA_character_)
  as.character(utils::packageVersion(package))
}

.sp_bench_validation_rows <- function(
  artifact,
  observed,
  reference,
  keys,
  tolerance,
  rounded_digits = NULL
) {
  merged <- merge(
    reference,
    observed,
    by = keys,
    all.x = TRUE,
    sort = FALSE
  )
  compare_observed <- merged$reproduced_power
  compare_reference <- merged$reference_power
  if (!is.null(rounded_digits)) {
    compare_observed <- round(compare_observed, rounded_digits)
    compare_reference <- round(compare_reference, rounded_digits)
  }
  merged$absolute_difference <- abs(compare_observed - compare_reference)
  merged$tolerance <- tolerance

  # A comparison that is mathematically on the tolerance boundary can be
  # represented a few machine units above that boundary. For example,
  # abs(0.866 - 0.868) may be stored as 0.0020000000000000018. Add only
  # scale-aware floating-point slack; this does not relax the substantive
  # benchmark tolerance.
  comparison_scale <- pmax(
    1, abs(compare_observed), abs(compare_reference),
    na.rm = TRUE
  )
  floating_slack <- 100 * .Machine$double.eps * comparison_scale

  merged$pass <- ifelse(
    merged$status %in% c("skipped", "reference_only"),
    NA,
    is.finite(merged$absolute_difference) &
      merged$absolute_difference <= tolerance + floating_slack
  )
  merged$artifact <- artifact
  merged
}

.sp_bench_stop_if_failed <- function(validation, strict, artifact) {
  if (!isTRUE(strict)) return(invisible(validation))

  failed <- !is.na(validation$pass) & !validation$pass
  if (any(failed)) {
    failed_rows <- validation[failed, , drop = FALSE]
    detail_columns <- intersect(
      c(
        "scenario", "method", "reference_power",
        "reproduced_power", "absolute_difference", "tolerance",
        "package_version", "status", "warning_message",
        "error_message"
      ),
      names(failed_rows)
    )
    details <- paste(
      capture.output(print(
        failed_rows[, detail_columns, drop = FALSE],
        row.names = FALSE, digits = 8
      )),
      collapse = "\n"
    )
    stop(
      paste0(
        artifact, " benchmark failed for ", sum(failed),
        " comparison(s):\n", details
      ),
      call. = FALSE
    )
  }
  invisible(validation)
}

.sp_bench_mpinv <- function(x, tolerance = NULL) {
  x <- as.matrix(x)
  decomposition <- svd(x)
  if (is.null(tolerance)) {
    tolerance <- max(dim(x)) * max(decomposition$d) * .Machine$double.eps
  }
  inverse_values <- ifelse(
    decomposition$d > tolerance,
    1 / decomposition$d,
    0
  )
  decomposition$v %*%
    (inverse_values * t(decomposition$u))
}

.sp_bench_one_batch_design <- function(steps) {
  steps <- as.integer(steps)
  if (length(steps) != 1L || is.na(steps) || steps < 1L) {
    stop("`steps` must be one positive integer.", call. = FALSE)
  }
  periods <- steps + 1L
  out <- matrix(0, nrow = steps, ncol = periods)
  for (sequence in seq_len(steps)) {
    out[sequence, seq.int(sequence + 1L, periods)] <- 1
  }
  rownames(out) <- paste0("Sequence ", seq_len(steps))
  colnames(out) <- paste0("Period ", seq_len(periods))
  out
}

.sp_bench_manuscript_power <- function(effect, variance, alpha = 0.05) {
  if (!is.finite(variance) || variance <= 0) return(NA_real_)
  stats::pnorm(
    abs(effect) / sqrt(variance) - stats::qnorm(1 - alpha / 2)
  )
}

.sp_bench_wide_reference <- function(reference, row, column, value) {
  rows <- unique(reference[[row]])
  columns <- unique(reference[[column]])
  out <- data.frame(rows, stringsAsFactors = FALSE)
  names(out)[1L] <- row
  for (column_value in columns) {
    index <- match(
      paste(rows, column_value, sep = "\r"),
      paste(reference[[row]], reference[[column]], sep = "\r")
    )
    out[[column_value]] <- reference[[value]][index]
  }
  out
}

.sp_bench_status_summary <- function(...) {
  objects <- list(...)

  # Validation tables for Table 2, Figure 3, Table 3, and Figure 5 contain
  # artifact-specific columns. Bind only the two fields required for the
  # cross-artifact summary; base::rbind() otherwise fails when schemas differ.
  pieces <- lapply(objects, function(x) {
    validation <- x$validation
    if (is.null(validation) || !is.data.frame(validation) || !nrow(validation)) {
      return(NULL)
    }
    required <- c("artifact", "pass")
    missing <- setdiff(required, names(validation))
    if (length(missing)) {
      stop(
        "Benchmark validation table is missing required column(s): ",
        paste(missing, collapse = ", "),
        call. = FALSE
      )
    }
    data.frame(
      artifact = as.character(validation$artifact),
      pass = as.logical(validation$pass),
      stringsAsFactors = FALSE
    )
  })
  pieces <- Filter(Negate(is.null), pieces)

  if (!length(pieces)) {
    return(data.frame(
      artifact = character(), evaluated = integer(),
      passed = integer(), failed = integer(), skipped = integer(),
      stringsAsFactors = FALSE
    ))
  }

  data <- do.call(rbind, pieces)
  artifacts <- unique(data$artifact)
  out <- do.call(rbind, lapply(artifacts, function(name) {
    rows <- data[data$artifact == name, , drop = FALSE]
    data.frame(
      artifact = name,
      evaluated = sum(!is.na(rows$pass)),
      passed = sum(rows$pass %in% TRUE, na.rm = TRUE),
      failed = sum(rows$pass %in% FALSE, na.rm = TRUE),
      skipped = sum(is.na(rows$pass)),
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}
