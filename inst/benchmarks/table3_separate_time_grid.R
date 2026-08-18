# Reproduce Lin et al. (2026), Table 3, using swdpwr's marginal GEE
# power calculation under a logit link and the separate-time equivalence.

.sp_bench_table3_design <- function(steps, batches) {
  one_batch <- .sp_bench_one_batch_design(steps)
  do.call(rbind, replicate(batches, one_batch, simplify = FALSE))
}

.sp_bench_table3_one <- function(row, run_external) {
  if (!isTRUE(run_external)) {
    return(data.frame(
      design = row$design,
      p0 = row$p0,
      p1 = row$p1,
      icc = row$icc,
      reproduced_power = NA_real_,
      status = "skipped",
      package_version = .sp_bench_package_version("swdpwr"),
      warning_message = NA_character_,
      error_message = "External benchmarks disabled.",
      stringsAsFactors = FALSE
    ))
  }
  if (!requireNamespace("swdpwr", quietly = TRUE)) {
    return(data.frame(
      design = row$design,
      p0 = row$p0,
      p1 = row$p1,
      icc = row$icc,
      reproduced_power = NA_real_,
      status = "skipped",
      package_version = NA_character_,
      warning_message = NA_character_,
      error_message = "Optional package 'swdpwr' is not installed.",
      stringsAsFactors = FALSE
    ))
  }

  design_matrix <- .sp_bench_table3_design(
    steps = row$steps_per_batch,
    batches = row$batches
  )
  captured <- .sp_bench_capture(
    swdpwr::swdpower(
      K = row$cluster_period_size,
      design = design_matrix,
      family = "binomial",
      model = "marginal",
      link = "logit",
      type = "cross-sectional",
      meanresponse_start = row$p0,
      meanresponse_end0 = row$p0 + 0.0001,
      meanresponse_end1 = row$p1,
      typeIerror = 0.05,
      alpha0 = row$icc,
      alpha1 = row$icc
    )
  )
  power <- .sp_bench_extract_power(captured$value)
  status <- if (!is.na(captured$error)) {
    "error"
  } else if (!is.finite(power)) {
    "nonfinite"
  } else {
    "evaluated"
  }

  data.frame(
    design = row$design,
    p0 = row$p0,
    p1 = row$p1,
    icc = row$icc,
    reproduced_power = power,
    status = status,
    package_version = .sp_bench_package_version("swdpwr"),
    warning_message = if (length(captured$warnings)) {
      paste(captured$warnings, collapse = " | ")
    } else {
      NA_character_
    },
    error_message = captured$error,
    stringsAsFactors = FALSE
  )
}

run_table3_benchmark <- function(
  output_dir = NULL,
  benchmark_dir = .sp_bench_default_dir(),
  run_external = TRUE,
  strict = FALSE,
  tolerance = 0.002,
  write_outputs = TRUE
) {
  reference <- .sp_bench_read_reference(
    "table3_reference.csv", benchmark_dir
  )
  numeric_columns <- c(
    "steps_per_batch", "periods_per_batch", "batches", "total_clusters",
    "cluster_period_size", "total_sample_size", "p0", "p1", "icc",
    "reference_power"
  )
  reference[numeric_columns] <- lapply(
    reference[numeric_columns], as.numeric
  )

  rows <- split(reference, seq_len(nrow(reference)))
  observed <- do.call(rbind, lapply(rows, function(row) {
    .sp_bench_table3_one(row, run_external)
  }))
  rownames(observed) <- NULL

  validation <- .sp_bench_validation_rows(
    artifact = "Table 3",
    observed = observed,
    reference = reference,
    keys = c("design", "p0", "p1", "icc"),
    tolerance = tolerance,
    rounded_digits = 3L
  )
  validation <- validation[
    order(
      validation$steps_per_batch,
      validation$p0,
      validation$icc
    ),
  ]
  rownames(validation) <- NULL
  .sp_bench_stop_if_failed(validation, strict, "Table 3")

  output_dir <- if (isTRUE(write_outputs)) {
    .sp_bench_make_output_dir(output_dir)
  } else {
    NULL
  }
  .sp_bench_write_csv(
    validation, "table3_separate_time_grid.csv", output_dir
  )

  list(
    reference = reference,
    results = observed,
    validation = validation,
    package_versions = data.frame(
      package = "swdpwr",
      version = .sp_bench_package_version("swdpwr"),
      stringsAsFactors = FALSE
    )
  )
}
