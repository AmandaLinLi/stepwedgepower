# Reproduce the calendar-time delay curves in Lin et al. (2026), Figure 3.
#
# This is an independent base-R matrix-inversion implementation of the pooled
# binary-variance LMM approximation used in the manuscript's public code.

.sp_bench_figure3_one <- function(
  steps,
  delay,
  p0 = 0.1,
  p1 = 0.2,
  icc = 0.01,
  cluster_period_size = 50,
  alpha = 0.05
) {
  steps <- as.integer(steps)
  delay <- as.integer(delay)
  periods <- steps + 1L
  if (delay < 0L || delay > periods) {
    stop("`delay` must be between 0 and periods per batch.", call. = FALSE)
  }

  one_batch <- .sp_bench_one_batch_design(steps)
  treatment <- rbind(one_batch, one_batch)
  total_clusters <- 2L * steps
  total_calendar_periods <- periods + delay

  pooled_probability <- (p0 + p1) / 2
  marginal_variance <- pooled_probability * (1 - pooled_probability)
  cluster_variance <- icc * marginal_variance / (1 - icc)
  cluster_covariance <- cluster_variance * matrix(
    1, nrow = periods, ncol = periods
  ) + diag(marginal_variance / cluster_period_size, periods)
  covariance <- kronecker(diag(total_clusters), cluster_covariance)

  local_time <- do.call(
    rbind,
    replicate(steps, diag(periods), simplify = FALSE)
  )
  batch_1_time <- cbind(
    local_time,
    matrix(0, nrow(local_time), delay)
  )
  batch_2_time <- cbind(
    matrix(0, nrow(local_time), delay),
    local_time
  )
  time_design <- rbind(batch_1_time, batch_2_time)
  treatment_vector <- as.vector(t(treatment))
  design_matrix <- cbind(time_design, treatment_vector)

  information <- crossprod(
    design_matrix,
    .sp_bench_mpinv(covariance) %*% design_matrix
  )
  coefficient_covariance <- .sp_bench_mpinv(information)
  treatment_variance <- coefficient_covariance[
    ncol(coefficient_covariance),
    ncol(coefficient_covariance)
  ]
  power <- .sp_bench_manuscript_power(
    effect = p1 - p0,
    variance = treatment_variance,
    alpha = alpha
  )

  data.frame(
    design = paste(steps, "x", periods),
    steps_per_batch = steps,
    periods_per_batch = periods,
    batches = 2L,
    total_clusters = total_clusters,
    delay = delay,
    cluster_period_size = cluster_period_size,
    p0 = p0,
    p1 = p1,
    icc = icc,
    variance = treatment_variance,
    power = power,
    fixed_effect_rank = qr(design_matrix)$rank,
    fixed_effect_columns = ncol(design_matrix),
    stringsAsFactors = FALSE
  )
}

.sp_bench_plot_figure3 <- function(data, file = NULL) {
  if (!is.null(file)) {
    grDevices::pdf(file, width = 7.0, height = 4.8, onefile = TRUE)
    on.exit(grDevices::dev.off(), add = TRUE)
  }

  designs <- unique(data$design)
  line_types <- c(1, 2, 3)
  point_types <- c(1, 2, 0)
  x_range <- range(data$delay)
  y_range <- range(c(0, data$power), finite = TRUE)
  graphics::plot(
    NA,
    xlim = x_range,
    ylim = c(0, min(1, max(1, y_range[2L] + 0.02))),
    xlab = "Delay in batch start time",
    ylab = "Power",
    main = "Calendar-time power across delayed batch starts",
    las = 1
  )
  for (i in seq_along(designs)) {
    rows <- data[data$design == designs[i], , drop = FALSE]
    graphics::lines(
      rows$delay,
      rows$power,
      type = "b",
      lty = line_types[i],
      pch = point_types[i],
      lwd = 1.2
    )
  }
  graphics::legend(
    "bottomright",
    legend = paste(designs, "design"),
    lty = line_types[seq_along(designs)],
    pch = point_types[seq_along(designs)],
    bty = "n",
    title = "Single-batch design"
  )
  invisible(data)
}

run_figure3_benchmark <- function(
  output_dir = NULL,
  benchmark_dir = .sp_bench_default_dir(),
  strict = FALSE,
  tolerance = 1e-10,
  write_outputs = TRUE,
  make_plot = TRUE
) {
  reference <- .sp_bench_read_reference(
    "figure3_reference.csv", benchmark_dir
  )
  numeric_columns <- c(
    "steps_per_batch", "periods_per_batch", "batches", "total_clusters",
    "delay", "cluster_period_size", "p0", "p1", "icc", "variance", "power",
    "fixed_effect_rank"
  )
  reference[numeric_columns] <- lapply(
    reference[numeric_columns], as.numeric
  )

  settings <- unique(reference[c(
    "steps_per_batch", "periods_per_batch", "delay",
    "cluster_period_size", "p0", "p1", "icc"
  )])
  rows <- split(settings, seq_len(nrow(settings)))
  reproduced <- do.call(rbind, lapply(rows, function(row) {
    .sp_bench_figure3_one(
      steps = row$steps_per_batch,
      delay = row$delay,
      p0 = row$p0,
      p1 = row$p1,
      icc = row$icc,
      cluster_period_size = row$cluster_period_size
    )
  }))
  rownames(reproduced) <- NULL

  validation <- merge(
    reference,
    reproduced,
    by = c(
      "design", "steps_per_batch", "periods_per_batch", "batches",
      "total_clusters", "delay", "cluster_period_size", "p0", "p1", "icc"
    ),
    suffixes = c("_reference", "_reproduced"),
    all.x = TRUE,
    sort = FALSE
  )
  validation$variance_absolute_difference <- abs(
    validation$variance_reproduced - validation$variance_reference
  )
  validation$power_absolute_difference <- abs(
    validation$power_reproduced - validation$power_reference
  )
  validation$tolerance <- tolerance
  validation$pass <- is.finite(validation$variance_absolute_difference) &
    is.finite(validation$power_absolute_difference) &
    validation$variance_absolute_difference <= tolerance &
    validation$power_absolute_difference <= tolerance &
    validation$fixed_effect_rank_reproduced ==
      validation$fixed_effect_rank_reference
  validation$artifact <- "Figure 3"
  validation$status <- "evaluated"
  validation <- validation[
    order(validation$steps_per_batch, validation$delay),
  ]
  rownames(validation) <- NULL
  .sp_bench_stop_if_failed(validation, strict, "Figure 3")

  # The manuscript states that the maximal non-overlap delay has the same
  # information as the no-delay design under the corresponding time models.
  endpoint_check <- do.call(rbind, lapply(
    split(reproduced, reproduced$design),
    function(x) {
      x <- x[order(x$delay), , drop = FALSE]
      data.frame(
        design = x$design[1L],
        no_delay_power = x$power[1L],
        maximal_delay_power = x$power[nrow(x)],
        absolute_difference = abs(x$power[1L] - x$power[nrow(x)]),
        pass = abs(x$power[1L] - x$power[nrow(x)]) <= tolerance,
        stringsAsFactors = FALSE
      )
    }
  ))
  rownames(endpoint_check) <- NULL
  if (isTRUE(strict) && any(!endpoint_check$pass)) {
    stop("Figure 3 terminal-delay invariance check failed.", call. = FALSE)
  }

  output_dir <- if (isTRUE(write_outputs)) {
    .sp_bench_make_output_dir(output_dir)
  } else {
    NULL
  }
  .sp_bench_write_csv(
    reproduced, "figure3_calendar_delay_values.csv", output_dir
  )
  .sp_bench_write_csv(
    validation, "figure3_calendar_delay_validation.csv", output_dir
  )
  .sp_bench_write_csv(
    endpoint_check, "figure3_terminal_delay_check.csv", output_dir
  )
  if (isTRUE(make_plot) && !is.null(output_dir)) {
    .sp_bench_plot_figure3(
      reproduced,
      file.path(output_dir, "figure3_calendar_delay.pdf")
    )
  }

  list(
    reference = reference,
    results = reproduced,
    validation = validation,
    endpoint_check = endpoint_check
  )
}
