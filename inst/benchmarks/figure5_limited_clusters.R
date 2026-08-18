# Reproduce the scaled-variance curves in Lin et al. (2026), Figure 5.
#
# The variance expressions are divided by the common factor 2a:
# classic: (1 - phi) (2 phi + 1) / (phi + 1)
# one-unit delay: (1 + phi) / 2
# two-unit delay: 1

.sp_bench_figure5_values <- function(icc = seq(0, 1, by = 0.01)) {
  if (!is.numeric(icc) || anyNA(icc) || any(!is.finite(icc)) ||
      any(icc < 0 | icc > 1)) {
    stop("`icc` must contain finite values in [0, 1].", call. = FALSE)
  }
  data.frame(
    icc = icc,
    classic_scaled_variance =
      (1 - icc) * (2 * icc + 1) / (icc + 1),
    one_unit_delay_scaled_variance = (1 + icc) / 2,
    two_unit_delay_scaled_variance = rep(1, length(icc)),
    stringsAsFactors = FALSE
  )
}

.sp_bench_figure5_matrix_one <- function(delay, icc) {
  delay <- as.integer(delay)
  if (length(delay) != 1L || is.na(delay) || !delay %in% 0:2) {
    stop("`delay` must be one of 0, 1, or 2.", call. = FALSE)
  }
  if (length(icc) != 1L || !is.finite(icc) || icc < 0 || icc >= 1) {
    return(NA_real_)
  }

  # Set a = 1 without loss of generality. Then b = phi = ICC and the
  # within-cluster covariance is b J + (a - b) I. Each design contains
  # two clusters and three observed periods per cluster.
  cluster_covariance <- icc * matrix(1, 3, 3) +
    (1 - icc) * diag(3)
  covariance <- kronecker(diag(2), cluster_covariance)

  total_periods <- 3L + delay
  time_design <- matrix(0, nrow = 6L, ncol = total_periods)
  time_design[seq_len(3L), seq_len(3L)] <- diag(3L)
  second_times <- seq_len(3L) + delay
  time_design[3L + seq_len(3L), second_times] <- diag(3L)

  treatment <- c(0, 1, 1, 0, 0, 1)
  design_matrix <- cbind(time_design, treatment)
  information <- crossprod(
    design_matrix,
    .sp_bench_mpinv(covariance) %*% design_matrix
  )
  coefficient_covariance <- .sp_bench_mpinv(information)

  # The manuscript curves divide Var(theta-hat) by 2a, and a = 1 here.
  coefficient_covariance[ncol(coefficient_covariance),
                         ncol(coefficient_covariance)] / 2
}

.sp_bench_figure5_matrix_values <- function(icc) {
  data.frame(
    icc = icc,
    classic_matrix_scaled_variance = vapply(
      icc, function(x) .sp_bench_figure5_matrix_one(0L, x), numeric(1)
    ),
    one_unit_delay_matrix_scaled_variance = vapply(
      icc, function(x) .sp_bench_figure5_matrix_one(1L, x), numeric(1)
    ),
    two_unit_delay_matrix_scaled_variance = vapply(
      icc, function(x) .sp_bench_figure5_matrix_one(2L, x), numeric(1)
    ),
    stringsAsFactors = FALSE
  )
}

.sp_bench_plot_figure5 <- function(data, file = NULL) {
  if (!is.null(file)) {
    grDevices::pdf(file, width = 7.0, height = 4.8, onefile = TRUE)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  graphics::plot(
    data$icc,
    data$classic_scaled_variance,
    type = "l",
    lty = 1,
    lwd = 1.4,
    ylim = c(0, 1.05),
    xlab = "ICC",
    ylab = "Variance divided by 2a",
    main = "Limited-cluster variance comparison",
    las = 1
  )
  graphics::lines(
    data$icc,
    data$one_unit_delay_scaled_variance,
    lty = 2,
    lwd = 1.4
  )
  graphics::lines(
    data$icc,
    data$two_unit_delay_scaled_variance,
    lty = 3,
    lwd = 1.4
  )
  graphics::legend(
    "bottomleft",
    legend = c(
      "Classic",
      "Irregular with 1-unit delay",
      "Irregular with 2-unit delay"
    ),
    lty = 1:3,
    lwd = 1.4,
    bty = "n"
  )
  invisible(data)
}

run_figure5_benchmark <- function(
  output_dir = NULL,
  benchmark_dir = .sp_bench_default_dir(),
  strict = FALSE,
  tolerance = 1e-12,
  write_outputs = TRUE,
  make_plot = TRUE
) {
  reference <- .sp_bench_read_reference(
    "figure5_reference.csv", benchmark_dir
  )
  numeric_columns <- c(
    "icc", "classic_scaled_variance",
    "one_unit_delay_scaled_variance",
    "two_unit_delay_scaled_variance"
  )
  reference[numeric_columns] <- lapply(
    reference[numeric_columns], as.numeric
  )

  reproduced <- .sp_bench_figure5_values(reference$icc)
  matrix_reproduced <- .sp_bench_figure5_matrix_values(reference$icc)
  validation <- merge(
    reference,
    reproduced,
    by = "icc",
    suffixes = c("_reference", "_reproduced"),
    all.x = TRUE,
    sort = FALSE
  )
  validation <- merge(
    validation,
    matrix_reproduced,
    by = "icc",
    all.x = TRUE,
    sort = FALSE
  )
  for (name in c(
    "classic_scaled_variance",
    "one_unit_delay_scaled_variance",
    "two_unit_delay_scaled_variance"
  )) {
    validation[[paste0(name, "_absolute_difference")]] <- abs(
      validation[[paste0(name, "_reproduced")]] -
        validation[[paste0(name, "_reference")]]
    )
  }
  validation$classic_matrix_absolute_difference <- abs(
    validation$classic_matrix_scaled_variance -
      validation$classic_scaled_variance_reproduced
  )
  validation$one_unit_delay_matrix_absolute_difference <- abs(
    validation$one_unit_delay_matrix_scaled_variance -
      validation$one_unit_delay_scaled_variance_reproduced
  )
  validation$two_unit_delay_matrix_absolute_difference <- abs(
    validation$two_unit_delay_matrix_scaled_variance -
      validation$two_unit_delay_scaled_variance_reproduced
  )

  stored_difference_columns <- grep(
    "_absolute_difference$", names(validation), value = TRUE
  )
  matrix_difference_columns <- grep(
    "_matrix_absolute_difference$", names(validation), value = TRUE
  )
  stored_difference_columns <- setdiff(
    stored_difference_columns, matrix_difference_columns
  )

  validation$tolerance <- tolerance
  validation$stored_reference_pass <- apply(
    validation[stored_difference_columns],
    1L,
    function(x) all(is.finite(x) & x <= tolerance)
  )
  validation$matrix_check_applicable <- validation$icc < 1
  validation$matrix_inversion_pass <- vapply(
    seq_len(nrow(validation)),
    function(i) {
      if (!validation$matrix_check_applicable[i]) return(TRUE)
      differences <- unlist(
        validation[i, matrix_difference_columns, drop = FALSE],
        use.names = FALSE
      )
      all(is.finite(differences) & differences <= tolerance)
    },
    logical(1)
  )
  validation$pass <- validation$stored_reference_pass &
    validation$matrix_inversion_pass
  validation$artifact <- "Figure 5"
  validation$status <- "evaluated"
  validation <- validation[order(validation$icc), ]
  rownames(validation) <- NULL
  .sp_bench_stop_if_failed(validation, strict, "Figure 5")

  crossing <- 1 / sqrt(5)
  crossing_check <- data.frame(
    theoretical_crossing_icc = crossing,
    classic_scaled_variance =
      (1 - crossing) * (2 * crossing + 1) / (crossing + 1),
    one_unit_delay_scaled_variance = (1 + crossing) / 2,
    absolute_difference = abs(
      (1 - crossing) * (2 * crossing + 1) / (crossing + 1) -
        (1 + crossing) / 2
    ),
    pass = abs(
      (1 - crossing) * (2 * crossing + 1) / (crossing + 1) -
        (1 + crossing) / 2
    ) <= tolerance,
    stringsAsFactors = FALSE
  )
  if (isTRUE(strict) && !crossing_check$pass) {
    stop("Figure 5 crossing-point check failed.", call. = FALSE)
  }

  output_dir <- if (isTRUE(write_outputs)) {
    .sp_bench_make_output_dir(output_dir)
  } else {
    NULL
  }
  .sp_bench_write_csv(
    reproduced, "figure5_limited_cluster_values.csv", output_dir
  )
  .sp_bench_write_csv(
    validation, "figure5_limited_cluster_validation.csv", output_dir
  )
  .sp_bench_write_csv(
    matrix_reproduced,
    "figure5_matrix_inversion_values.csv",
    output_dir
  )
  .sp_bench_write_csv(
    crossing_check, "figure5_crossing_check.csv", output_dir
  )
  if (isTRUE(make_plot) && !is.null(output_dir)) {
    .sp_bench_plot_figure5(
      reproduced,
      file.path(output_dir, "figure5_limited_clusters.pdf")
    )
  }

  list(
    reference = reference,
    results = reproduced,
    matrix_results = matrix_reproduced,
    validation = validation,
    crossing_check = crossing_check
  )
}
