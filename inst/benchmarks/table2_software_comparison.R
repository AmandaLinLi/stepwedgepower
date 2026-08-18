# Reproduce the software-comparison values in Lin et al. (2026), Table 2.
#
# PASS and the Shiny CRT Calculator are retained as manuscript reference values.
# swCRTdesign and swdpwr are rerun when those optional packages are installed.

.sp_bench_table2_scenarios <- function() {
  list(
    `1` = list(
      scenario = 1L,
      swcrt = function() {
        swCRTdesign::swPwr(
          swCRTdesign::swDsn(c(5, 5)),
          distn = "gaussian",
          n = 15,
          mu0 = 1,
          mu1 = 1.5,
          sigma = 1,
          tau = sqrt(1 / 99),
          eta = 0,
          rho = 0,
          gamma = 0,
          alpha = 0.05
        )
      },
      swdpwr_time = function() {
        design <- matrix(
          c(rep(c(0, 1, 1), 5), rep(c(0, 0, 1), 5)),
          nrow = 10, ncol = 3, byrow = TRUE
        )
        swdpwr::swdpower(
          K = 15,
          design = design,
          family = "gaussian",
          model = "marginal",
          link = "identity",
          type = "cross-sectional",
          meanresponse_start = 1,
          meanresponse_end0 = 1.5,
          effectsize_beta = 0.5,
          sigma2 = 1,
          typeIerror = 0.05,
          alpha0 = 0.01,
          alpha1 = 0.01
        )
      },
      swdpwr_no_time = function() {
        design <- matrix(
          c(rep(c(0, 1, 1), 5), rep(c(0, 0, 1), 5)),
          nrow = 10, ncol = 3, byrow = TRUE
        )
        swdpwr::swdpower(
          K = 15,
          design = design,
          family = "gaussian",
          model = "marginal",
          link = "identity",
          type = "cross-sectional",
          meanresponse_start = 1,
          meanresponse_end1 = 1.5,
          sigma2 = 1,
          typeIerror = 0.05,
          alpha0 = 0.01,
          alpha1 = 0.01
        )
      }
    ),
    `2` = list(
      scenario = 2L,
      swcrt = function() {
        swCRTdesign::swPwr(
          swCRTdesign::swDsn(c(5, 5)),
          distn = "binomial",
          n = 50,
          mu0 = 0.1,
          mu1 = 0.2,
          icc = 0.01,
          alpha = 0.05
        )
      },
      swdpwr_time = function() {
        design <- matrix(
          c(rep(c(0, 1, 1), 5), rep(c(0, 0, 1), 5)),
          nrow = 10, ncol = 3, byrow = TRUE
        )
        swdpwr::swdpower(
          K = 50,
          design = design,
          family = "binomial",
          model = "marginal",
          link = "identity",
          type = "cross-sectional",
          meanresponse_start = 0.1,
          meanresponse_end0 = 0.1001,
          meanresponse_end1 = 0.2,
          typeIerror = 0.05,
          alpha0 = 0.01,
          alpha1 = 0.01
        )
      },
      swdpwr_no_time = function() {
        design <- matrix(
          c(rep(c(0, 1, 1), 5), rep(c(0, 0, 1), 5)),
          nrow = 10, ncol = 3, byrow = TRUE
        )
        swdpwr::swdpower(
          K = 50,
          design = design,
          family = "binomial",
          model = "marginal",
          link = "identity",
          type = "cross-sectional",
          meanresponse_start = 0.1,
          meanresponse_end0 = 0.1,
          meanresponse_end1 = 0.2,
          typeIerror = 0.05,
          alpha0 = 0.01,
          alpha1 = 0.01
        )
      }
    ),
    `3` = list(
      scenario = 3L,
      swcrt = function() {
        swCRTdesign::swPwr(
          swCRTdesign::swDsn(c(5, 5, 5)),
          distn = "binomial",
          n = 50,
          mu0 = 0.05,
          mu1 = 0.1,
          icc = 0.01,
          alpha = 0.05
        )
      },
      swdpwr_time = function() {
        design <- matrix(
          c(
            rep(c(0, 1, 1, 1), 5),
            rep(c(0, 0, 1, 1), 5),
            rep(c(0, 0, 0, 1), 5)
          ),
          nrow = 15, ncol = 4, byrow = TRUE
        )
        swdpwr::swdpower(
          K = 50,
          design = design,
          family = "binomial",
          model = "marginal",
          link = "identity",
          type = "cross-sectional",
          meanresponse_start = 0.05,
          meanresponse_end0 = 0.0501,
          effectsize_beta = 0.05,
          typeIerror = 0.05,
          alpha0 = 0.01,
          alpha1 = 0.01
        )
      },
      swdpwr_no_time = function() {
        design <- matrix(
          c(
            rep(c(0, 1, 1, 1), 5),
            rep(c(0, 0, 1, 1), 5),
            rep(c(0, 0, 0, 1), 5)
          ),
          nrow = 15, ncol = 4, byrow = TRUE
        )
        swdpwr::swdpower(
          K = 50,
          design = design,
          family = "binomial",
          model = "marginal",
          link = "identity",
          type = "cross-sectional",
          meanresponse_start = 0.05,
          meanresponse_end0 = 0.05,
          meanresponse_end1 = 0.1,
          typeIerror = 0.05,
          alpha0 = 0.01,
          alpha1 = 0.01
        )
      }
    ),
    `4` = list(
      scenario = 4L,
      swcrt = function() {
        # Under the separate-time model with equal batches, the information
        # combines as the corresponding complete SWD.
        swCRTdesign::swPwr(
          swCRTdesign::swDsn(c(6, 6)),
          distn = "binomial",
          n = 50,
          mu0 = 0.1,
          mu1 = 0.2,
          icc = 0.01,
          alpha = 0.05
        )
      },
      swdpwr_time = function() {
        one_batch <- rbind(c(0, 1, 1), c(0, 0, 1))
        design <- do.call(
          rbind,
          replicate(6L, one_batch, simplify = FALSE)
        )
        swdpwr::swdpower(
          K = 50,
          design = design,
          family = "binomial",
          model = "marginal",
          link = "identity",
          type = "cross-sectional",
          meanresponse_start = 0.1,
          meanresponse_end0 = 0.1001,
          meanresponse_end1 = 0.2,
          typeIerror = 0.05,
          alpha0 = 0.01,
          alpha1 = 0.01
        )
      },
      swdpwr_no_time = function() {
        one_batch <- rbind(c(0, 1, 1), c(0, 0, 1))
        design <- do.call(
          rbind,
          replicate(6L, one_batch, simplify = FALSE)
        )
        swdpwr::swdpower(
          K = 50,
          design = design,
          family = "binomial",
          model = "marginal",
          link = "identity",
          type = "cross-sectional",
          meanresponse_start = 0.1,
          meanresponse_end0 = 0.1,
          meanresponse_end1 = 0.2,
          typeIerror = 0.05,
          alpha0 = 0.01,
          alpha1 = 0.01
        )
      }
    )
  )
}

.sp_bench_table2_one <- function(scenario, method, run_external) {
  package <- if (method == "swCRTdesign") "swCRTdesign" else "swdpwr"
  if (!isTRUE(run_external)) {
    return(data.frame(
      scenario = scenario$scenario,
      method = method,
      reproduced_power = NA_real_,
      status = "skipped",
      package_version = .sp_bench_package_version(package),
      warning_message = NA_character_,
      error_message = "External benchmarks disabled.",
      stringsAsFactors = FALSE
    ))
  }
  if (!requireNamespace(package, quietly = TRUE)) {
    return(data.frame(
      scenario = scenario$scenario,
      method = method,
      reproduced_power = NA_real_,
      status = "skipped",
      package_version = NA_character_,
      warning_message = NA_character_,
      error_message = paste0("Optional package '", package, "' is not installed."),
      stringsAsFactors = FALSE
    ))
  }

  runner <- switch(
    method,
    swCRTdesign = scenario$swcrt,
    swdpwr_with_time = scenario$swdpwr_time,
    swdpwr_without_time = scenario$swdpwr_no_time,
    stop("Unknown Table 2 method.", call. = FALSE)
  )
  captured <- .sp_bench_capture(runner())
  power <- .sp_bench_extract_power(captured$value)
  status <- if (!is.na(captured$error)) {
    "error"
  } else if (!is.finite(power)) {
    "nonfinite"
  } else {
    "evaluated"
  }

  data.frame(
    scenario = scenario$scenario,
    method = method,
    reproduced_power = power,
    status = status,
    package_version = .sp_bench_package_version(package),
    warning_message = if (length(captured$warnings)) {
      paste(captured$warnings, collapse = " | ")
    } else {
      NA_character_
    },
    error_message = captured$error,
    stringsAsFactors = FALSE
  )
}

run_table2_benchmark <- function(
  output_dir = NULL,
  benchmark_dir = .sp_bench_default_dir(),
  run_external = TRUE,
  strict = FALSE,
  tolerance = 0.002,
  write_outputs = TRUE
) {
  reference <- .sp_bench_read_reference(
    "table2_reference.csv", benchmark_dir
  )
  reference$reference_power <- as.numeric(reference$reference_power)

  scenarios <- .sp_bench_table2_scenarios()
  reproducible_methods <- c(
    "swCRTdesign", "swdpwr_with_time", "swdpwr_without_time"
  )
  reproduced <- do.call(rbind, lapply(scenarios, function(scenario) {
    do.call(rbind, lapply(reproducible_methods, function(method) {
      .sp_bench_table2_one(scenario, method, run_external)
    }))
  }))
  rownames(reproduced) <- NULL

  reference_only <- reference[
    reference$method %in% c("PASS", "Shiny CRT Calculator"),
    c("scenario", "method")
  ]
  reference_only$reproduced_power <- NA_real_
  reference_only$status <- "reference_only"
  reference_only$package_version <- NA_character_
  reference_only$warning_message <- NA_character_
  reference_only$error_message <- NA_character_

  observed <- rbind(reproduced, reference_only)
  validation <- .sp_bench_validation_rows(
    artifact = "Table 2",
    observed = observed,
    reference = reference,
    keys = c("scenario", "method"),
    tolerance = tolerance,
    rounded_digits = 3L
  )
  validation <- validation[order(validation$scenario, validation$method), ]
  rownames(validation) <- NULL
  .sp_bench_stop_if_failed(validation, strict, "Table 2")

  output_dir <- if (isTRUE(write_outputs)) {
    .sp_bench_make_output_dir(output_dir)
  } else {
    NULL
  }
  .sp_bench_write_csv(
    validation, "table2_software_comparison.csv", output_dir
  )

  list(
    reference = reference,
    results = observed,
    validation = validation,
    package_versions = data.frame(
      package = c("swCRTdesign", "swdpwr"),
      version = c(
        .sp_bench_package_version("swCRTdesign"),
        .sp_bench_package_version("swdpwr")
      ),
      stringsAsFactors = FALSE
    )
  )
}
