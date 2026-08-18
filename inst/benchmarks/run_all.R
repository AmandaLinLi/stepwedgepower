# Run the complete stepwedgepower benchmark and validation suite.
#
# Usage from an installed package:
#   benchmark_dir <- system.file("benchmarks", package = "stepwedgepower")
#   source(file.path(benchmark_dir, "run_all.R"))
#
# Usage from a source checkout:
#   Rscript inst/benchmarks/run_all.R
#
# Environment variables:
#   STEPWEDGEPOWER_BENCHMARK_DIR
#   STEPWEDGEPOWER_BENCHMARK_OUTPUT
#   STEPWEDGEPOWER_RUN_EXTERNAL       (true/false; default true)
#   STEPWEDGEPOWER_BENCHMARK_STRICT   (true/false; default false)

local({
  script_dir <- function() {
    configured <- Sys.getenv("STEPWEDGEPOWER_BENCHMARK_DIR", unset = "")
    if (nzchar(configured)) {
      return(normalizePath(configured, mustWork = TRUE))
    }

    command <- commandArgs(trailingOnly = FALSE)
    file_argument <- grep("^--file=", command, value = TRUE)
    if (length(file_argument)) {
      return(dirname(normalizePath(
        sub("^--file=", "", file_argument[1L]),
        mustWork = TRUE
      )))
    }

    installed <- system.file("benchmarks", package = "stepwedgepower")
    if (nzchar(installed)) {
      return(normalizePath(installed, mustWork = TRUE))
    }

    candidates <- c(
      file.path(getwd(), "inst", "benchmarks"),
      getwd()
    )
    existing <- candidates[dir.exists(candidates)]
    if (!length(existing)) {
      stop("Could not locate benchmark scripts.", call. = FALSE)
    }
    normalizePath(existing[1L], mustWork = TRUE)
  }

  env_flag <- function(name, default = FALSE) {
    value <- tolower(trimws(Sys.getenv(
      name,
      unset = if (default) "true" else "false"
    )))
    value %in% c("1", "true", "yes", "y", "on")
  }

  benchmark_dir <- script_dir()
  benchmark_env <- new.env(parent = globalenv())
  old_options <- options(stepwedgepower.benchmark_dir = benchmark_dir)
  on.exit(options(old_options), add = TRUE)

  for (file in c(
    "benchmark_helpers.R",
    "table2_software_comparison.R",
    "figure3_calendar_delay.R",
    "table3_separate_time_grid.R",
    "figure5_limited_clusters.R"
  )) {
    source(
      file.path(benchmark_dir, file),
      local = benchmark_env,
      chdir = FALSE
    )
  }

  output_dir <- Sys.getenv(
    "STEPWEDGEPOWER_BENCHMARK_OUTPUT",
    unset = file.path(getwd(), "stepwedgepower-benchmark-results")
  )
  run_external <- env_flag(
    "STEPWEDGEPOWER_RUN_EXTERNAL", default = TRUE
  )
  strict <- env_flag(
    "STEPWEDGEPOWER_BENCHMARK_STRICT", default = FALSE
  )
  output_dir <- benchmark_env$.sp_bench_make_output_dir(output_dir)

  table2 <- benchmark_env$run_table2_benchmark(
    output_dir = output_dir,
    benchmark_dir = benchmark_dir,
    run_external = run_external,
    strict = strict
  )
  figure3 <- benchmark_env$run_figure3_benchmark(
    output_dir = output_dir,
    benchmark_dir = benchmark_dir,
    strict = strict
  )
  table3 <- benchmark_env$run_table3_benchmark(
    output_dir = output_dir,
    benchmark_dir = benchmark_dir,
    run_external = run_external,
    strict = strict
  )
  figure5 <- benchmark_env$run_figure5_benchmark(
    output_dir = output_dir,
    benchmark_dir = benchmark_dir,
    strict = strict
  )

  summary_table <- benchmark_env$.sp_bench_status_summary(
    table2, figure3, table3, figure5
  )
  utils::write.csv(
    summary_table,
    file.path(output_dir, "benchmark_summary.csv"),
    row.names = FALSE
  )

  metadata <- data.frame(
    item = c(
      "stepwedgepower_version",
      "R_version",
      "platform",
      "run_external",
      "strict",
      "benchmark_directory",
      "output_directory",
      "timestamp_utc"
    ),
    value = c(
      if (requireNamespace("stepwedgepower", quietly = TRUE)) {
        as.character(utils::packageVersion("stepwedgepower"))
      } else {
        "source checkout"
      },
      R.version.string,
      R.version$platform,
      as.character(run_external),
      as.character(strict),
      benchmark_dir,
      output_dir,
      format(Sys.time(), tz = "UTC", usetz = TRUE)
    ),
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    metadata,
    file.path(output_dir, "benchmark_metadata.csv"),
    row.names = FALSE
  )

  capture.output(
    utils::sessionInfo(),
    file = file.path(output_dir, "sessionInfo.txt")
  )
  saveRDS(
    list(
      table2 = table2,
      figure3 = figure3,
      table3 = table3,
      figure5 = figure5,
      summary = summary_table,
      metadata = metadata
    ),
    file = file.path(output_dir, "benchmark_results.rds")
  )

  cat("stepwedgepower benchmark suite completed.\n")
  cat("Results:", output_dir, "\n\n")
  print(summary_table, row.names = FALSE)
})
