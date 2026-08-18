test_that("benchmark module is installed without exporting new API", {
  benchmark_dir <- system.file("benchmarks", package = "stepwedgepower")
  expect_true(nzchar(benchmark_dir))
  expect_true(file.exists(file.path(
    benchmark_dir, "benchmark_helpers.R"
  )))
  expect_true(file.exists(file.path(
    benchmark_dir, "reference", "table2_reference.csv"
  )))
  expect_true(file.exists(file.path(
    benchmark_dir, "reference", "table3_reference.csv"
  )))
  expect_false(any(grepl(
    "benchmark",
    getNamespaceExports("stepwedgepower"),
    ignore.case = TRUE
  )))
})

test_that("Figure 3 analytical benchmark reproduces stored references", {
  benchmark_dir <- system.file("benchmarks", package = "stepwedgepower")
  env <- new.env(parent = globalenv())
  old_options <- options(stepwedgepower.benchmark_dir = benchmark_dir)
  on.exit(options(old_options), add = TRUE)
  source(file.path(benchmark_dir, "benchmark_helpers.R"), local = env)
  source(file.path(benchmark_dir, "figure3_calendar_delay.R"), local = env)

  result <- env$run_figure3_benchmark(
    benchmark_dir = benchmark_dir,
    strict = TRUE,
    write_outputs = FALSE,
    make_plot = FALSE
  )
  expect_true(all(result$validation$pass))
  expect_true(all(result$endpoint_check$pass))
})

test_that("Figure 5 closed-form benchmark reproduces stored references", {
  benchmark_dir <- system.file("benchmarks", package = "stepwedgepower")
  env <- new.env(parent = globalenv())
  old_options <- options(stepwedgepower.benchmark_dir = benchmark_dir)
  on.exit(options(old_options), add = TRUE)
  source(file.path(benchmark_dir, "benchmark_helpers.R"), local = env)
  source(file.path(benchmark_dir, "figure5_limited_clusters.R"), local = env)

  result <- env$run_figure5_benchmark(
    benchmark_dir = benchmark_dir,
    strict = TRUE,
    write_outputs = FALSE,
    make_plot = FALSE
  )
  expect_true(all(result$validation$pass))
  expect_true(all(
    result$validation$matrix_inversion_pass
  ))
  expect_true(result$crossing_check$pass)
})

test_that("Table 2 and Table 3 references load without optional packages", {
  benchmark_dir <- system.file("benchmarks", package = "stepwedgepower")
  env <- new.env(parent = globalenv())
  old_options <- options(stepwedgepower.benchmark_dir = benchmark_dir)
  on.exit(options(old_options), add = TRUE)
  source(file.path(benchmark_dir, "benchmark_helpers.R"), local = env)
  source(
    file.path(benchmark_dir, "table2_software_comparison.R"),
    local = env
  )
  source(
    file.path(benchmark_dir, "table3_separate_time_grid.R"),
    local = env
  )

  table2 <- env$run_table2_benchmark(
    benchmark_dir = benchmark_dir,
    run_external = FALSE,
    strict = FALSE,
    write_outputs = FALSE
  )
  table3 <- env$run_table3_benchmark(
    benchmark_dir = benchmark_dir,
    run_external = FALSE,
    strict = FALSE,
    write_outputs = FALSE
  )

  expect_equal(nrow(table2$reference), 20L)
  expect_equal(nrow(table3$reference), 24L)
  expect_true(all(table2$results$status %in% c(
    "skipped", "reference_only"
  )))
  expect_true(all(table3$results$status == "skipped"))
})
