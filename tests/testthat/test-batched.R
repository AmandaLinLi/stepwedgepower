test_that("batched design records delayed initiation and time on trial", {
  design <- sw_batched_design(
    c(2L, 2L, 3L, 3L),
    rbind(
      G1 = c("0", "1", "1+2", "1+2", "-"),
      G2 = c("0", "0", "1", "1+2", "-"),
      G3 = c("-", "0", "1", "1+2", "1+2"),
      G4 = c("-", "0", "0", "1", "1+2")
    ),
    batch = c("Batch 1", "Batch 1", "Batch 2", "Batch 2")
  )
  expect_s3_class(design, "sw_batched_design")
  expect_equal(design$n_batches, 2L)
  expect_equal(unname(design$batch_start_period), c(1L, 2L))
  expect_equal(unname(design$batch_delay), c(0L, 1L))
  expect_equal(unname(design$batch_gap_from_previous), c(0L, 1L))
  expect_equal(design$n_observed_cluster_periods, 40L)
  expect_equal(design$n_missing_cluster_periods, 10L)
  expect_equal(design$time_on_trial[1, ], 1:5)
  expect_equal(design$time_on_trial[3, ], c(NA, 1:4))
})

test_that("batch labels can be inferred from first observed period", {
  design <- sw_batched_design(
    c(1L, 1L, 1L),
    rbind(
      S1 = c("0", "1", "1"),
      S2 = c("-", "0", "1"),
      S3 = c("-", "0", "0")
    )
  )
  expect_true(design$batch_inferred)
  expect_equal(design$batch, c("Batch 1", "Batch 2", "Batch 2"))
  expect_equal(unname(design$batch_start_period), c(1L, 2L))
})

test_that("sequences in one batch must share initiation", {
  expect_error(
    sw_batched_design(
      c(1L, 1L),
      rbind(c("0", "1"), c("-", "0")),
      batch = c("Batch 1", "Batch 1")
    ),
    "same first observed"
  )
})

test_that("batched assumptions support all three time models", {
  calendar <- sw_batched_assumptions(
    baseline_prob = 0.1, treatment_or_a = 1.2,
    icc = 0.05, time_model = "calendar",
    time_effects = c(0, 0.1), n_per_cluster_period = 10
  )
  trial_time <- sw_batched_assumptions(
    baseline_prob = 0.1, treatment_or_a = 1.2,
    icc = 0.05, time_model = "time_on_trial",
    time_effects = c(0, 0.1), n_per_cluster_period = 10
  )
  separate <- sw_batched_assumptions(
    baseline_prob = 0.1, treatment_or_a = 1.2,
    icc = 0.05, time_model = "separate",
    time_effects = list(c(0, 0.1), c(0, -0.1)),
    n_per_cluster_period = 10
  )
  expect_equal(calendar$time_model, "calendar")
  expect_equal(trial_time$time_model, "time_on_trial")
  expect_equal(separate$time_model, "separate")
})

test_that("simulation adds batch and relative-time columns", {
  design <- sw_batched_design(
    c(1L, 1L),
    rbind(B1 = c("0", "1", "1"), B2 = c("-", "0", "1")),
    batch = c("Batch 1", "Batch 2")
  )
  assumptions <- sw_batched_assumptions(
    baseline_prob = 0.1, treatment_or_a = 1.2,
    cluster_sd = 0, time_model = "time_on_trial",
    time_effects = c(0, 0.2), n_per_cluster_period = 10
  )
  trial <- simulate_batched_swcrt(design, assumptions, seed = 1)
  expect_true(all(c(
    "batch", "calendar_period", "time_on_trial", "true_time_effect"
  ) %in% names(trial)))
  expect_equal(trial$time_on_trial[trial$sequence == "B2"], c(1L, 2L))
  expect_equal(trial$true_time_effect[trial$sequence == "B2"], c(0, 0.2))
})

test_that("separate profiles cover observed local periods only", {
  design <- sw_batched_design(
    c(1L, 1L),
    rbind(B1 = c("0", "1", "1", "-"), B2 = c("-", "0", "1", "1")),
    batch = c("Batch 1", "Batch 2")
  )
  assumptions <- sw_batched_assumptions(
    baseline_prob = 0.1, treatment_or_a = 1.2,
    cluster_sd = 0, time_model = "separate",
    time_effects = list(
      `Batch 1` = c(0, 0.1, 0.2),
      `Batch 2` = c(0, -0.1, -0.2)
    ),
    n_per_cluster_period = 10
  )
  expect_silent(simulate_batched_swcrt(design, assumptions, seed = 1))
})

test_that("batched audit uses requested time parameterization", {
  design <- sw_batched_design(
    c(2L, 2L, 2L, 2L),
    rbind(
      G1 = c("0", "1", "1", "-"),
      G2 = c("0", "0", "1", "-"),
      G3 = c("-", "0", "1", "1"),
      G4 = c("-", "0", "0", "1")
    ),
    batch = c(1, 1, 2, 2)
  )
  assumptions <- sw_batched_assumptions(
    baseline_prob = 0.1, treatment_or_a = 1.3,
    cluster_sd = 0.2, time_model = "calendar",
    n_per_cluster_period = 10
  )
  audit <- audit_batched_design(
    design, assumptions, "separate", "A_vs_control",
    include_interaction = FALSE
  )
  expect_equal(audit$time_model, "separate")
  expect_match(
    paste(deparse(audit$formula), collapse = " "),
    "factor\\(batch_period\\)"
  )
})

test_that("B versus A is a standard component contrast", {
  matrix <- stepwedgepower:::.component_contrast_matrix(TRUE)
  expect_equal(unname(matrix["B_vs_A", ]), c(-1, 1, 0))
  expect_equal(
    stepwedgepower:::.component_contrast_labels()[["B_vs_A"]],
    "B vs A"
  )
})
