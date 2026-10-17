test_that("power_grid runs the full cartesian product", {
  d <- sw_design(clusters_per_sequence = c(4, 4, 4, 4),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.05,
                      n_per_cluster_period = 20)
  g <- power_grid(d, a, vary = list(treatment_or = c(1.5, 2.5),
                                    icc = c(0.02, 0.10)),
                  nsim = 5, seed = 1)
  expect_s3_class(g, "sw_power_grid")
  expect_equal(nrow(g), 4)
  expect_true(all(c("power", "mcse", "conf_low", "conf_high") %in% names(g)))
  expect_length(attr(g, "runs"), 4)
})

test_that("power_grid can vary the number of clusters", {
  d <- sw_design(clusters_per_sequence = c(4, 4, 4, 4),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.05,
                      n_per_cluster_period = 20)
  g <- power_grid(
    d, a,
    vary = list(clusters_per_sequence = list(c(3, 3, 3, 3), c(6, 6, 6, 6))),
    nsim = 5, seed = 1
  )
  expect_equal(nrow(g), 2)
  runs <- attr(g, "runs")
  expect_equal(runs[[1]]$design$n_clusters, 12)
  expect_equal(runs[[2]]$design$n_clusters, 24)
})

test_that("power_grid rejects unknown parameters", {
  d <- sw_design(clusters_per_sequence = c(4, 4), crossover_period = c(2, 3),
                 n_periods = 3)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.05)
  expect_error(power_grid(d, a, vary = list(nonsense = 1:2), nsim = 2),
               "unknown parameter")
})

test_that("replicate RNG is reproducible regardless of core count", {
  available <- suppressWarnings(parallel::detectCores(logical = FALSE))
  skip_if(is.na(available) || available < 2L)

  draw <- function(i) stats::rnorm(3L)
  serial <- stepwedgepower:::.simulate_replicates(
    draw, nsim = 6L, n_cores = 1L, seed = 42
  )
  parallel_run <- stepwedgepower:::.simulate_replicates(
    draw, nsim = 6L, n_cores = 2L, seed = 42
  )
  expect_equal(serial, parallel_run, tolerance = 0)
})

test_that("simulation helpers restore the caller RNG state", {
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)

  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(123)
  
  before <- .Random.seed
  before_kind <- RNGkind()
  invisible(stepwedgepower:::.simulate_replicates(
    function(i) stats::runif(1L), nsim = 4L, n_cores = 1L, seed = 99
  ))
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind(), before_kind)
})

test_that("core resolution handles unavailable detection and CRAN limits", {
  old <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA_character_)
  on.exit({
    if (is.na(old)) Sys.unsetenv("_R_CHECK_LIMIT_CORES_") else
      Sys.setenv(`_R_CHECK_LIMIT_CORES_` = old)
  }, add = TRUE)

  Sys.setenv(`_R_CHECK_LIMIT_CORES_` = "TRUE")
  expect_lte(stepwedgepower:::.resolve_n_cores(8L, 20L), 2L)
  expect_equal(stepwedgepower:::.resolve_n_cores(8L, 1L), 1L)
})

test_that("the same seed gives the same answer twice", {
  d <- sw_design(clusters_per_sequence = c(4, 4, 4, 4),
                 crossover_period = c(2, 3, 4, 5), n_periods = 5)
  a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.05,
                      n_per_cluster_period = 20)
  expect_equal(
    power_swcrt(d, a, nsim = 5, seed = 7)$p_values,
    power_swcrt(d, a, nsim = 5, seed = 7)$p_values
  )
})
