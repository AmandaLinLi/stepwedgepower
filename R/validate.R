#' Validate the simulation engine against an independent reference
#'
#' Cross-checks [power_swcrt()] against a reference implementation that shares
#' none of its code: the reference draws the stepped-wedge data directly from
#' the generating model and fits the analysis model with a hand-written call, so
#' agreement is evidence that the engine's design construction, data generation,
#' and testing are all wired up correctly.
#'
#' This is a self-contained check. It deliberately does *not* treat any external
#' power package as ground truth. Closed-form stepped-wedge power formulae rest
#' on asymptotic approximations that can be badly optimistic when the number of
#' clusters is small, which is precisely the regime most stepped-wedge trials
#' operate in; see [compare_with_swdpwr()] for an informational comparison and a
#' worked demonstration of that gap.
#'
#' @param design An [sw_design()] object.
#' @param assumptions An [sw_assumptions()] object.
#' @param nsim Number of replicates for both the engine and the reference.
#' @param alpha Two-sided significance level.
#' @param seed Optional seed. The engine and the reference are run from the same
#'   seed but are otherwise independent implementations.
#' @param tolerance Maximum acceptable absolute difference in power beyond
#'   combined Monte Carlo error, used to set `agrees`.
#'
#' @return A one-row data frame comparing the engine's power with the
#'   reference's, including both Monte Carlo intervals and an `agrees` flag.
#' @examples
#' \donttest{
#' design <- sw_design(clusters_per_sequence = c(5, 5, 5, 5),
#'                     crossover_period = c(2, 3, 4, 5), n_periods = 5)
#' a <- sw_assumptions(baseline_prob = 0.15, treatment_or = 1.6, icc = 0.05,
#'                     n_per_cluster_period = 40)
#' validate_engine(design, a, nsim = 200, seed = 1)
#' }
#' @export
validate_engine <- function(
  design,
  assumptions,
  nsim = 500,
  alpha = 0.05,
  seed = NULL,
  tolerance = 0.05
) {
  if (!inherits(design, "sw_design")) {
    stop("`design` must be an sw_design object.", call. = FALSE)
  }
  if (!inherits(assumptions, "sw_assumptions")) {
    stop("`assumptions` must be an sw_assumptions object.", call. = FALSE)
  }

  engine <- power_swcrt(design, assumptions, nsim = nsim, alpha = alpha,
                        seed = seed)
  reference <- .reference_power(design, assumptions, nsim = nsim, alpha = alpha,
                                seed = seed)

  diff <- engine$power - reference$power
  combined_mcse <- sqrt(engine$mcse^2 + reference$mcse^2)
  agrees <- !is.na(diff) &&
    abs(diff) <= tolerance + 2 * combined_mcse

  data.frame(
    engine_power = engine$power,
    engine_low = engine$conf.int[1],
    engine_high = engine$conf.int[2],
    reference_power = reference$power,
    reference_low = reference$conf_low,
    reference_high = reference$conf_high,
    difference = diff,
    combined_mcse = combined_mcse,
    agrees = agrees,
    nsim = nsim,
    stringsAsFactors = FALSE
  )
}

# Independent reference implementation.
#
# Written deliberately without reusing simulate_swcrt() or fit_stepwedge_model():
# it expands the design by hand, draws the outcome directly, and calls glmer()
# with a literal formula. If this and the engine agree, the agreement is not an
# artifact of shared code.
.reference_power <- function(design, assumptions, nsim, alpha, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  n_seq <- design$n_sequences
  n_per <- design$n_periods
  trt <- design$treatment
  dimnames(trt) <- NULL

  cluster_sequence <- rep(seq_len(n_seq), times = design$clusters_per_sequence)
  n_clusters <- length(cluster_sequence)

  baseline <- .expand_baseline_logit(assumptions$baseline_logit, n_seq, n_per)
  gamma <- .expand_period_effects(assumptions$period_effects, n_per)
  beta <- assumptions$treatment_effect
  sd_c <- assumptions$cluster_sd
  size_spec <- assumptions$n_per_cluster_period

  grid <- expand.grid(cluster = seq_len(n_clusters), period = seq_len(n_per))
  grid$sequence <- cluster_sequence[grid$cluster]
  grid$trt <- trt[cbind(grid$sequence, grid$period)]

  reject <- rep(NA, nsim)
  for (r in seq_len(nsim)) {
    b <- stats::rnorm(n_clusters, 0, sd_c)
    size <- .resolve_sample_size(size_spec, grid$sequence, grid$period,
                                 n_seq, n_per)
    eta <- baseline[cbind(grid$sequence, grid$period)] +
      beta * grid$trt + gamma[grid$period] + b[grid$cluster]
    y <- stats::rbinom(nrow(grid), size, stats::plogis(eta))

    dat <- data.frame(y = y, size = size, trt = grid$trt,
                      period = grid$period, cluster = grid$cluster)
    fit <- tryCatch(
      suppressWarnings(suppressMessages(
        lme4::glmer(cbind(y, size - y) ~ trt + factor(period) + (1 | cluster),
                    family = stats::binomial(), data = dat, nAGQ = 1)
      )),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      cf <- summary(fit)$coefficients
      if ("trt" %in% rownames(cf)) {
        reject[r] <- cf["trt", "Pr(>|z|)"] < alpha
      }
    }
  }

  ok <- !is.na(reject)
  n_ok <- sum(ok)
  n_rej <- sum(reject[ok])
  power <- if (n_ok > 0) n_rej / n_ok else NA_real_
  mcse <- if (n_ok > 0) sqrt(power * (1 - power) / n_ok) else NA_real_
  ci <- if (n_ok > 0) {
    stats::binom.test(n_rej, n_ok, conf.level = 1 - alpha)$conf.int
  } else {
    c(NA_real_, NA_real_)
  }

  list(power = power, mcse = mcse,
       conf_low = unname(ci[1]), conf_high = unname(ci[2]),
       n_successful = n_ok)
}

#' Informational comparison with a closed-form power package
#'
#' Runs [power_swcrt()] alongside the closed-form calculation in the
#' \pkg{swdpwr} package, for the complete designs that both can express.
#'
#' The result is offered for information, not as a validation target, and the
#' two numbers can differ substantially. The closed-form calculation is
#' asymptotic in the number of clusters; with the small cluster counts typical
#' of stepped-wedge trials it can report power well above what a simulated trial
#' -- or an independent hand-written reference, see [validate_engine()] --
#' actually achieves. When the two disagree, prefer the simulated value, and
#' treat the closed-form number as an optimistic bound rather than a target.
#'
#' \pkg{swdpwr} is listed under `Suggests` and is not required to use this
#' package.
#'
#' @references
#' Chen J, Zhou X, Li F, Spiegelman D (2022). "swdpwr: A SAS macro and an R
#' package for power calculations in stepped wedge cluster randomized trials."
#' \emph{Computer Methods and Programs in Biomedicine}, 213, 106522.
#' \doi{10.1016/j.cmpb.2021.106522}
#'
#' Zhou X, Liao X, Kunz LM, Normand SLT, Wang M, Spiegelman D (2020). "A
#' maximum likelihood approach to power calculations for stepped wedge designs
#' of binary outcomes." \emph{Biostatistics}, 21(1), 102-121.
#' \doi{10.1093/biostatistics/kxy031}
#'
#' @param clusters_per_sequence Integer vector; must be constant across
#'   sequences for this comparison.
#' @param n_per_cluster_period Cluster-period size (a single value).
#' @param baseline_prob Common baseline probability under control.
#' @param treatment_or Treatment odds ratio.
#' @param icc Latent-scale intraclass correlation.
#' @param alpha Two-sided significance level.
#' @param nsim Number of simulations.
#' @param seed Optional seed.
#' @param n_cores Cores for the simulation.
#' @param adjust_sequence Whether the fitted model includes a fixed sequence
#'   term. Defaults to `FALSE`, which matches the model the closed-form
#'   calculation assumes.
#'
#' @return A one-row data frame with the simulated power and its Monte Carlo
#'   interval, the closed-form power, and their difference.
#' @examples
#' \donttest{
#' if (requireNamespace("swdpwr", quietly = TRUE)) {
#'   compare_with_swdpwr(
#'     clusters_per_sequence = c(5, 5, 5, 5),
#'     n_per_cluster_period = 40,
#'     baseline_prob = 0.15, treatment_or = 1.3, icc = 0.05,
#'     nsim = 200, seed = 1
#'   )
#' }
#' }
#' @export
compare_with_swdpwr <- function(
  clusters_per_sequence,
  n_per_cluster_period,
  baseline_prob,
  treatment_or,
  icc,
  alpha = 0.05,
  nsim = 500,
  seed = NULL,
  n_cores = 1,
  adjust_sequence = FALSE
) {
  if (!requireNamespace("swdpwr", quietly = TRUE)) {
    stop("Package 'swdpwr' is required for this comparison. ",
         "Install it, or use power_swcrt() directly.", call. = FALSE)
  }
  if (length(unique(clusters_per_sequence)) != 1L) {
    stop("This comparison requires an equal number of clusters per sequence.",
         call. = FALSE)
  }
  if (length(n_per_cluster_period) != 1L || length(baseline_prob) != 1L) {
    stop("`n_per_cluster_period` and `baseline_prob` must be single values.",
         call. = FALSE)
  }

  n_sequences <- length(clusters_per_sequence)
  n_periods <- n_sequences + 1L

  design <- sw_design(
    clusters_per_sequence = clusters_per_sequence,
    crossover_period = seq_len(n_sequences) + 1L,
    n_periods = n_periods
  )
  assumptions <- sw_assumptions(
    baseline_prob = baseline_prob, treatment_or = treatment_or,
    icc = icc, n_per_cluster_period = n_per_cluster_period
  )
  sim <- power_swcrt(
    design, assumptions, nsim = nsim, alpha = alpha,
    n_cores = n_cores, seed = seed,
    analysis_args = list(adjust_sequence = adjust_sequence)
  )

  # swdpwr's design matrix has one row per cluster, not per sequence.
  cluster_design <- design$treatment[
    rep(seq_len(n_sequences), times = clusters_per_sequence), , drop = FALSE
  ]
  dimnames(cluster_design) <- NULL

  odds1 <- (baseline_prob / (1 - baseline_prob)) * treatment_or
  p1 <- odds1 / (1 + odds1)

  closed <- suppressWarnings(swdpwr::swdpower(
    K = n_per_cluster_period,
    design = cluster_design,
    family = "binomial", model = "conditional", link = "logit",
    type = "cross-sectional",
    meanresponse_start = baseline_prob,
    meanresponse_end0 = baseline_prob,
    meanresponse_end1 = p1,
    typeIerror = alpha,
    # The conditional binary model requires alpha0 == alpha1.
    alpha0 = icc, alpha1 = icc
  ))
  closed_power <- .extract_swdpwr_power(closed)

  data.frame(
    simulated_power = sim$power,
    conf_low = sim$conf.int[1],
    conf_high = sim$conf.int[2],
    closed_form_power = closed_power,
    difference = sim$power - closed_power,
    nsim = nsim,
    stringsAsFactors = FALSE
  )
}

.extract_swdpwr_power <- function(x) {
  if (is.list(x)) {
    for (nm in c("Power", "power")) {
      if (!is.null(x[[nm]])) return(as.numeric(x[[nm]])[1])
    }
  }
  if (is.numeric(x) && length(x) >= 1L) return(as.numeric(x)[1])
  NA_real_
}
