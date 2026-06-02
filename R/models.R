#' Fit a specialty-level testing-rate model
#'
#' Fits either a binomial GLM or a provider-random-intercept binomial GLMM for
#' an aggregated success/trial outcome.
#'
#' @param data A data frame containing counts and grouping variables.
#' @param successes Name of the success-count column.
#' @param trials Name of the trial-count column.
#' @param specialty_var Name of the specialty column.
#' @param provider_var Optional provider identifier column. If supplied,
#'   \code{random_intercept = TRUE} fits a random-intercept GLMM.
#' @param link Link function. Supported values are \code{"logit"} and
#'   \code{"identity"}.
#' @param random_intercept Logical; whether to include a provider random
#'   intercept.
#' @param nAGQ Number of adaptive Gauss-Hermite quadrature points for
#'   \code{lme4::glmer()}.
#'
#' @return A fitted \code{glm} or \code{merMod} object.
#' @export
fit_specialty_rate_model <- function(
  data,
  successes,
  trials,
  specialty_var = "specialty",
  provider_var = NULL,
  link = c("logit", "identity"),
  random_intercept = !is.null(provider_var),
  nAGQ = 10
) {
  link <- match.arg(link)
  required <- c(successes, trials, specialty_var)
  if (isTRUE(random_intercept)) {
    required <- c(required, provider_var)
  }
  .check_required_columns(data, required)

  dat <- data
  dat <- dat[!is.na(dat[[successes]]) & !is.na(dat[[trials]]), , drop = FALSE]
  dat <- dat[dat[[trials]] > 0, , drop = FALSE]

  if (nrow(dat) == 0) {
    stop("No non-missing observations with positive trials were available.", call. = FALSE)
  }

  dat[[specialty_var]] <- as.factor(dat[[specialty_var]])
  dat[[".success"]] <- dat[[successes]]
  dat[[".failure"]] <- dat[[trials]] - dat[[successes]]

  if (any(dat[[".success"]] < 0) || any(dat[[".failure"]] < 0)) {
    stop("Successes must be between 0 and trials for every row.", call. = FALSE)
  }

  family_obj <- stats::binomial(link = link)

  if (!isTRUE(random_intercept)) {
    formula_obj <- stats::as.formula(
      paste0("cbind(.success, .failure) ~ ", specialty_var)
    )
    return(stats::glm(formula_obj, family = family_obj, data = dat))
  }

  formula_obj <- stats::as.formula(
    paste0(
      "cbind(.success, .failure) ~ ",
      specialty_var,
      " + (1|",
      provider_var,
      ")"
    )
  )

  lme4::glmer(formula_obj, family = family_obj, data = dat, nAGQ = nAGQ)
}

#' Estimate specialty-specific probabilities from a fitted model
#'
#' Extracts specialty-level probabilities from a model produced by
#' \code{\link{fit_specialty_rate_model}}. For random-intercept logit models, a
#' simple approximation is used to convert fixed-effect conditional log-odds to
#' approximate marginal log-odds.
#'
#' @param model A fitted model returned by \code{\link{fit_specialty_rate_model}}.
#' @param specialty_levels Optional vector of specialty levels. By default the
#'   levels are recovered from the model frame.
#' @param specialty_var Name of the specialty column used in the model.
#' @param link Link function for the fitted model.
#' @param approximate_marginal Logical; whether to apply the standard logit
#'   approximation for random-intercept models.
#' @param logit_scale_factor Approximation constant used in the shrinkage factor.
#'
#' @return A data frame with specialty-level linear predictors and probabilities.
#' @export
estimate_specialty_rates <- function(
  model,
  specialty_levels = NULL,
  specialty_var = "specialty",
  link = c("logit", "identity"),
  approximate_marginal = TRUE,
  logit_scale_factor = 0.346
) {
  link <- match.arg(link)

  if (is.null(specialty_levels)) {
    mf <- stats::model.frame(model)
    specialty_levels <- levels(as.factor(mf[[specialty_var]]))
  }

  if (inherits(model, "merMod")) {
    coef_vals <- lme4::fixef(model)
  } else {
    coef_vals <- stats::coef(model)
  }

  intercept <- unname(coef_vals["(Intercept)"])
  if (is.na(intercept)) {
    stop("Model intercept could not be recovered.", call. = FALSE)
  }

  re_var <- if (inherits(model, "merMod")) .random_intercept_variance(model) else 0

  rows <- vector("list", length(specialty_levels))
  for (i in seq_along(specialty_levels)) {
    level_i <- specialty_levels[i]
    coef_name <- paste0(specialty_var, level_i)
    eta <- intercept
    if (coef_name %in% names(coef_vals)) {
      eta <- eta + unname(coef_vals[coef_name])
    }

    eta_out <- eta
    if (inherits(model, "merMod") &&
        identical(link, "logit") &&
        isTRUE(approximate_marginal)) {
      eta_out <- eta / sqrt(1 + logit_scale_factor * re_var)
    }

    rows[[i]] <- data.frame(
      specialty = level_i,
      linear_predictor = eta_out,
      probability = .inverse_link(eta_out, link),
      model_class = class(model)[1],
      link = link,
      random_effect_variance = re_var,
      stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Reproduce the core Lp(a) outcome analyses
#'
#' Fits the main outcome models from the original script for both overall Lp(a)
#' testing and Lp(a) testing among patients with elevated LDL.
#'
#' @param data A physician-level analysis data frame.
#' @param provider_var Provider identifier column.
#' @param specialty_var Specialty column.
#' @param outcomes Named list defining success/trial columns for each outcome.
#' @param links Character vector of links to fit.
#' @param nAGQ Number of quadrature points for \code{glmer()}.
#'
#' @return A nested list containing fitted models and specialty-rate tables.
#' @export
analyze_lpa_outcomes <- function(
  data,
  provider_var = "prov_id",
  specialty_var = "specialty",
  outcomes = list(
    overall = list(successes = "n_lpa_pat", trials = "n_total_pat"),
    high_ldl = list(successes = "n_ldl_lpa_pat", trials = "n_ldl_pat")
  ),
  links = c("logit", "identity"),
  nAGQ = 10
) {
  .check_required_columns(data, c(provider_var, specialty_var))

  results <- list()

  for (outcome_name in names(outcomes)) {
    outcome_def <- outcomes[[outcome_name]]
    out_dat <- data
    out_dat <- out_dat[!is.na(out_dat[[outcome_def$trials]]) &
                         out_dat[[outcome_def$trials]] > 0, , drop = FALSE]

    link_results <- list()
    for (link_name in links) {
      glm_fit <- fit_specialty_rate_model(
        data = out_dat,
        successes = outcome_def$successes,
        trials = outcome_def$trials,
        specialty_var = specialty_var,
        link = link_name,
        random_intercept = FALSE
      )

      glmer_fit <- fit_specialty_rate_model(
        data = out_dat,
        successes = outcome_def$successes,
        trials = outcome_def$trials,
        specialty_var = specialty_var,
        provider_var = provider_var,
        link = link_name,
        random_intercept = TRUE,
        nAGQ = nAGQ
      )

      link_results[[link_name]] <- list(
        glm = glm_fit,
        glm_rates = estimate_specialty_rates(
          glm_fit,
          specialty_var = specialty_var,
          link = link_name,
          approximate_marginal = FALSE
        ),
        glmer = glmer_fit,
        glmer_rates = estimate_specialty_rates(
          glmer_fit,
          specialty_var = specialty_var,
          link = link_name,
          approximate_marginal = TRUE
        )
      )
    }

    results[[outcome_name]] <- link_results
  }

  results
}
