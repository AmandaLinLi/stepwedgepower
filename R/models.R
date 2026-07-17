#' Fit a grouped rate model
#'
#' Fits either a binomial GLM or a cluster-random-intercept binomial GLMM for an
#' aggregated success/trial outcome, grouped by an arbitrary grouping variable.
#' This is the generic form of the former specialty-rate model: `group` may be a
#' specialty, region, arm, school, or any categorical grouping.
#'
#' @param data A data frame containing counts and a grouping variable.
#' @param successes Name of the success-count column.
#' @param trials Name of the trial-count column.
#' @param group Name of the grouping column.
#' @param cluster Optional cluster identifier column. Required when
#'   `random_intercept = TRUE`.
#' @param link Link function, `"logit"` or `"identity"`.
#' @param random_intercept Logical; include a cluster random intercept.
#' @param nAGQ Quadrature points for [lme4::glmer()].
#' @return A fitted `glm` or `merMod` object.
#' @examples
#' dat <- read_example_physician_data()
#' fit_grouped_rate_model(dat, successes = "n_lpa_pat", trials = "n_total_pat",
#'                        group = "specialty")
#' @export
fit_grouped_rate_model <- function(
  data,
  successes,
  trials,
  group = "group",
  cluster = NULL,
  link = c("logit", "identity"),
  random_intercept = !is.null(cluster),
  nAGQ = 10
) {
  link <- match.arg(link)
  required <- c(successes, trials, group)
  if (isTRUE(random_intercept)) {
    if (is.null(cluster)) {
      stop("`cluster` must be supplied when `random_intercept = TRUE`.",
           call. = FALSE)
    }
    required <- c(required, cluster)
  }
  .check_required_columns(data, required)

  dat <- data[!is.na(data[[successes]]) & !is.na(data[[trials]]), , drop = FALSE]
  dat <- dat[dat[[trials]] > 0, , drop = FALSE]
  if (nrow(dat) == 0) {
    stop("No non-missing observations with positive trials were available.",
         call. = FALSE)
  }

  dat[[group]] <- as.factor(dat[[group]])
  dat[[".success"]] <- dat[[successes]]
  dat[[".failure"]] <- dat[[trials]] - dat[[successes]]
  if (any(dat[[".success"]] < 0) || any(dat[[".failure"]] < 0)) {
    stop("Successes must lie between 0 and trials for every row.", call. = FALSE)
  }

  family_obj <- stats::binomial(link = link)

  if (!isTRUE(random_intercept)) {
    formula_obj <- stats::as.formula(
      paste0("cbind(.success, .failure) ~ ", group)
    )
    return(stats::glm(formula_obj, family = family_obj, data = dat))
  }

  formula_obj <- stats::as.formula(
    paste0("cbind(.success, .failure) ~ ", group, " + (1|", cluster, ")")
  )
  lme4::glmer(formula_obj, family = family_obj, data = dat, nAGQ = nAGQ)
}

#' Estimate group-specific probabilities from a fitted rate model
#'
#' Extracts group-level probabilities from a model produced by
#' [fit_grouped_rate_model()]. For random-intercept logit models, a standard
#' approximation converts the conditional log-odds to an approximate marginal
#' log-odds.
#'
#' @param model A model from [fit_grouped_rate_model()].
#' @param group_levels Optional vector of group levels; recovered from the model
#'   frame by default.
#' @param group Name of the grouping column used in the model.
#' @param link Link function.
#' @param approximate_marginal Logical; apply the logit approximation for
#'   random-intercept models.
#' @param logit_scale_factor Approximation constant.
#' @return A data frame of group-level linear predictors and probabilities.
#' @export
estimate_group_rates <- function(
  model,
  group_levels = NULL,
  group = "group",
  link = c("logit", "identity"),
  approximate_marginal = TRUE,
  logit_scale_factor = 0.346
) {
  link <- match.arg(link)

  if (is.null(group_levels)) {
    mf <- stats::model.frame(model)
    group_levels <- levels(as.factor(mf[[group]]))
  }

  coef_vals <- if (inherits(model, "merMod")) {
    lme4::fixef(model)
  } else {
    stats::coef(model)
  }

  intercept <- unname(coef_vals["(Intercept)"])
  if (is.na(intercept)) {
    stop("Model intercept could not be recovered.", call. = FALSE)
  }

  re_var <- if (inherits(model, "merMod")) {
    .random_intercept_variance(model)
  } else {
    0
  }

  rows <- vector("list", length(group_levels))
  for (i in seq_along(group_levels)) {
    level_i <- group_levels[i]
    coef_name <- paste0(group, level_i)
    eta <- intercept
    if (coef_name %in% names(coef_vals)) {
      eta <- eta + unname(coef_vals[coef_name])
    }
    eta_out <- eta
    if (inherits(model, "merMod") && identical(link, "logit") &&
        isTRUE(approximate_marginal)) {
      eta_out <- eta / sqrt(1 + logit_scale_factor * re_var)
    }
    rows[[i]] <- data.frame(
      group = level_i,
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

#' Analyze aggregated grouped outcomes
#'
#' Generic form of the applied outcome analysis: fits GLM and GLMM rate models
#' for one or more success/trial outcome definitions, across one or more links.
#'
#' @param data An aggregated data frame.
#' @param cluster Cluster identifier column.
#' @param group Grouping column.
#' @param outcomes Named list; each element is a list with `successes` and
#'   `trials` naming the relevant columns.
#' @param links Character vector of links to fit.
#' @param nAGQ Quadrature points.
#' @return A nested list of fitted models and group-rate tables, one element per
#'   outcome and link.
#' @export
analyze_aggregated_outcomes <- function(
  data,
  cluster = "cluster_id",
  group = "group",
  outcomes,
  links = c("logit", "identity"),
  nAGQ = 10
) {
  .check_required_columns(data, c(cluster, group))
  results <- list()

  for (outcome_name in names(outcomes)) {
    outcome_def <- outcomes[[outcome_name]]
    out_dat <- data[!is.na(data[[outcome_def$trials]]) &
                      data[[outcome_def$trials]] > 0, , drop = FALSE]

    link_results <- list()
    for (link_name in links) {
      glm_fit <- fit_grouped_rate_model(
        data = out_dat, successes = outcome_def$successes,
        trials = outcome_def$trials, group = group,
        link = link_name, random_intercept = FALSE
      )
      glmer_fit <- fit_grouped_rate_model(
        data = out_dat, successes = outcome_def$successes,
        trials = outcome_def$trials, group = group, cluster = cluster,
        link = link_name, random_intercept = TRUE, nAGQ = nAGQ
      )
      link_results[[link_name]] <- list(
        glm = glm_fit,
        glm_rates = estimate_group_rates(
          glm_fit, group = group, link = link_name,
          approximate_marginal = FALSE
        ),
        glmer = glmer_fit,
        glmer_rates = estimate_group_rates(
          glmer_fit, group = group, link = link_name,
          approximate_marginal = TRUE
        )
      )
    }
    results[[outcome_name]] <- link_results
  }
  results
}

# ---- Lp(a) application wrappers --------------------------------------------
# These are thin wrappers over the generic functions above. They exist for
# backward compatibility and to support the Lp(a) case-study vignette; they are
# not the package's central interface.

#' Fit a specialty-level rate model (Lp(a) application helper)
#'
#' Application wrapper around [fit_grouped_rate_model()] with specialty as the
#' grouping variable. See `vignette("lpa-case-study")`.
#'
#' @inheritParams fit_grouped_rate_model
#' @param specialty_var Name of the specialty column.
#' @param provider_var Optional provider identifier column.
#' @return A fitted model.
#' @export
fit_specialty_rate_model <- function(
  data, successes, trials,
  specialty_var = "specialty",
  provider_var = NULL,
  link = c("logit", "identity"),
  random_intercept = !is.null(provider_var),
  nAGQ = 10
) {
  link <- match.arg(link)
  fit_grouped_rate_model(
    data = data, successes = successes, trials = trials,
    group = specialty_var, cluster = provider_var,
    link = link, random_intercept = random_intercept, nAGQ = nAGQ
  )
}

#' Estimate specialty-specific probabilities (Lp(a) application helper)
#'
#' Application wrapper around [estimate_group_rates()]. The grouping column in
#' the returned data frame is named `specialty` for continuity with earlier
#' versions.
#'
#' @inheritParams estimate_group_rates
#' @param specialty_var Name of the specialty column used in the model.
#' @param specialty_levels Optional vector of specialty levels.
#' @return A data frame of specialty-level rates.
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
  out <- estimate_group_rates(
    model = model, group_levels = specialty_levels, group = specialty_var,
    link = link, approximate_marginal = approximate_marginal,
    logit_scale_factor = logit_scale_factor
  )
  names(out)[names(out) == "group"] <- "specialty"
  out
}

#' Reproduce the core Lp(a) outcome analyses (application helper)
#'
#' Application wrapper around [analyze_aggregated_outcomes()] using the Lp(a)
#' outcome definitions and specialty grouping. See
#' `vignette("lpa-case-study")`.
#'
#' @param data A physician-level analysis data frame.
#' @param provider_var Provider identifier column.
#' @param specialty_var Specialty column.
#' @param outcomes Named list defining success/trial columns for each outcome.
#' @param links Links to fit.
#' @param nAGQ Quadrature points.
#' @return A nested list of fitted models and specialty-rate tables.
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
  res <- analyze_aggregated_outcomes(
    data = data, cluster = provider_var, group = specialty_var,
    outcomes = outcomes, links = links, nAGQ = nAGQ
  )
  for (outcome_name in names(res)) {
    for (link_name in names(res[[outcome_name]])) {
      block <- res[[outcome_name]][[link_name]]
      for (tbl in c("glm_rates", "glmer_rates")) {
        if (!is.null(block[[tbl]])) {
          names(block[[tbl]])[names(block[[tbl]]) == "group"] <- "specialty"
        }
      }
      res[[outcome_name]][[link_name]] <- block
    }
  }
  res
}
