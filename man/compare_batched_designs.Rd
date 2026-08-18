% Generated manually for version 0.4.1
\name{compare_batched_designs}
\alias{compare_batched_designs}
\alias{compare_swd_bswd}
\alias{compare_batched_time_models}
\alias{print.sw_batched_comparison}
\title{Compare Batched Stepped-Wedge Designs and Time Models}
\usage{
compare_batched_designs(
  designs, assumptions, time_models = "calendar", labels = NULL,
  nsim = 1000, alpha = 0.05, contrasts = NULL,
  include_interaction = FALSE,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL, fit_link = c("logit", "identity"),
  nAGQ = 1, analysis_args = list(), n_cores = 1, seed = NULL,
  check_design = TRUE, warn_on_design = TRUE
)
compare_swd_bswd(
  swd, bswd, assumptions,
  swd_time_model = "calendar", bswd_time_model = "calendar", ...
)
compare_batched_time_models(
  design, assumptions,
  time_models = c("calendar", "time_on_trial", "separate"), ...
)
\method{print}{sw_batched_comparison}(x, ...)
}
\arguments{
  \item{designs}{List of at least two \code{sw_batched_design} objects.}
  \item{assumptions}{One batched assumptions object or one per design.}
  \item{time_models}{One analysis time model or one per design.}
  \item{labels}{Optional design labels.}
  \item{nsim, alpha, contrasts, include_interaction, multiplicity, multiplicity_family, fit_link, nAGQ, analysis_args, n_cores, seed, check_design, warn_on_design}{Arguments passed to \code{power_batched_swcrt}.}
  \item{swd, bswd}{Classic and batched design objects.}
  \item{swd_time_model, bswd_time_model}{Analysis time models for the two designs.}
  \item{design}{One batched design for a time-model comparison.}
  \item{x}{An \code{sw_batched_comparison} object.}
  \item{...}{Additional arguments or unused.}
}
\description{Combines operating characteristics, fit diagnostics, time parameterizations, and resource totals for design or time-model comparisons.}
\value{An object of class \code{sw_batched_comparison}.}
