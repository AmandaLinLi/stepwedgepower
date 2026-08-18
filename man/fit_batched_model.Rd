% Generated manually for version 0.4.1
\name{fit_batched_model}
\alias{fit_batched_model}
\title{Fit a Classic or Batched Stepped-Wedge Analysis Model}
\usage{
fit_batched_model(
  data,
  time_model = c("calendar", "time_on_trial", "separate"),
  formula = NULL,
  outcome = c("events", "n"),
  contrasts = NULL,
  include_interaction = FALSE,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL,
  adjust_sequence = FALSE,
  link = c("logit", "identity"),
  nAGQ = 1,
  alpha = 0.05
)
}
\arguments{
  \item{data}{Aggregated cluster-period data from \code{simulate_batched_swcrt}.}
  \item{time_model}{Calendar, time-on-trial, or separate time parameterization.}
  \item{formula}{Optional custom mixed-model formula.}
  \item{outcome}{Names of event and total-count columns.}
  \item{contrasts}{Standard component contrasts.}
  \item{include_interaction}{Whether to fit the A-by-B interaction. Defaults to \code{FALSE}, which is appropriate for cumulative Control/A/A+B schedules without a B-only state.}
  \item{multiplicity}{Multiplicity adjustment.}
  \item{multiplicity_family}{Contrasts in the adjustment family.}
  \item{adjust_sequence}{Whether to include sequence fixed effects.}
  \item{link}{Binomial link.}
  \item{nAGQ}{Adaptive quadrature points.}
  \item{alpha}{Two-sided significance level.}
}
\description{Fits the component GLMM with shared categorical calendar effects, shared categorical time-on-trial effects, or a categorical batch-by-local-time factor giving each batch a separate time profile.}
\value{The result from \code{fit_component_model} with batched-time metadata.}
