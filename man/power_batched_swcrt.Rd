% Generated manually for version 0.4.1
\name{power_batched_swcrt}
\alias{power_batched_swcrt}
\alias{type1_batched_swcrt}
\alias{print.sw_batched_power}
\title{Power and Type I Error for Batched Stepped-Wedge Designs}
\usage{
power_batched_swcrt(
  design, assumptions,
  analysis_time_model = c("calendar", "time_on_trial", "separate"),
  nsim = 1000, alpha = 0.05, contrasts = NULL,
  include_interaction = FALSE,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL, fit_link = c("logit", "identity"),
  nAGQ = 1, analysis_args = list(), n_cores = 1, seed = NULL,
  check_design = TRUE, warn_on_design = TRUE
)

type1_batched_swcrt(
  design, assumptions,
  analysis_time_model = c("calendar", "time_on_trial", "separate"),
  nsim = 1000, alpha = 0.05, contrasts = NULL,
  include_interaction = FALSE,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL, fit_link = c("logit", "identity"),
  nAGQ = 1, analysis_args = list(), n_cores = 1, seed = NULL,
  check_design = TRUE, warn_on_design = TRUE
)
\method{print}{sw_batched_power}(x, ...)
}
\arguments{
  \item{design}{An \code{sw_batched_design} object.}
  \item{assumptions}{An \code{sw_batched_assumptions} object.}
  \item{analysis_time_model}{Time parameterization used in the fitted model.}
  \item{nsim}{Number of simulated trials.}
  \item{alpha}{Two-sided significance level.}
  \item{contrasts}{Standard component contrasts.}
  \item{include_interaction}{Whether to fit the A-by-B interaction. Defaults to \code{FALSE}; set to \code{TRUE} for adequately supported four-state designs.}
  \item{multiplicity}{Multiplicity adjustment.}
  \item{multiplicity_family}{Contrasts in the adjustment family.}
  \item{fit_link}{Binomial analysis link.}
  \item{nAGQ}{Adaptive quadrature points.}
  \item{analysis_args}{Additional arguments passed to the component fitting engine.}
  \item{n_cores}{Number of processor cores.}
  \item{seed}{Optional reproducibility seed.}
  \item{check_design}{Whether to run a batched-design audit.}
  \item{warn_on_design}{Whether audit cautions produce a warning.}
  \item{x}{An \code{sw_batched_power} object.}
  \item{...}{Unused.}
}
\description{Estimates operating characteristics under a selected analysis time model. The generating time model remains separately specified in the assumptions object, allowing matched or misspecified time-model studies.}
\value{An object of class \code{sw_batched_power}, inheriting from \code{sw_component_power}.}
