% Generated manually for version 0.4.0
\name{power_component_swcrt}
\alias{power_component_swcrt}
\alias{type1_component_swcrt}
\alias{sw_component_power}
\alias{print.sw_component_power}
\alias{summary.sw_component_power}
\alias{plot.sw_component_power}
\title{Power and Type I Error for Component-Based Designs}
\usage{
power_component_swcrt(
  design,
  assumptions,
  nsim = 1000,
  alpha = 0.05,
  contrasts = NULL,
  include_interaction = TRUE,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL,
  fit_link = c("logit", "identity"),
  nAGQ = 1,
  analysis_args = list(),
  n_cores = 1,
  seed = NULL,
  check_design = TRUE,
  warn_on_design = TRUE
)

type1_component_swcrt(
  design,
  assumptions,
  nsim = 1000,
  alpha = 0.05,
  contrasts = NULL,
  include_interaction = TRUE,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL,
  fit_link = c("logit", "identity"),
  nAGQ = 1,
  analysis_args = list(),
  n_cores = 1,
  seed = NULL,
  check_design = TRUE,
  warn_on_design = TRUE
)

\method{print}{sw_component_power}(x, ...)
\method{summary}{sw_component_power}(object, ...)
\method{plot}{sw_component_power}(x, type = c("conditional", "failure-aware"), ...)
}
\arguments{
  \item{design}{An \code{sw_component_design} object.}
  \item{assumptions}{An \code{sw_component_assumptions} object.}
  \item{nsim}{Number of simulated trials.}
  \item{alpha}{Two-sided significance level.}
  \item{contrasts}{Standard contrasts to evaluate.}
  \item{include_interaction}{Whether to fit the interaction.}
  \item{multiplicity}{Multiplicity adjustment.}
  \item{multiplicity_family}{Contrasts in the adjustment family.}
  \item{fit_link}{Binomial analysis link.}
  \item{nAGQ}{Adaptive quadrature points.}
  \item{analysis_args}{Additional arguments to \code{fit_component_model}.}
  \item{n_cores}{Number of processor cores.}
  \item{seed}{Optional reproducibility seed.}
  \item{check_design}{Whether to run a design audit.}
  \item{warn_on_design}{Whether audit cautions produce a warning.}
  \item{x, object}{An \code{sw_component_power} object.}
  \item{type}{Conditional or failure-aware plotting scale.}
  \item{...}{Additional plotting arguments or unused.}
}
\description{
Estimates raw and multiplicity-adjusted rejection probabilities, exact Monte Carlo intervals, estimation diagnostics, convergence, singularity, and both conditional and failure-aware power. Fit output separately reports hard GLMM errors, nonconverged fits, converged fits with non-finite requested contrasts, singular fits, and successful fits. The type I error wrapper sets all component effects to zero.
}
\value{An object of class \code{sw_component_power}. The \code{fit_diagnostics} table contains aggregate counts and rates for the five fit categories. The \code{replicate_diagnostics} table contains one row per simulation with fit status, singularity, non-finite contrasts, optimizer code, and retained error and convergence messages. Singular fit is a non-exclusive property and may overlap with successful fit.}
\examples{
\donttest{
state <- rbind(S1 = c(0, 1, 3, 2), S2 = c(0, 2, 3, 3),
               S3 = c(0, 1, 1, 3), S4 = c(0, 2, 2, 3))
d <- sw_component_design(rep(3, 4), state = state)
a <- sw_component_assumptions(baseline_prob = 0.15,
  treatment_or_a = 1.5, treatment_or_b = 1.4,
  interaction_or = 1.1, icc = 0.05, n_per_cluster_period = 30)
power_component_swcrt(d, a, nsim = 20, seed = 1,
                      warn_on_design = FALSE)
}
}
