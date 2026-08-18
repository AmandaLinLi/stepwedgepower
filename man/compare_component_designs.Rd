% Generated manually for version 0.4.0
\name{compare_component_designs}
\alias{compare_component_designs}
\alias{print.sw_component_comparison}
\title{Compare Component-Based Stepped-Wedge Designs}
\usage{
compare_component_designs(
  designs,
  assumptions,
  labels = NULL,
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

\method{print}{sw_component_comparison}(x, ...)
}
\arguments{
  \item{designs}{A list of at least two \code{sw_component_design} objects.}
  \item{assumptions}{One assumptions object used for all designs, or one per design.}
  \item{labels}{Optional design labels.}
  \item{nsim, alpha, contrasts, include_interaction, multiplicity, multiplicity_family, fit_link, nAGQ, analysis_args, n_cores, seed, check_design, warn_on_design}{Arguments passed to \code{power_component_swcrt}.}
  \item{x}{An \code{sw_component_comparison} object.}
  \item{...}{Unused.}
}
\description{Runs the component power engine for multiple schedules and reports operating characteristics and resource totals side by side. Calendar and observed cluster-period totals are reported separately so complete and batched stepped-wedge designs can be compared under equal observed resources.}
\value{An object of class \code{sw_component_comparison}. The \code{fit_diagnostics} element combines the five fit-category counts and rates across designs.}
\examples{
\donttest{
state <- rbind(S1 = c(0, 1, 3, 2), S2 = c(0, 2, 3, 3),
               S3 = c(0, 1, 1, 3), S4 = c(0, 2, 2, 3))
d <- sw_component_design(rep(3, 4), state = state)
a <- sw_component_assumptions(baseline_prob = 0.15,
  treatment_or_a = 1.5, treatment_or_b = 1.4,
  interaction_or = 1.1, icc = 0.05)
compare_component_designs(list(first = d, second = d), a,
                          nsim = 10, seed = 1,
                          warn_on_design = FALSE)
}
}
