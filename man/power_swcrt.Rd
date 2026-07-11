\name{power_swcrt}
\alias{power_swcrt}
\title{Estimate power for a stepped-wedge trial by simulation}
\usage{
power_swcrt(
  design,
  assumptions,
  nsim = 1000,
  alpha = 0.05,
  fit_link = c("logit", "identity"),
  nAGQ = 1,
  analysis_args = list(),
  seed = NULL
)
}
\arguments{
\item{design}{An [sw_design()] object.}

\item{assumptions}{An [sw_assumptions()] object.}

\item{nsim}{Number of simulations.}

\item{alpha}{Two-sided significance level.}

\item{fit_link}{Link used when fitting the analysis model.}

\item{nAGQ}{Number of adaptive Gauss-Hermite quadrature points.}

\item{analysis_args}{Optional named list of extra arguments to [fit_stepwedge_model()].}

\item{seed}{Optional random seed.}
}
\value{
An object of class \code{"sw_power"}.
}
\description{
Repeatedly simulates from an [sw_design()] and [sw_assumptions()], fits the analysis model, and estimates rejection probability at level \code{alpha}. Returns an [sw_power] object with the Monte Carlo standard error, a confidence interval for power, per-simulation estimates and standard errors, convergence and singular-fit diagnostics, and empirical bias and coverage.
}
\examples{
\donttest{
d <- sw_design(c(8,8,8,8), crossover_period = c(2,3,4,5), n_periods = 5)
a <- sw_assumptions(baseline_prob = 0.05, treatment_or = 2, icc = 0.03)
summary(power_swcrt(d, a, nsim = 50, seed = 1))
}
}
