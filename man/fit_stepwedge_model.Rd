\name{fit_stepwedge_model}
\alias{fit_stepwedge_model}
\title{Fit a stepped-wedge analysis model}
\usage{
fit_stepwedge_model(
  data,
  formula = NULL,
  outcome = c("events", "n"),
  treatment = "intervention",
  period = "period",
  cluster = "cluster_id",
  sequence = "sequence",
  adjust_sequence = FALSE,
  period_effect = c("categorical", "linear", "none"),
  link = c("logit", "identity"),
  nAGQ = 1
)
}
\arguments{
\item{data}{Aggregated cluster-period data.}

\item{formula}{Optional model formula; if supplied, the naming/structural arguments are ignored.}

\item{outcome}{Outcome type. Currently \code{"binary"}.}

\item{treatment}{Optional 0/1 matrix (sequences x periods); overrides \code{crossover_period}.}

\item{period}{Name of the period column.}

\item{cluster}{Name of the cluster identifier column.}

\item{sequence}{Name of the sequence column.}

\item{adjust_sequence}{Logical; include a fixed sequence adjustment.}

\item{period_effect}{One of \code{"categorical"}, \code{"linear"}, \code{"none"}.}

\item{link}{Binomial link, \code{"logit"} or \code{"identity"}.}

\item{nAGQ}{Number of adaptive Gauss-Hermite quadrature points.}
}
\value{
A list with the fitted model, treatment coefficient row, estimate, standard error, p-value, and convergence/singularity flags.
}
\description{
Fits a binomial random-intercept model to aggregated cluster-period data, configurable through variable names or a user-supplied \code{formula}. Independent of any application-specific column names.
}
\examples{
d <- sw_design(c(6,6,6,6), crossover_period = c(2,3,4,5), n_periods = 5)
a <- sw_assumptions(baseline_prob = 0.1, treatment_or = 2, icc = 0.03)
fit_stepwedge_model(simulate_swcrt(d, a, seed = 1))$p_value
}
