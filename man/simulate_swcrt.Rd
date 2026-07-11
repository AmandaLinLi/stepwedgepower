\name{simulate_swcrt}
\alias{simulate_swcrt}
\title{Simulate one stepped-wedge cluster-randomized trial}
\usage{
simulate_swcrt(
  design,
  assumptions,
  seed = NULL
)
}
\arguments{
\item{design}{An [sw_design()] object.}

\item{assumptions}{An [sw_assumptions()] object.}

\item{seed}{Optional random seed.}
}
\value{
A data frame with one row per cluster-period (columns \code{cluster_id}, \code{sequence}, \code{sequence_idx}, \code{period}, \code{intervention}, \code{n}, \code{events}, \code{true_prob}).
}
\description{
Generates an aggregated cluster-by-period dataset from an [sw_design()] and [sw_assumptions()] under a logistic random-intercept model with treatment, period, and baseline effects. Application-agnostic: clusters may be hospitals, clinics, schools, practices, physicians, or regions.
}
\examples{
d <- sw_design(c(8,8,8,8), crossover_period = c(2,3,4,5), n_periods = 5)
a <- sw_assumptions(baseline_prob = 0.05, treatment_or = 1.5, icc = 0.05)
head(simulate_swcrt(d, a, seed = 1))
}
