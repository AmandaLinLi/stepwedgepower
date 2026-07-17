\name{validate_engine}
\alias{validate_engine}
\title{Validate the simulation engine against an independent reference}
\usage{
validate_engine(
  design,
  assumptions,
  nsim = 500,
  alpha = 0.05,
  seed = NULL,
  tolerance = 0.05
)
}
\arguments{
\item{design}{An \code{\link{sw_design}} object.}

\item{assumptions}{An \code{\link{sw_assumptions}} object.}

\item{nsim}{Number of simulations per grid cell.}

\item{alpha}{Two-sided significance level.}

\item{seed}{Optional seed.}

\item{tolerance}{Maximum acceptable absolute difference in power beyond combined Monte Carlo error.}
}
\value{
A one-row data frame comparing the engine's power with the reference's.
}
\description{
Cross-checks \code{\link{power_swcrt}} against a reference implementation that shares none of its code, and reports whether the two agree within Monte Carlo error. This is a self-contained check and deliberately does not treat any external power package as ground truth.
}
