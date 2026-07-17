\name{power_grid}
\alias{power_grid}
\alias{print.sw_power_grid}
\alias{summary.sw_power_grid}
\alias{plot.sw_power_grid}
\title{Power across a grid of design and assumption values}
\usage{
power_grid(
  design,
  assumptions,
  vary,
  nsim = 500,
  alpha = 0.05,
  n_cores = 1,
  seed = NULL,
  ...
)
}
\arguments{
\item{design}{An \code{\link{sw_design}} object.}

\item{assumptions}{An \code{\link{sw_assumptions}} object.}

\item{vary}{A named list of vectors to vary. Names may be any \code{\link{sw_assumptions}} argument and/or \code{clusters_per_sequence}, which must then be a list of integer vectors.}

\item{nsim}{Number of simulations per grid cell.}

\item{alpha}{Two-sided significance level.}

\item{n_cores}{Number of cores. \code{1} runs serially. Results are reproducible under a given \code{seed} regardless of \code{n_cores}.}

\item{seed}{Optional seed.}

\item{...}{Passed to \code{\link{power_swcrt}}.}
}
\value{
An object of class \code{"sw_power_grid"}: a data frame with one row per grid cell, carrying the full \code{sw_power} objects in an attribute.
}
\description{
Runs \code{\link{power_swcrt}} across the Cartesian product of one or more varying parameters, holding everything else fixed. Supports parallel execution, with results reproducible under a given seed regardless of the number of cores.
}
