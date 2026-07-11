\name{sw_design}
\alias{sw_design}
\alias{print.sw_design}
\title{Construct a stepped-wedge design}
\usage{
sw_design(
  clusters_per_sequence,
  crossover_period = NULL,
  n_periods = NULL,
  treatment = NULL,
  sequence_names = NULL
)
}
\arguments{
\item{clusters_per_sequence}{Integer vector; number of clusters in each sequence.}

\item{crossover_period}{Integer vector; period at which each sequence turns on (\code{Inf} = never). Ignored if \code{treatment} is given.}

\item{n_periods}{Number of periods; inferred if \code{NULL}.}

\item{treatment}{Optional 0/1 matrix (sequences x periods); overrides \code{crossover_period}.}

\item{sequence_names}{Optional sequence labels.}
}
\value{
An object of class \code{"sw_design"}.
}
\description{
Builds a stepped-wedge design object with a 0/1 treatment matrix (sequences x periods), specified either by per-sequence crossover periods or by an explicit treatment matrix. Supports multiple baseline periods, unequal clusters per sequence, and custom or simultaneous crossovers.
}
\examples{
sw_design(clusters_per_sequence = c(10,10,10,10),
         crossover_period = c(2,3,4,5), n_periods = 5)
}
