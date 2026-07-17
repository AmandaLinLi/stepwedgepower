\name{analyze_aggregated_outcomes}
\alias{analyze_aggregated_outcomes}
\title{Analyze aggregated grouped outcomes}
\usage{
analyze_aggregated_outcomes(
  data,
  cluster = "cluster_id",
  group = "group",
  outcomes,
  links = c("logit", "identity"),
  nAGQ = 10
)
}
\arguments{
\item{data}{A data frame.}

\item{cluster}{Cluster identifier column.}

\item{group}{Name of the grouping column.}

\item{outcomes}{Named list; each element gives \code{successes} and \code{trials}.}

\item{links}{Character vector of links to fit.}

\item{nAGQ}{Quadrature points.}
}
\value{
A nested list of fitted models and group-rate tables.
}
\description{
Generic form of the applied outcome analysis: fits GLM and GLMM rate models for one or more success/trial outcome definitions, across one or more links.
}
