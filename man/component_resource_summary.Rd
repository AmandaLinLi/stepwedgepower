% Generated manually for version 0.4.0
\name{component_resource_summary}
\alias{component_resource_summary}
\title{Summarize Resources for a Component-Based Design}
\usage{
component_resource_summary(design, assumptions = NULL)
}
\arguments{
  \item{design}{An \code{sw_component_design} object.}
  \item{assumptions}{Optional \code{sw_component_assumptions} object. When supplied, total individual observations are calculated from the cluster-period sample-size specification.}
}
\description{
Reports calendar and observed sequence-periods and cluster-periods. Structurally unobserved cells are omitted from observed-resource totals.
}
\value{A one-row data frame containing calendar, observed, and missing resource counts and, when available, total individual observations.}
\examples{
design <- sw_incomplete_component_design(
  c(2, 2, 3, 3),
  rbind(
    c("0", "1", "1+2", "1+2", "-"),
    c("0", "0", "1", "1+2", "-"),
    c("-", "0", "1", "1+2", "1+2"),
    c("-", "0", "0", "1", "1+2")
  )
)
component_resource_summary(design)
}
