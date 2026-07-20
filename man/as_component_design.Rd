% Generated manually for version 0.4.0
\name{as_component_design}
\alias{as_component_design}
\title{Convert a Cumulative Design to a Component Design}
\usage{as_component_design(design)}
\arguments{\item{design}{An \code{sw_multistage_design} or \code{sw_component_design} object.}}
\description{Maps the version 0.3.0 Control/A/A+B representation to the general four-state component representation.}
\value{An \code{sw_component_design} object.}
\examples{
old <- sw_multistage_design(c(3, 3), a_start = c(2, 3),
                            b_start = c(4, 5), n_periods = 5)
as_component_design(old)
}
