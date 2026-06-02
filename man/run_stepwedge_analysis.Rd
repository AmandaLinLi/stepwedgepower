\name{run_stepwedge_analysis}
\alias{run_stepwedge_analysis}
\title{Fit the stepped-wedge analysis model to a simulated dataset}
\usage{
run_stepwedge_analysis(
  sim_data,
  fit_link = c("logit", "identity"),
  nAGQ = 1
)
}
\arguments{
  \item{sim_data}{A compatible aggregated provider-step dataset.}
  \item{fit_link}{Link function used in the fitted model.}
  \item{nAGQ}{Number of quadrature points for \code{lme4::glmer()}.}
}
\value{
A list with the fitted model, coefficient table, and treatment p-value.
}
\description{
Fits the mixed-effects stepped-wedge analysis model used in the original script.
}
