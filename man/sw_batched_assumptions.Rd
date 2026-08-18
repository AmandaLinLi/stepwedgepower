% Generated manually for version 0.4.1
\name{sw_batched_assumptions}
\alias{sw_batched_assumptions}
\alias{print.sw_batched_assumptions}
\title{Specify Assumptions for a Batched Stepped-Wedge Trial}
\usage{
sw_batched_assumptions(
  ...,
  time_model = c("calendar", "time_on_trial", "separate"),
  time_effects = NULL
)

\method{print}{sw_batched_assumptions}(x, ...)
}
\arguments{
  \item{...}{Arguments passed to \code{sw_component_assumptions}. Do not supply \code{period_effects}; use \code{time_effects}.}
  \item{time_model}{Data-generating time parameterization: calendar time, shared time on trial, or separate batch-specific time.}
  \item{time_effects}{Time-effect profile on the log-odds scale. For separate time, supply a numeric matrix or list with one profile per batch.}
  \item{x}{An \code{sw_batched_assumptions} object.}
}
\description{Extends component assumptions with one of the three time parameterizations used for batched stepped-wedge designs.}
\value{An object of class \code{sw_batched_assumptions}, inheriting from \code{sw_component_assumptions}.}
