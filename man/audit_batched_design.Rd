% Generated manually for version 0.4.1
\name{audit_batched_design}
\alias{audit_batched_design}
\alias{print.sw_batched_audit}
\title{Audit a Classic or Batched Stepped-Wedge Design}
\usage{
audit_batched_design(
  design,
  assumptions = NULL,
  time_model = c("calendar", "time_on_trial", "separate"),
  contrasts = NULL,
  include_interaction = FALSE,
  adjust_sequence = FALSE
)
\method{print}{sw_batched_audit}(x, ...)
}
\arguments{
  \item{design}{An \code{sw_batched_design} object.}
  \item{assumptions}{Optional component or batched assumptions object.}
  \item{time_model}{Planned analysis time parameterization.}
  \item{contrasts}{Requested standard contrasts.}
  \item{include_interaction}{Whether to include the A-by-B interaction. Defaults to \code{FALSE}; set to \code{TRUE} for adequately supported four-state designs.}
  \item{adjust_sequence}{Whether to include sequence fixed effects.}
  \item{x}{An \code{sw_batched_audit} object.}
  \item{...}{Unused.}
}
\description{Checks batch initiation, observed-state support, fixed-effect rank, and formal estimability under the selected time model.}
\value{An object of class \code{sw_batched_audit}.}
