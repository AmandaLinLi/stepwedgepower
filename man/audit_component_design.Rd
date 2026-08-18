% Generated manually for version 0.4.0
\name{audit_component_design}
\alias{audit_component_design}
\alias{print.sw_component_audit}
\title{Audit a Component-Based Stepped-Wedge Design}
\usage{
audit_component_design(
  design,
  assumptions = NULL,
  contrasts = NULL,
  include_interaction = TRUE,
  adjust_sequence = FALSE,
  period_effect = c("categorical", "linear", "none")
)

\method{print}{sw_component_audit}(x, ...)
}
\arguments{
  \item{design}{An \code{sw_component_design} object.}
  \item{assumptions}{Optional \code{sw_component_assumptions} object.}
  \item{contrasts}{Requested standard contrasts.}
  \item{include_interaction}{Whether to audit an A-by-B interaction model.}
  \item{adjust_sequence}{Whether to include sequence fixed effects.}
  \item{period_effect}{Categorical, linear, or no period effect.}
  \item{x}{An \code{sw_component_audit} object.}
  \item{...}{Unused.}
}
\description{
Checks observed treatment-state support, overlap by calendar period, resource counts, withdrawal and restart patterns, fixed-effect rank, and estimability of requested contrasts. Structurally unobserved cells are excluded from rank and overlap calculations but retained for latent exposure history.
}
\value{An object of class \code{sw_component_audit}.}
\examples{
state <- rbind(S1 = c(0, 1, 3, 2), S2 = c(0, 2, 3, 3),
               S3 = c(0, 1, 1, 3), S4 = c(0, 2, 2, 3))
d <- sw_component_design(rep(3, 4), state = state)
audit_component_design(d)
}
