\name{fit_specialty_rate_model}
\alias{fit_specialty_rate_model}
\title{Fit a specialty-level testing-rate model}
\usage{
fit_specialty_rate_model(
  data,
  successes,
  trials,
  specialty_var = "specialty",
  provider_var = NULL,
  link = c("logit", "identity"),
  random_intercept = !is.null(provider_var),
  nAGQ = 10
)
}
\arguments{
  \item{data}{A data frame containing counts and grouping variables.}
  \item{successes}{Name of the success-count column.}
  \item{trials}{Name of the trial-count column.}
  \item{specialty_var}{Name of the specialty column.}
  \item{provider_var}{Optional provider identifier column.}
  \item{link}{Link function. Supported values are \code{"logit"} and \code{"identity"}.}
  \item{random_intercept}{Logical; whether to include a provider random intercept.}
  \item{nAGQ}{Number of adaptive Gauss-Hermite quadrature points for \code{lme4::glmer()}.}
}
\value{
A fitted \code{glm} or \code{merMod} object.
}
\description{
Fits either a binomial GLM or a provider-random-intercept binomial GLMM for aggregated success/trial data.
}
