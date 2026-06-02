\name{estimate_specialty_rates}
\alias{estimate_specialty_rates}
\title{Estimate specialty-specific probabilities from a fitted model}
\usage{
estimate_specialty_rates(
  model,
  specialty_levels = NULL,
  specialty_var = "specialty",
  link = c("logit", "identity"),
  approximate_marginal = TRUE,
  logit_scale_factor = 0.346
)
}
\arguments{
  \item{model}{A fitted model returned by \code{\link{fit_specialty_rate_model}}.}
  \item{specialty_levels}{Optional vector of specialty levels.}
  \item{specialty_var}{Name of the specialty column used in the model.}
  \item{link}{Link function for the fitted model.}
  \item{approximate_marginal}{Logical; whether to apply the random-intercept logit approximation.}
  \item{logit_scale_factor}{Approximation constant used in the shrinkage factor.}
}
\value{
A data frame with specialty-level linear predictors and probabilities.
}
\description{
Extracts specialty-level probabilities from a fitted specialty model.
}
