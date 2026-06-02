\name{estimate_type1_error}
\alias{estimate_type1_error}
\title{Estimate type I error by repeated stepped-wedge simulation}
\usage{
estimate_type1_error(
  n_simulations = 100,
  alpha = 0.05,
  n_providers_per_specialty = c(40, 40, 40, 40) * 0.25,
  specialty_names = c("Cardiol", "IntMed", "FamMed", "Neurol"),
  tau_provider = 1.21,
  base_probs = c(0.07, 0.04, 0.03, 0.02),
  pts_per_step = 100/5,
  n_steps = length(n_providers_per_specialty) + 1L,
  fit_link = c("logit", "identity"),
  seed = NULL,
  nAGQ = 1
)
}
\arguments{
  \item{n_simulations}{Number of simulations.}
  \item{alpha}{Significance threshold.}
  \item{n_providers_per_specialty}{Provider counts by specialty.}
  \item{specialty_names}{Labels for the specialty groups.}
  \item{tau_provider}{Standard deviation of the provider random intercept.}
  \item{base_probs}{Baseline testing probabilities by specialty.}
  \item{pts_per_step}{Patients per provider per study step.}
  \item{n_steps}{Number of study steps.}
  \item{fit_link}{Link used when fitting the analysis model.}
  \item{seed}{Optional random seed.}
  \item{nAGQ}{Number of quadrature points for the fitted mixed model.}
}
\value{
A list like \code{\link{estimate_power}} with the type I error estimate added.
}
\description{
A convenience wrapper around \code{\link{estimate_power}} that sets the treatment odds ratio to 1.
}
