\name{simulate_stepwedge_trial}
\alias{simulate_stepwedge_trial}
\title{Simulate one stepped-wedge trial dataset}
\usage{
simulate_stepwedge_trial(
  effect_size_or = 1.5,
  n_providers_per_specialty = c(40, 40, 40, 40),
  specialty_names = c("Cardiol", "IntMed", "FamMed", "Neurol"),
  tau_provider = 1.21,
  base_probs = c(0.06, 0.04, 0.03, 0.02),
  pts_per_step = 20,
  n_steps = length(n_providers_per_specialty) + 1L,
  seed = NULL
)
}
\arguments{
  \item{effect_size_or}{Odds ratio for treatment under the data-generating model.}
  \item{n_providers_per_specialty}{Provider counts by specialty.}
  \item{specialty_names}{Labels for the specialty groups.}
  \item{tau_provider}{Standard deviation of the provider random intercept.}
  \item{base_probs}{Baseline testing probabilities for each specialty.}
  \item{pts_per_step}{Number of patients per provider per study step.}
  \item{n_steps}{Number of study steps.}
  \item{seed}{Optional random seed.}
}
\value{
A data frame with one row per provider-step combination.
}
\description{
Generates aggregated provider-by-step binomial data for a sequential stepped-wedge design.
}
