% Generated manually for version 0.4.0
\name{simulate_component_swcrt}
\alias{simulate_component_swcrt}
\title{Simulate a Component-Based Stepped-Wedge Trial}
\usage{simulate_component_swcrt(design, assumptions, seed = NULL)}
\arguments{
  \item{design}{An \code{sw_component_design} object.}
  \item{assumptions}{An \code{sw_component_assumptions} object.}
  \item{seed}{Optional random seed.}
}
\description{
Generates aggregated binary cluster-period outcomes under arbitrary Control/A/B/A+B schedules with wash-in, withdrawal, carryover, restart, secular trend, component effects, and interaction.
}
\value{A data frame containing assignments, exposure histories, effect weights, outcomes, and true probabilities.}
\examples{
state <- rbind(S1 = c(0, 1, 3, 2), S2 = c(0, 2, 3, 3))
d <- sw_component_design(c(3, 3), state = state)
a <- sw_component_assumptions(baseline_prob = 0.15,
  treatment_or_a = 1.4, treatment_or_b = 1.3,
  interaction_or = 1.1, icc = 0.05)
head(simulate_component_swcrt(d, a, seed = 1))
}
