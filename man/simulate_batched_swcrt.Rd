% Generated manually for version 0.4.1
\name{simulate_batched_swcrt}
\alias{simulate_batched_swcrt}
\title{Simulate a Classic or Batched Stepped-Wedge Trial}
\usage{simulate_batched_swcrt(design, assumptions, seed = NULL, include_unobserved = FALSE)}
\arguments{
  \item{design}{An \code{sw_batched_design} object.}
  \item{assumptions}{An \code{sw_batched_assumptions} object.}
  \item{seed}{Optional random seed.}
  \item{include_unobserved}{Whether to retain latent unobserved rows with missing outcomes.}
}
\description{Simulates aggregated binary cluster-period outcomes and appends batch, calendar-period, and time-on-trial variables.}
\value{A data frame of cluster-period outcomes and treatment-history variables.}
