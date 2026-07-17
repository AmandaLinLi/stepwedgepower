\name{compare_with_swdpwr}
\alias{compare_with_swdpwr}
\title{Informational comparison with a closed-form power package}
\usage{
compare_with_swdpwr(
  clusters_per_sequence,
  n_per_cluster_period,
  baseline_prob,
  treatment_or,
  icc,
  alpha = 0.05,
  nsim = 500,
  seed = NULL,
  n_cores = 1,
  adjust_sequence = FALSE
)
}
\arguments{
\item{clusters_per_sequence}{Integer vector; must be constant across sequences for this comparison.}

\item{n_per_cluster_period}{Cluster-period size (a single value).}

\item{baseline_prob}{Common baseline probability under control.}

\item{treatment_or}{Treatment odds ratio.}

\item{icc}{Latent-scale intraclass correlation.}

\item{alpha}{Two-sided significance level.}

\item{nsim}{Number of simulations per grid cell.}

\item{seed}{Optional seed.}

\item{n_cores}{Number of cores. \code{1} runs serially. Results are reproducible under a given \code{seed} regardless of \code{n_cores}.}

\item{adjust_sequence}{Whether the fitted model includes a fixed sequence term.}
}
\value{
A one-row data frame with the simulated power, its Monte Carlo interval, the closed-form power, and their difference.
}
\description{
Runs \code{\link{power_swcrt}} alongside the closed-form calculation in the \pkg{swdpwr} package. Offered for information, not as a validation target: closed-form stepped-wedge power is asymptotic in the number of clusters and can be markedly optimistic at the small cluster counts these trials typically use.
}
\references{
Chen J, Zhou X, Li F, Spiegelman D (2022). "swdpwr: A SAS macro and an R
package for power calculations in stepped wedge cluster randomized trials."
\emph{Computer Methods and Programs in Biomedicine}, 213, 106522.
\doi{10.1016/j.cmpb.2021.106522}

Zhou X, Liao X, Kunz LM, Normand SLT, Wang M, Spiegelman D (2020). "A maximum
likelihood approach to power calculations for stepped wedge designs of binary
outcomes." \emph{Biostatistics}, 21(1), 102-121.
\doi{10.1093/biostatistics/kxy031}
}
