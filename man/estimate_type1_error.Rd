\name{estimate_type1_error}
\alias{estimate_type1_error}
\title{Estimate type I error by repeated stepped-wedge simulation}
\usage{
estimate_type1_error(
  n_simulations = 100,
  alpha = 0.05,
  n_clusters_per_sequence = NULL,
  sequence_names = NULL,
  cluster_sd = NULL,
  icc = NULL,
  baseline_probs = NULL,
  n_per_cluster_period = NULL,
  n_steps = NULL,
  fit_link = c("logit", "identity"),
  seed = NULL,
  nAGQ = 1,
  n_providers_per_specialty = NULL,
  specialty_names = NULL,
  base_probs = NULL,
  tau_provider = NULL,
  pts_per_step = NULL
)
}
\arguments{
\item{n_simulations}{Number of simulations.}

\item{alpha}{Significance threshold.}

\item{n_clusters_per_sequence}{Integer vector of clusters per sequence.}

\item{sequence_names}{Labels for the sequences.}

\item{cluster_sd}{Standard deviation of the cluster random intercept.}

\item{icc}{Optional intraclass correlation (alternative to \code{cluster_sd}).}

\item{baseline_probs}{Baseline probabilities by sequence.}

\item{n_per_cluster_period}{Units per cluster per period.}

\item{n_steps}{Number of periods.}

\item{fit_link}{Link used when fitting the analysis model.}

\item{seed}{Optional seed.}

\item{nAGQ}{Number of quadrature points.}

\item{n_providers_per_specialty}{Deprecated legacy alias for \code{n_clusters_per_sequence}.}

\item{specialty_names}{Deprecated legacy alias for \code{sequence_names}.}

\item{base_probs}{Deprecated legacy alias for \code{baseline_probs}.}

\item{tau_provider}{Deprecated legacy alias for \code{cluster_sd}.}

\item{pts_per_step}{Deprecated legacy alias for \code{n_per_cluster_period}.}
}
\value{
A list like \code{\link{estimate_power}} with the type I error estimate added.
}
\description{
A convenience wrapper around \code{\link{estimate_power}} that sets the treatment odds ratio to 1.
}
