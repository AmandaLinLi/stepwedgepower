\name{simulate_stepwedge_trial}
\alias{simulate_stepwedge_trial}
\title{Simulate one stepped-wedge trial dataset}
\usage{
simulate_stepwedge_trial(
  treatment_or = NULL,
  n_clusters_per_sequence = NULL,
  sequence_names = NULL,
  baseline_probs = NULL,
  cluster_sd = NULL,
  icc = NULL,
  n_per_cluster_period = NULL,
  n_steps = NULL,
  seed = NULL,
  effect_size_or = NULL,
  n_providers_per_specialty = NULL,
  specialty_names = NULL,
  base_probs = NULL,
  tau_provider = NULL,
  pts_per_step = NULL
)
}
\arguments{
\item{treatment_or}{Odds ratio under the data-generating model.}

\item{n_clusters_per_sequence}{Integer vector of clusters per sequence.}

\item{sequence_names}{Labels for the sequences.}

\item{baseline_probs}{Baseline probabilities by sequence.}

\item{cluster_sd}{Standard deviation of the cluster random intercept.}

\item{icc}{Optional intraclass correlation (alternative to \code{cluster_sd}).}

\item{n_per_cluster_period}{Units per cluster per period.}

\item{n_steps}{Number of periods.}

\item{seed}{Optional seed.}

\item{effect_size_or}{Deprecated legacy alias for \code{treatment_or}.}

\item{n_providers_per_specialty}{Deprecated legacy alias for \code{n_clusters_per_sequence}.}

\item{specialty_names}{Deprecated legacy alias for \code{sequence_names}.}

\item{base_probs}{Deprecated legacy alias for \code{baseline_probs}.}

\item{tau_provider}{Deprecated legacy alias for \code{cluster_sd}.}

\item{pts_per_step}{Deprecated legacy alias for \code{n_per_cluster_period}.}
}
\value{
A data frame with one row per provider-step combination.
}
\description{
Generates aggregated provider-by-step binomial data for a sequential stepped-wedge design.
}
