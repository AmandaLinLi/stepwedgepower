\name{sw_assumptions}
\alias{sw_assumptions}
\alias{print.sw_assumptions}
\title{Specify the data-generating assumptions for a stepped-wedge trial}
\usage{
sw_assumptions(
  outcome = "binary",
  baseline_prob = NULL,
  baseline_logit = NULL,
  treatment_or = NULL,
  treatment_effect = NULL,
  icc = NULL,
  cluster_sd = NULL,
  period_effects = NULL,
  n_per_cluster_period = 20
)
}
\arguments{
\item{outcome}{Outcome type. Currently \code{"binary"}.}

\item{baseline_prob}{Baseline probability: scalar, length-\code{n_sequences} vector, or \code{n_sequences x n_periods} matrix.}

\item{baseline_logit}{Baseline linear predictor on the logit scale (same shapes as \code{baseline_prob}).}

\item{treatment_or}{Treatment odds ratio.}

\item{treatment_effect}{Treatment effect on the log-odds scale.}

\item{icc}{Latent-scale intraclass correlation.}

\item{cluster_sd}{Random-intercept standard deviation.}

\item{period_effects}{Length-\code{n_periods} secular trend on the log-odds scale.}

\item{n_per_cluster_period}{Cluster-period sample size: scalar, \code{n_sequences x n_periods} matrix, or a function of the number of rows.}
}
\value{
An object of class \code{"sw_assumptions"}.
}
\description{
Collects baseline risk, treatment effect, cluster random-effect magnitude (via \code{icc} or \code{cluster_sd}), secular period trend, and cluster-period sample size for simulation. Baseline and sample size may be scalar, per-sequence, or per sequence-period.
}
\examples{
sw_assumptions(baseline_prob = 0.05, treatment_or = 1.5, icc = 0.05,
               period_effects = c(0,0.05,0.1,0.15,0.2), n_per_cluster_period = 20)
}
