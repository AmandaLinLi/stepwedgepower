\name{fit_grouped_rate_model}
\alias{fit_grouped_rate_model}
\title{Fit a grouped rate model}
\usage{
fit_grouped_rate_model(
  data,
  successes,
  trials,
  group = "group",
  cluster = NULL,
  link = c("logit", "identity"),
  random_intercept = !is.null(cluster),
  nAGQ = 10
)
}
\arguments{
\item{data}{A data frame.}

\item{successes}{Name of the success-count column.}

\item{trials}{Name of the trial-count column.}

\item{group}{Name of the grouping column.}

\item{cluster}{Cluster identifier column.}

\item{link}{Link function.}

\item{random_intercept}{Logical; include a cluster random intercept.}

\item{nAGQ}{Quadrature points.}
}
\value{
A fitted \code{glm} or \code{merMod} object.
}
\description{
Fits a binomial GLM or cluster-random-intercept GLMM for an aggregated success/trial outcome grouped by an arbitrary grouping variable.
}
