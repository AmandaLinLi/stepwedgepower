\name{estimate_group_rates}
\alias{estimate_group_rates}
\title{Estimate group-specific probabilities from a fitted rate model}
\usage{
estimate_group_rates(
  model,
  group_levels = NULL,
  group = "group",
  link = c("logit", "identity"),
  approximate_marginal = TRUE,
  logit_scale_factor = 0.346
)
}
\arguments{
\item{model}{A fitted rate model.}

\item{group_levels}{Optional vector of group levels.}

\item{group}{Name of the grouping column.}

\item{link}{Link function.}

\item{approximate_marginal}{Logical; apply the marginal approximation.}

\item{logit_scale_factor}{Approximation constant.}
}
\value{
A data frame of group-level linear predictors and probabilities.
}
\description{
Extracts group-level probabilities from a model produced by \code{\link{fit_grouped_rate_model}}.
}
