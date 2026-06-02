\name{analyze_lpa_outcomes}
\alias{analyze_lpa_outcomes}
\title{Reproduce the core Lp(a) outcome analyses}
\usage{
analyze_lpa_outcomes(
  data,
  provider_var = "prov_id",
  specialty_var = "specialty",
  outcomes = list(
    overall = list(successes = "n_lpa_pat", trials = "n_total_pat"),
    high_ldl = list(successes = "n_ldl_lpa_pat", trials = "n_ldl_pat")
  ),
  links = c("logit", "identity"),
  nAGQ = 10
)
}
\arguments{
  \item{data}{A physician-level analysis data frame.}
  \item{provider_var}{Provider identifier column.}
  \item{specialty_var}{Specialty column.}
  \item{outcomes}{Named list defining success and trial columns for each outcome.}
  \item{links}{Character vector of links to fit.}
  \item{nAGQ}{Number of quadrature points for \code{glmer()}.}
}
\value{
A nested list containing fitted models and specialty-rate tables.
}
\description{
Fits the main outcome models from the original script for both overall Lp(a)
testing and Lp(a) testing among patients with elevated LDL.
}
