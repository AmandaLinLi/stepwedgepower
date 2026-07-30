% Generated manually for version 0.4.0
\name{sw_incomplete_component_design}
\alias{sw_incomplete_component_design}
\title{Construct a Component Design with Structurally Missing Periods}
\usage{
sw_incomplete_component_design(
  clusters_per_sequence,
  state,
  latent_state = NULL,
  sequence_names = NULL,
  missing = c("-", "--", "–", "—", "", "NA", ".")
)
}
\arguments{
  \item{clusters_per_sequence}{Positive integer vector giving the number of clusters in each sequence.}
  \item{state}{Sequence-by-period schedule containing treatment labels and structurally missing markers. Labels may include \code{0}, \code{1}, \code{2}, and \code{1+2}.}
  \item{latent_state}{Optional complete treatment-state matrix defining the latent state in every calendar period, including unobserved cells. It is required for internal missing gaps whose latent history cannot be inferred.}
  \item{sequence_names}{Optional unique sequence labels.}
  \item{missing}{Character values interpreted as structurally missing cells rather than Control. Actual \code{NA} values are always missing.}
}
\description{
Creates a component-based stepped-wedge design from a manuscript-style schedule in which dashes, blanks, or missing values mean that no outcome is observed. Leading missing cells inherit the first observed state and trailing missing cells inherit the last observed state. The inferred latent state is used only to preserve wash-in, withdrawal, carryover, and restart history; unobserved cells are excluded from simulation outcomes, model fitting, power, and resource totals.
}
\value{An object of class \code{sw_component_design} with an observation mask.}
\examples{
bswd <- sw_incomplete_component_design(
  c(2, 2, 3, 3),
  rbind(
    c("0", "1", "1+2", "1+2", "-"),
    c("0", "0", "1", "1+2", "-"),
    c("-", "0", "1", "1+2", "1+2"),
    c("-", "0", "0", "1", "1+2")
  )
)
bswd
}
