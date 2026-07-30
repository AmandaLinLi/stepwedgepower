% Generated manually for version 0.4.0
\name{sw_component_design}
\alias{sw_component_design}
\alias{print.sw_component_design}
\title{Construct a Component-Based Stepped-Wedge Design}
\usage{
sw_component_design(
  clusters_per_sequence,
  state = NULL,
  component_a = NULL,
  component_b = NULL,
  sequence_names = NULL,
  observed = NULL
)

\method{print}{sw_component_design}(x, ...)
}
\arguments{
  \item{clusters_per_sequence}{Positive integer vector giving the number of clusters in each sequence.}
  \item{state}{Optional sequence-by-period matrix containing codes 0 (Control), 1 (A), 2 (B), and 3 (A+B), or equivalent character labels such as \code{"1+2"}.}
  \item{component_a, component_b}{Optional binary sequence-by-period assignment matrices. Supply both when \code{state} is omitted.}
  \item{sequence_names}{Optional unique sequence labels.}
  \item{observed}{Optional logical or 0/1 sequence-by-period matrix. A false value marks a structurally unobserved cell. Its latent treatment state is retained for exposure history but it contributes no outcome or fitted-model row.}
  \item{x}{An \code{sw_component_design} object.}
  \item{...}{Unused.}
}
\description{
Creates an arbitrary asynchronous schedule containing Control, A, B, and A+B. The schedule may include withdrawal, reintroduction, and structurally unobserved cluster-periods, and need not be monotone. Use \code{sw_incomplete_component_design()} for schedules written directly with dashes or missing values.
}
\value{An object of class \code{sw_component_design}.}
\examples{
state <- rbind(
  S1 = c("Control", "A", "A", "A+B", "A+B", "B"),
  S2 = c("Control", "Control", "B", "B", "A+B", "A+B"),
  S3 = c("Control", "Control", "A", "A", "A+B", "A+B")
)
design <- sw_component_design(c(4, 4, 4), state = state)
design
}
