% Generated manually for version 0.4.1
\name{sw_batched_design}
\alias{sw_batched_design}
\alias{as_batched_design}
\alias{print.sw_batched_design}
\title{Construct a Classic or Batched Stepped-Wedge Design}
\usage{
sw_batched_design(
  clusters_per_sequence,
  state,
  batch = NULL,
  latent_state = NULL,
  sequence_names = NULL,
  missing = c("-", "--", "–", "—", "", "NA", ".")
)

as_batched_design(design, batch = NULL)

\method{print}{sw_batched_design}(x, ...)
}
\arguments{
  \item{clusters_per_sequence}{Positive integer vector giving clusters per sequence.}
  \item{state}{Sequence-by-calendar-period treatment schedule. Labels may include \code{0}, \code{1}, \code{2}, \code{1+2}, \code{Control}, \code{A}, \code{B}, and \code{A+B}.}
  \item{batch}{Optional batch label per sequence. When \code{NULL}, batches are inferred from first observed calendar periods.}
  \item{latent_state}{Optional complete latent treatment-state matrix for schedules with internal unobserved cells.}
  \item{sequence_names}{Optional sequence labels.}
  \item{missing}{Character markers treated as structurally unobserved.}
  \item{design}{An \code{sw_component_design} object.}
  \item{x}{An \code{sw_batched_design} object.}
  \item{...}{Unused.}
}
\description{
Creates a batch-aware component design. A batched stepped-wedge design groups sequences into batches that begin trial participation at staggered calendar periods. Dashes are structurally unobserved cluster-periods and are not Control observations.
}
\details{
All sequences in a batch must share the same first observed calendar period. The returned object records calendar period, time on trial, batch starts, delays from the first batch, gaps from the preceding batch, cluster totals by batch, and the treatment schedule and observation mask.
}
\value{An object of class \code{sw_batched_design}, inheriting from \code{sw_component_design}.}
\examples{
bswd <- sw_batched_design(
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
