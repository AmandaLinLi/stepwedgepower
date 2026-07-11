\name{cluster_sd_to_icc}
\alias{cluster_sd_to_icc}
\title{Convert a random-intercept standard deviation to an intraclass correlation}
\usage{
cluster_sd_to_icc(cluster_sd)
}
\arguments{
\item{cluster_sd}{Numeric \code{>= 0}. Random-intercept standard deviation.}
}
\value{
The latent-scale intraclass correlation.
}
\description{
Inverse of \code{\link{icc_to_cluster_sd}}.
}
\examples{
cluster_sd_to_icc(0.416)
}
