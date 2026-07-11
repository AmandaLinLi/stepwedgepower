\name{icc_to_cluster_sd}
\alias{icc_to_cluster_sd}
\title{Convert an intraclass correlation to a random-intercept standard deviation}
\usage{
icc_to_cluster_sd(icc)
}
\arguments{
\item{icc}{Numeric in \code{[0, 1)}. Intraclass correlation on the latent scale.}
}
\value{
The random-intercept standard deviation.
}
\description{
For a logistic random-intercept model the latent-scale residual variance is
\eqn{\pi^2/3}. This maps a latent-scale ICC to the cluster-level
random-intercept standard deviation.
}
\examples{
icc_to_cluster_sd(0.05)
}
