\name{sw_power}
\alias{sw_power}
\alias{print.sw_power}
\alias{summary.sw_power}
\alias{plot.sw_power}
\title{Methods for sw_power objects}
\usage{
\method{print}{sw_power}(x, ...)

\method{summary}{sw_power}(object, ...)

\method{plot}{sw_power}(x, ...)
}
\arguments{
\item{x, object}{An \code{sw_power} object from \code{\link{power_swcrt}}.}

\item{...}{Unused.}
}
\value{
Invisibly, the \code{sw_power} object. Called for their printing / plotting
side effects.
}
\description{
\code{print}, \code{summary}, and \code{plot} methods for the object returned
by \code{\link{power_swcrt}}. \code{plot} shows the distribution of estimated
treatment effects with the true effect and mean estimate marked.
}
