\name{analyze_swcrt}
\alias{analyze_swcrt}
\title{Analyze a simulated stepped-wedge trial}
\usage{
analyze_swcrt(
  trial,
  ...
)
}
\arguments{
\item{trial}{A data frame from [simulate_swcrt()].}

\item{...}{Passed on / unused.}
}
\value{
The list returned by [fit_stepwedge_model()].
}
\description{
Convenience wrapper around [fit_stepwedge_model()] for data produced by [simulate_swcrt()].
}
