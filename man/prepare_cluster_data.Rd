\name{prepare_cluster_data}
\alias{prepare_cluster_data}
\title{Prepare grouped cluster-level analysis data}
\usage{
prepare_cluster_data(
  data,
  groups = NULL,
  min_size = 0,
  max_size = Inf,
  group_var = "group",
  size_var = "n",
  order_var = NULL
)
}
\arguments{
\item{data}{A data frame.}

\item{groups}{Optional character vector of groups to keep.}

\item{min_size}{Minimum cluster size (inclusive).}

\item{max_size}{Maximum cluster size (exclusive).}

\item{group_var}{Name of the grouping column.}

\item{size_var}{Name of the cluster-size column.}

\item{order_var}{Optional ordering column.}
}
\value{
A filtered and sorted data frame.
}
\description{
Filters a cluster-level dataset to the groups of interest, keeps clusters within a size window, and sorts the result.
}
