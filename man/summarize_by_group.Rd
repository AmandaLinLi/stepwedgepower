\name{summarize_by_group}
\alias{summarize_by_group}
\title{Summarize numeric variables by group}
\usage{
summarize_by_group(
  data,
  group_var = "group",
  vars,
  na.rm = TRUE
)
}
\arguments{
\item{data}{A data frame.}

\item{group_var}{Name of the grouping column.}

\item{vars}{Character vector of numeric variable names.}

\item{na.rm}{Logical; remove missing values.}
}
\value{
A data frame with one row per group-variable combination.
}
\description{
Computes the sample size and summary statistics of one or more numeric variables within each level of a grouping variable.
}
