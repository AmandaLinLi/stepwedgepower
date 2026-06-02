\name{summarize_by_specialty}
\alias{summarize_by_specialty}
\title{Summarize physician counts by specialty}
\usage{
summarize_by_specialty(
  data,
  specialty_var = "specialty",
  vars = c("n_total_pat", "n_ldl_pat"),
  na.rm = TRUE
)
}
\arguments{
  \item{data}{A data frame.}
  \item{specialty_var}{Name of the specialty column.}
  \item{vars}{Character vector of numeric variable names to summarize.}
  \item{na.rm}{Logical; whether to remove missing values.}
}
\value{
A data frame with one row per specialty-variable combination.
}
\description{
Computes common summary statistics for one or more numeric variables within each specialty.
}
