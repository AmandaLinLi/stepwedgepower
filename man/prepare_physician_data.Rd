\name{prepare_physician_data}
\alias{prepare_physician_data}
\title{Prepare physician-level stepped-wedge analysis data}
\usage{
prepare_physician_data(
  data,
  specialties = c("CARDIOLOGY", "FAMILY MEDICINE", "INTERNAL MEDICINE", "NEUROLOGY"),
  min_patients = 100,
  max_patients = 10000,
  specialty_var = "specialty",
  patient_var = "n_total_pat",
  provider_name_var = "PROV_NAME"
)
}
\arguments{
  \item{data}{A data frame.}
  \item{specialties}{Character vector of specialties to keep.}
  \item{min_patients}{Minimum total number of patients required.}
  \item{max_patients}{Maximum total number of patients allowed.}
  \item{specialty_var}{Name of the specialty column.}
  \item{patient_var}{Name of the total-patient count column.}
  \item{provider_name_var}{Name of the provider name column used for ordering.}
}
\value{
A filtered and sorted data frame.
}
\description{
Filters the input data to the specialties of interest, applies panel-size
thresholds, removes extreme outliers, and sorts the output.
}
