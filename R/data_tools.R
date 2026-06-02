#' Read the bundled example physician data
#'
#' Reads a small synthetic physician-level dataset stored under
#' \code{inst/extdata}. The file mirrors the columns expected by the package's
#' data preparation and modeling functions.
#'
#' @return A data frame.
#' @export
read_example_physician_data <- function() {
  utils::read.csv(
    system.file("extdata", "example_physicians.csv", package = "stepwedgepower"),
    stringsAsFactors = FALSE
  )
}

#' Prepare physician-level stepped-wedge analysis data
#'
#' Filters a physician-level dataset to the specialties of interest, keeps
#' physicians above a minimum panel size, removes extremely large outliers, and
#' sorts the output for analysis.
#'
#' @param data A data frame with at least specialty and total-patient columns.
#' @param specialties Character vector of specialties to keep.
#' @param min_patients Minimum total number of patients required to retain a
#'   physician.
#' @param max_patients Maximum total number of patients allowed. This is useful
#'   for trimming extreme outliers.
#' @param specialty_var Name of the specialty column.
#' @param patient_var Name of the total-patient count column.
#' @param provider_name_var Name of the provider name column used for ordering.
#'
#' @return A filtered and sorted data frame.
#' @export
prepare_physician_data <- function(
  data,
  specialties = c("CARDIOLOGY", "FAMILY MEDICINE", "INTERNAL MEDICINE", "NEUROLOGY"),
  min_patients = 100,
  max_patients = 10000,
  specialty_var = "specialty",
  patient_var = "n_total_pat",
  provider_name_var = "PROV_NAME"
) {
  .check_required_columns(data, c(specialty_var, patient_var, provider_name_var))

  keep <- data[[specialty_var]] %in% specialties
  filtered <- data[keep, , drop = FALSE]

  filtered <- filtered[!is.na(filtered[[patient_var]]), , drop = FALSE]
  filtered <- filtered[filtered[[patient_var]] >= min_patients, , drop = FALSE]
  filtered <- filtered[filtered[[patient_var]] < max_patients, , drop = FALSE]

  ord <- order(filtered[[specialty_var]], filtered[[provider_name_var]])
  filtered[ord, , drop = FALSE]
}

#' Summarize physician counts by specialty
#'
#' Computes sample size and common summary statistics for one or more numeric
#' variables within each specialty.
#'
#' @param data A data frame.
#' @param specialty_var Name of the specialty column.
#' @param vars Character vector of numeric variable names to summarize.
#' @param na.rm Logical; whether to remove missing values.
#'
#' @return A data frame with one row per specialty-variable combination.
#' @export
summarize_by_specialty <- function(
  data,
  specialty_var = "specialty",
  vars = c("n_total_pat", "n_ldl_pat"),
  na.rm = TRUE
) {
  .check_required_columns(data, c(specialty_var, vars))

  specialties <- .specialty_levels(data, specialty_var)
  out <- vector("list", length(vars) * length(specialties))
  idx <- 1L

  for (var_name in vars) {
    for (sp in specialties) {
      values <- data[data[[specialty_var]] == sp, var_name]
      if (na.rm) {
        values <- values[!is.na(values)]
      }
      if (length(values) == 0) {
        stats_row <- data.frame(
          variable = var_name,
          specialty = sp,
          n = 0L,
          min = NA_real_,
          q1 = NA_real_,
          median = NA_real_,
          mean = NA_real_,
          q3 = NA_real_,
          max = NA_real_,
          stringsAsFactors = FALSE
        )
      } else {
        qs <- stats::quantile(values, probs = c(0.25, 0.5, 0.75), na.rm = na.rm)
        stats_row <- data.frame(
          variable = var_name,
          specialty = sp,
          n = length(values),
          min = min(values, na.rm = na.rm),
          q1 = unname(qs[1]),
          median = unname(qs[2]),
          mean = mean(values, na.rm = na.rm),
          q3 = unname(qs[3]),
          max = max(values, na.rm = na.rm),
          stringsAsFactors = FALSE
        )
      }
      out[[idx]] <- stats_row
      idx <- idx + 1L
    }
  }

  do.call(rbind, out)
}
