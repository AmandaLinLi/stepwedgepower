#' Read the bundled example physician data
#'
#' Reads a small synthetic physician-level dataset stored under `inst/extdata`.
#' Used by the Lp(a) case-study vignette.
#'
#' @return A data frame.
#' @export
read_example_physician_data <- function() {
  utils::read.csv(
    system.file("extdata", "example_physicians.csv", package = "stepwedgepower"),
    stringsAsFactors = FALSE
  )
}

#' Prepare grouped cluster-level analysis data
#'
#' Filters a cluster-level dataset to the groups of interest, keeps clusters
#' within a size window, and sorts the result. This is the generic form of the
#' former physician-data preparation: `group_var` may be a specialty, region,
#' school district, or any categorical grouping, and `size_var` any per-cluster
#' size measure.
#'
#' @param data A data frame with a grouping column and a size column.
#' @param groups Optional character vector of groups to keep. `NULL` keeps all.
#' @param min_size Minimum cluster size to retain (inclusive).
#' @param max_size Maximum cluster size (exclusive), for trimming outliers.
#' @param group_var Name of the grouping column.
#' @param size_var Name of the cluster-size column.
#' @param order_var Optional column used for ordering within group.
#' @return A filtered and sorted data frame.
#' @examples
#' dat <- read_example_physician_data()
#' out <- prepare_cluster_data(dat, group_var = "specialty",
#'                             size_var = "n_total_pat", min_size = 100)
#' nrow(out)
#' @export
prepare_cluster_data <- function(
  data,
  groups = NULL,
  min_size = 0,
  max_size = Inf,
  group_var = "group",
  size_var = "n",
  order_var = NULL
) {
  required <- c(group_var, size_var)
  if (!is.null(order_var)) required <- c(required, order_var)
  .check_required_columns(data, required)

  filtered <- data
  if (!is.null(groups)) {
    filtered <- filtered[filtered[[group_var]] %in% groups, , drop = FALSE]
  }
  filtered <- filtered[!is.na(filtered[[size_var]]), , drop = FALSE]
  filtered <- filtered[filtered[[size_var]] >= min_size, , drop = FALSE]
  filtered <- filtered[filtered[[size_var]] < max_size, , drop = FALSE]

  ord <- if (!is.null(order_var)) {
    order(filtered[[group_var]], filtered[[order_var]])
  } else {
    order(filtered[[group_var]])
  }
  filtered[ord, , drop = FALSE]
}

#' Summarize numeric variables by group
#'
#' Computes the sample size and summary statistics of one or more numeric
#' variables within each level of a grouping variable. Generic form of the
#' former specialty summary.
#'
#' @param data A data frame.
#' @param group_var Name of the grouping column.
#' @param vars Character vector of numeric variable names.
#' @param na.rm Logical; remove missing values.
#' @return A data frame with one row per group-variable combination.
#' @examples
#' dat <- read_example_physician_data()
#' summarize_by_group(dat, group_var = "specialty", vars = "n_total_pat")
#' @export
summarize_by_group <- function(
  data,
  group_var = "group",
  vars,
  na.rm = TRUE
) {
  .check_required_columns(data, c(group_var, vars))
  groups <- levels(as.factor(data[[group_var]]))
  out <- vector("list", length(vars) * length(groups))
  idx <- 1L

  for (var_name in vars) {
    for (g in groups) {
      values <- data[data[[group_var]] == g, var_name]
      if (na.rm) values <- values[!is.na(values)]
      if (length(values) == 0) {
        stats_row <- data.frame(
          variable = var_name, group = g, n = 0L,
          min = NA_real_, q1 = NA_real_, median = NA_real_,
          mean = NA_real_, q3 = NA_real_, max = NA_real_,
          stringsAsFactors = FALSE
        )
      } else {
        qs <- stats::quantile(values, probs = c(0.25, 0.5, 0.75), na.rm = na.rm)
        stats_row <- data.frame(
          variable = var_name, group = g, n = length(values),
          min = min(values, na.rm = na.rm), q1 = unname(qs[1]),
          median = unname(qs[2]), mean = mean(values, na.rm = na.rm),
          q3 = unname(qs[3]), max = max(values, na.rm = na.rm),
          stringsAsFactors = FALSE
        )
      }
      out[[idx]] <- stats_row
      idx <- idx + 1L
    }
  }
  do.call(rbind, out)
}

# ---- Lp(a) application wrappers --------------------------------------------

#' Prepare physician-level analysis data (Lp(a) application helper)
#'
#' Application wrapper around [prepare_cluster_data()] preserving the original
#' physician-level defaults and column names. See `vignette("lpa-case-study")`.
#'
#' @param data A physician-level data frame.
#' @param specialties Character vector of specialties to keep.
#' @param min_patients Minimum total patients to retain a physician.
#' @param max_patients Maximum total patients allowed (exclusive).
#' @param specialty_var Specialty column.
#' @param patient_var Total-patient count column.
#' @param provider_name_var Provider-name column, used for ordering.
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
  prepare_cluster_data(
    data = data, groups = specialties,
    min_size = min_patients, max_size = max_patients,
    group_var = specialty_var, size_var = patient_var,
    order_var = provider_name_var
  )
}

#' Summarize physician counts by specialty (Lp(a) application helper)
#'
#' Application wrapper around [summarize_by_group()]. The grouping column in the
#' output is named `specialty` for continuity with earlier versions.
#'
#' @param data A data frame.
#' @param specialty_var Specialty column.
#' @param vars Numeric variables to summarize.
#' @param na.rm Logical; remove missing values.
#' @return A data frame with one row per specialty-variable combination.
#' @export
summarize_by_specialty <- function(
  data,
  specialty_var = "specialty",
  vars = c("n_total_pat", "n_ldl_pat"),
  na.rm = TRUE
) {
  out <- summarize_by_group(
    data = data, group_var = specialty_var, vars = vars, na.rm = na.rm
  )
  names(out)[names(out) == "group"] <- "specialty"
  out
}
