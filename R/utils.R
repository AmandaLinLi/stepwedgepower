# Utility validators and small helpers -------------------------------------

.check_required_columns <- function(data, required) {
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}

.inverse_link <- function(eta, link) {
  if (identical(link, "logit")) {
    return(stats::plogis(eta))
  }
  if (identical(link, "identity")) {
    return(eta)
  }
  stop("Unsupported link: ", link, call. = FALSE)
}

.specialty_levels <- function(data, specialty_var) {
  levels(as.factor(data[[specialty_var]]))
}

.random_intercept_variance <- function(model) {
  if (!inherits(model, "merMod")) {
    return(0)
  }
  vc <- lme4::VarCorr(model)
  first_component <- vc[[1]]
  as.numeric(first_component[1, 1])
}
