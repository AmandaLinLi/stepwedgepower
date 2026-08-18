.component_contrast_matrix <- function(include_interaction = TRUE) {
  if (isTRUE(include_interaction)) {
    out <- rbind(
      A_vs_control = c(a = 1, b = 0, ab = 0),
      B_vs_control = c(a = 0, b = 1, ab = 0),
      B_vs_A = c(a = -1, b = 1, ab = 0),
      AB_vs_control = c(a = 1, b = 1, ab = 1),
      AB_vs_A = c(a = 0, b = 1, ab = 1),
      AB_vs_B = c(a = 1, b = 0, ab = 1),
      interaction = c(a = 0, b = 0, ab = 1)
    )
  } else {
    out <- rbind(
      A_vs_control = c(a = 1, b = 0, ab = 0),
      B_vs_control = c(a = 0, b = 1, ab = 0),
      B_vs_A = c(a = -1, b = 1, ab = 0),
      AB_vs_control = c(a = 1, b = 1, ab = 0),
      AB_vs_A = c(a = 0, b = 1, ab = 0),
      AB_vs_B = c(a = 1, b = 0, ab = 0)
    )
  }
  out
}

.component_contrast_labels <- function() {
  c(
    A_vs_control = "A vs Control",
    B_vs_control = "B vs Control",
    B_vs_A = "B vs A",
    AB_vs_control = "A+B vs Control",
    AB_vs_A = "A+B vs A",
    AB_vs_B = "A+B vs B",
    interaction = "A-by-B interaction"
  )
}

.validate_component_contrasts <- function(contrasts, include_interaction = TRUE) {
  available <- rownames(.component_contrast_matrix(include_interaction))
  if (is.null(contrasts)) return(available)
  if (!is.character(contrasts) || !length(contrasts) || anyNA(contrasts)) {
    stop("`contrasts` must be a non-empty character vector.", call. = FALSE)
  }
  unknown <- setdiff(contrasts, available)
  if (length(unknown)) {
    stop("Unknown contrast(s): ", paste(unknown, collapse = ", "),
         ". Available contrasts are ", paste(available, collapse = ", "), ".",
         call. = FALSE)
  }
  unique(contrasts)
}

.component_true_effects <- function(assumptions, include_interaction = TRUE) {
  beta <- c(
    a = assumptions$treatment_effect_a,
    b = assumptions$treatment_effect_b,
    ab = if (isTRUE(include_interaction)) assumptions$interaction_effect else 0
  )
  contrast_matrix <- .component_contrast_matrix(include_interaction)
  values <- as.numeric(contrast_matrix %*% beta)
  stats::setNames(as.list(values), rownames(contrast_matrix))
}

.component_is_estimable <- function(linear, design_matrix, tolerance = 1e-8) {
  if (length(linear) != ncol(design_matrix)) return(FALSE)
  singular <- svd(design_matrix, nu = 0, nv = ncol(design_matrix))
  if (!length(singular$d)) return(FALSE)
  rank <- sum(singular$d > max(singular$d) * tolerance)
  if (rank == ncol(design_matrix)) return(TRUE)
  null_basis <- singular$v[, seq.int(rank + 1L, ncol(design_matrix)), drop = FALSE]
  max(abs(crossprod(null_basis, linear))) <= tolerance *
    max(1, sqrt(sum(linear^2)))
}

.component_observed_cluster_period_count <- function(design) {
  observed <- .component_observed_matrix(design)
  as.integer(sum(design$clusters_per_sequence * rowSums(observed)))
}

.component_total_sample_size <- function(design, assumptions) {
  spec <- assumptions$n_per_cluster_period
  if (is.function(spec)) return(NA_real_)

  observed <- .component_observed_matrix(design)
  cluster_sequence <- rep(
    seq_len(design$n_sequences), times = design$clusters_per_sequence
  )
  sequence_idx_full <- rep(cluster_sequence, each = design$n_periods)
  period_full <- rep(seq_len(design$n_periods), times = design$n_clusters)
  keep <- observed[cbind(sequence_idx_full, period_full)]
  sequence_idx <- sequence_idx_full[keep]
  period <- period_full[keep]

  n <- .resolve_sample_size(
    spec, sequence_idx, period, design$n_sequences, design$n_periods
  )
  if (anyNA(n)) NA_real_ else sum(n)
}
