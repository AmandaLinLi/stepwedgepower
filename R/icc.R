#' Convert an intraclass correlation to a random-intercept standard deviation
#'
#' For a logistic random-intercept model the latent-scale residual variance is
#' \eqn{\pi^2/3}. This maps a latent-scale ICC to the cluster-level
#' random-intercept standard deviation.
#'
#' @param icc Numeric in `[0, 1)`. Intraclass correlation on the latent scale.
#' @return The random-intercept standard deviation.
#' @examples
#' icc_to_cluster_sd(0.05)
#' @export
icc_to_cluster_sd <- function(icc) {
  if (any(icc < 0 | icc >= 1)) {
    stop("`icc` must be in [0, 1).", call. = FALSE)
  }
  sqrt((icc * (pi^2 / 3)) / (1 - icc))
}

#' Convert a random-intercept standard deviation to an intraclass correlation
#'
#' @param cluster_sd Numeric `>= 0`. Random-intercept standard deviation.
#' @return The latent-scale intraclass correlation.
#' @examples
#' cluster_sd_to_icc(0.416)
#' @export
cluster_sd_to_icc <- function(cluster_sd) {
  if (any(cluster_sd < 0)) {
    stop("`cluster_sd` must be non-negative.", call. = FALSE)
  }
  cluster_sd^2 / (cluster_sd^2 + pi^2 / 3)
}

# Resolve icc / cluster_sd, disallowing disagreeing dual specification.
# Returns the cluster_sd to use. If both are NULL, returns `default`.
.resolve_cluster_sd <- function(icc = NULL, cluster_sd = NULL,
                                default = NULL, tol = 1e-6) {
  if (is.null(icc) && is.null(cluster_sd)) {
    return(default)
  }
  if (!is.null(icc) && !is.null(cluster_sd)) {
    implied <- icc_to_cluster_sd(icc)
    if (abs(implied - cluster_sd) > tol) {
      stop(
        sprintf(
          "`icc` (%.4f) and `cluster_sd` (%.4f) disagree; icc implies sd = %.4f.",
          icc, cluster_sd, implied
        ),
        call. = FALSE
      )
    }
    return(cluster_sd)
  }
  if (!is.null(icc)) icc_to_cluster_sd(icc) else cluster_sd
}
