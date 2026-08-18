% Generated manually for version 0.4.0
\name{fit_component_model}
\alias{fit_component_model}
\title{Fit a Component-Based Stepped-Wedge Model}
\usage{
fit_component_model(
  data,
  formula = NULL,
  outcome = c("events", "n"),
  weight_a = "a_effect_weight",
  weight_b = "b_effect_weight",
  weight_ab = "ab_effect_weight",
  term_a = weight_a,
  term_b = weight_b,
  term_ab = weight_ab,
  include_interaction = TRUE,
  contrasts = NULL,
  multiplicity = c("holm", "bonferroni", "none"),
  multiplicity_family = NULL,
  period = "period",
  cluster = "cluster_id",
  sequence = "sequence",
  adjust_sequence = FALSE,
  period_effect = c("categorical", "linear", "none"),
  link = c("logit", "identity"),
  nAGQ = 1,
  alpha = 0.05
)
}
\arguments{
  \item{data}{Aggregated cluster-period data.}
  \item{formula}{Optional custom mixed-model formula.}
  \item{outcome}{Names of event and total-count columns.}
  \item{weight_a, weight_b, weight_ab}{Predictor names for effect weights.}
  \item{term_a, term_b, term_ab}{Fitted coefficient names used in contrasts.}
  \item{include_interaction}{Whether to include an interaction term.}
  \item{contrasts}{Standard contrasts to report.}
  \item{multiplicity}{None, Holm, or Bonferroni adjustment.}
  \item{multiplicity_family}{Contrasts included in the adjustment family.}
  \item{period, cluster, sequence}{Column names.}
  \item{adjust_sequence}{Whether to include sequence fixed effects.}
  \item{period_effect}{Categorical, linear, or no period effect.}
  \item{link}{Binomial link.}
  \item{nAGQ}{Adaptive quadrature points.}
  \item{alpha}{Two-sided significance level.}
}
\description{
Fits the planned mixed model and returns A, B, combined-treatment, incremental, and interaction Wald contrasts with covariance-correct standard errors. If the data contain an \code{observed} column, rows marked false or zero are removed before outcome validation and fitting.
}
\value{A list containing the fitted model, contrast table, multiplicity results, and fitting diagnostics.}
\examples{
\donttest{
state <- rbind(S1 = c(0, 1, 3, 2), S2 = c(0, 2, 3, 3),
               S3 = c(0, 1, 1, 3), S4 = c(0, 2, 2, 3))
d <- sw_component_design(rep(4, 4), state = state)
a <- sw_component_assumptions(baseline_prob = 0.15,
  treatment_or_a = 1.6, treatment_or_b = 1.5,
  interaction_or = 1.1, icc = 0.05, n_per_cluster_period = 40)
fit_component_model(simulate_component_swcrt(d, a, seed = 1))$contrasts
}
}
