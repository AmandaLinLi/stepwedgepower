% Generated manually for version 0.4.0
\name{sw_component_assumptions}
\alias{sw_component_assumptions}
\alias{print.sw_component_assumptions}
\title{Specify Assumptions for a Component-Based Trial}
\usage{
sw_component_assumptions(
  outcome = "binary",
  baseline_prob = NULL,
  baseline_logit = NULL,
  treatment_or_a = NULL,
  treatment_effect_a = NULL,
  treatment_or_b = NULL,
  treatment_effect_b = NULL,
  interaction_or = NULL,
  interaction_effect = NULL,
  delay_a = 0L,
  delay_b = 0L,
  carryover_periods_a = 0L,
  carryover_periods_b = 0L,
  carryover_weights_a = NULL,
  carryover_weights_b = NULL,
  restart_rule_a = c("reset", "resume"),
  restart_rule_b = c("reset", "resume"),
  interaction_mode = c("effective_overlap", "assigned_overlap", "none"),
  icc = NULL,
  cluster_sd = NULL,
  period_effects = NULL,
  n_per_cluster_period = 20
)

\method{print}{sw_component_assumptions}(x, ...)
}
\arguments{
  \item{outcome}{Currently only \code{"binary"}.}
  \item{baseline_prob, baseline_logit}{Supply exactly one baseline probability or baseline logit specification.}
  \item{treatment_or_a, treatment_effect_a}{A main effect as an odds ratio or log-odds coefficient.}
  \item{treatment_or_b, treatment_effect_b}{B main effect.}
  \item{interaction_or, interaction_effect}{A-by-B interaction as an odds ratio or log-odds coefficient.}
  \item{delay_a, delay_b}{Non-negative wash-in periods.}
  \item{carryover_periods_a, carryover_periods_b}{Periods of residual effect after withdrawal.}
  \item{carryover_weights_a, carryover_weights_b}{Non-increasing weights in [0, 1], one per carryover period or one repeated value.}
  \item{restart_rule_a, restart_rule_b}{Whether a restarted component resets or resumes its wash-in clock.}
  \item{interaction_mode}{Whether interaction follows effective overlap, current assigned overlap, or is suppressed.}
  \item{icc, cluster_sd}{Supply one latent-scale ICC or cluster random-intercept standard deviation.}
  \item{period_effects}{Optional secular log-odds effects.}
  \item{n_per_cluster_period}{Cluster-period sample size specification.}
  \item{x}{An \code{sw_component_assumptions} object.}
  \item{...}{Unused.}
}
\description{
Defines component main effects, interaction, wash-in, withdrawal carryover, restart rules, correlation, secular trend, and sample size for the four-state engine.
}
\value{An object of class \code{sw_component_assumptions}.}
\examples{
a <- sw_component_assumptions(
  baseline_prob = 0.15,
  treatment_or_a = 1.35,
  treatment_or_b = 1.25,
  interaction_or = 1.10,
  carryover_periods_a = 2,
  carryover_weights_a = c(0.5, 0.25),
  icc = 0.05
)
a
}
