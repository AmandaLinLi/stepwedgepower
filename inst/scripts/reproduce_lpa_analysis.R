# Reproduce the core analysis from the original script ----------------------
# This file is a package-oriented adaptation of the original one-off script.
# Replace the CSV paths below with your local files.

library(stepwedgepower)

# dat0 <- read.csv("data_pseudo_homeDPT.csv")
# datg <- read.csv("PhysiciansListGemini.csv")

# dat <- prepare_physician_data(dat0)
# summarize_by_specialty(dat, vars = c("n_total_pat", "n_ldl_pat"))

# analysis_out <- analyze_lpa_outcomes(dat)

# analysis_out$overall$logit$glm_rates
# analysis_out$overall$logit$glmer_rates
# analysis_out$high_ldl$logit$glm_rates
# analysis_out$high_ldl$logit$glmer_rates

# Example power calculation
# power_out <- estimate_power(
#   n_simulations = 500,
#   effect_size_or = 2.11,
#   n_providers_per_specialty = c(40, 40, 40, 40) * 0.25,
#   tau_provider = 1.21,
#   base_probs = c(0.05, 0.05, 0.05, 0.05),
#   pts_per_step = 50 / 5,
#   seed = 2026
# )
# power_out$power
