#### Lp(a) Grant Proposal: Analysis of data and power calculations


### 1. Gemini list of physicians in four specialties
datg = read.csv("PhysiciansListGemini.csv")
table(datg$Specialty) # Cardiology 44, FamMed 43, GenIntMed 50, Neurol 38

### 1. Analysis of data on physicians and Lp(a) testing
dat0 = read.csv("data_pseudo_homeDPT.csv")
dat0$X=NULL

## Keep physicians in 4 specialties of interest: Cardiology, FamMed, GenIntMed, Neurology
dat = dat0[dat0$specialty %in% c("CARDIOLOGY", "FAMILY MEDICINE", "INTERNAL MEDICINE", "NEUROLOGY"),]

## Sort physicians by specialty and number of patients (n_total_pat)
dat = dat[order(dat$specialty, dat$n_total_pat, decreasing=TRUE),]

## Summarize number of patients per physician
tapply(dat$n_total_pat, dat$specialty, summary) # low median for all but neurology

## Keep physicians with at least 100 patients
dat = dat[dat$n_total_pat >= 100,]

## Remove Amy who saw 60K+ patients
dat = dat[dat$n_total_pat <10000,]

table(dat$specialty)
# CARDIOLOGY   FAMILY MEDICINE INTERNAL MEDICINE         NEUROLOGY 
# 66               100               148                69 
dat = dat[order(dat$specialty, dat$PROV_NAME),]
## pretty good overlap with the datg list of names!

## Summarize number of patients
tapply(dat$n_total_pat, dat$specialty, summary)
tapply(dat$n_total_pat, dat$specialty, median)
# CARDIOLOGY   FAMILY MEDICINE INTERNAL MEDICINE         NEUROLOGY 
# 887               663               366               423 
## average of medians = 550

tapply(dat$n_ldl_pat, dat$specialty, summary)
tapply(dat$n_ldl_pat, dat$specialty, median, na.rm=TRUE)
# CARDIOLOGY   FAMILY MEDICINE INTERNAL MEDICINE         NEUROLOGY 
# 244.5             287.5                141.0              85.0 
## average of medians = 550


## Fit model for probability of Lp(a) testing by department
## with and without accounting for within-physician correlation

fit0 = glm(n_lpa_pat/n_total_pat ~ specialty, weights=n_total_pat, family=binomial, data=dat)
summary(fit0)
(beta = c(coefficients(summary(fit0))[,1]))
(logodds = beta[1]+c(0, beta[-1]))
names(logodds) = c("Cardiol","FamMed", "GenIntMed", "Neurol")
prob = exp(logodds)/(1+exp(logodds))
round(prob, 3)
# Cardiol    FamMed GenIntMed    Neurol 
# 0.071     0.024     0.037     0.021 


fit0id = glm(n_lpa_pat/n_total_pat ~ specialty, weights=n_total_pat, 
             family=binomial(link="identity"), data=dat)
summary(fit0id)
(beta = c(coefficients(summary(fit0id))[,1]))
(logodds = beta[1]+c(0, beta[-1]))
names(logodds) = c("Cardiol","FamMed", "GenIntMed", "Neurol")
prob = exp(logodds)/(1+exp(logodds))
round(prob, 3)
# Cardiol    FamMed GenIntMed    Neurol 
# 0.071     0.024     0.037     0.021 

library(lme4)
fit = glmer(n_lpa_pat/n_total_pat ~ specialty + (1|prov_id), weights=n_total_pat, family=binomial, data=dat, nAGQ=10)
summary(fit)
reff.var = 1.466   # reff var=1.466, reff SD=1.211
# marginal coefficients: betaM = betaC /sqrt(1+0.346*reff.var)
(betaC = c(coefficients(summary(fit))[,1]))
(betaM = betaC/sqrt(1+0.346*reff.var))
(logoddsM = betaM[1]+c(0, betaM[-1]))
names(logoddsM) = c("Cardiol","FamMed", "GenIntMed", "Neurol")
probM = exp(logoddsM)/(1+exp(logoddsM))
round(probM, 3)
# Cardiol    FamMed GenIntMed    Neurol 
# 0.056     0.029     0.039     0.015 


fit.id = glmer(n_lpa_pat/n_total_pat ~ specialty + (1|prov_id), 
               weights=n_total_pat, 
               family=binomial(link="identity"), 
               data=dat, nAGQ=10)
summary(fit.id)
reff.var = 0.002318   # reff var=0.002318, reff SD=0.04814
# marginal coefficients: betaM = betaC /sqrt(1+0.346*reff.var)
(betaC = c(coefficients(summary(fit.id))[,1]))
(betaM = betaC)
(probM = betaM[1]+c(0, betaM[-1]))
names(probM) = c("Cardiol","FamMed", "GenIntMed", "Neurol")
round(probM, 3)
# Cardiol    FamMed GenIntMed    Neurol 
# 0.065     0.023     0.031     0.035

## Fit model for probability of Lp(a) testing among those with high LDL by department
## with and without accounting for within-physician correlation

fit0ldl = glm(n_ldl_lpa_pat/n_ldl_pat ~ specialty, weights=n_ldl_pat, family=binomial, data=dat)
summary(fit0ldl)
(beta = c(coefficients(summary(fit0ldl))[,1]))
(logodds = beta[1]+c(0, beta[-1]))
names(logodds) = c("Cardiol","FamMed", "GenIntMed", "Neurol")
prob = exp(logodds)/(1+exp(logodds))
round(prob, 3)
# Cardiol    FamMed GenIntMed    Neurol 
# 0.106     0.035     0.058     0.080 


fitldl = glmer(n_ldl_lpa_pat/n_ldl_pat ~ specialty + (1|prov_id), weights=n_ldl_pat, family=binomial, data=dat, nAGQ=10)
summary(fitldl)
reff.var = 0.9732   # reff var=0.973, reff SD=0.987
# marginal coefficients: betaM = betaC /sqrt(1+0.346*reff.var)
(betaC = c(coefficients(summary(fitldl))[,1]))
(betaM = betaC/sqrt(1+0.346*reff.var))
(logoddsM = betaM[1]+c(0, betaM[-1]))
names(logoddsM) = c("Cardiol","FamMed", "GenIntMed", "Neurol")
probM = exp(logoddsM)/(1+exp(logoddsM))
round(probM, 3)
# Cardiol    FamMed GenIntMed    Neurol 
# 0.080     0.039     0.054     0.060 

#### Power calculations: assume total pool npool=c(40,40,40,40) physicians in the 4 specialties
## Proportion prec are being recruited in the study
## nphys = round(npool*prec)
## npat = nr patients seen in each specialty in each phase/step

### Version 1: ignore within-physician correlation
## 5 phases (0/1/2/3/4 specialties receive treatment)
## Total number of patients seen: nphys*npat*4*5  # 4 specialties, 5 phases
## Half of the patients are seen under treatment, half under control
## Determine power of treatment effect = doubling of odds, or similar
## Use WebPower:wp.logistic

# Assume each physician sees about 100/200/300/400 different patients in a year (duration of study)
# npool=40 physicians per specialty

library("WebPower")
wp.logistic(n=4*40*100*c(1, 0.75, 0.50, 0.25),
            p0 = 0.05, p1=0.10, family="Bernoulli") # power>0.999 in all cases



## Version 2: take into account within-physician correlation

## Use simulation program of Amanda
## Requires dplyr
library(dplyr)
library(tidyr)
run_one_simulation <- function(effect_size_OR = 1.5, 
                               n_providers_per_specialty = c(40, 40, 40, 40), # rounded off from Gemini search 
                               # Order: Cardiol, Internal Med, Family Med, Neurol) 
                               specialty_names = c("Cardiol", "IntMed", "FamMed", "Neurol"),
                               tau_provider = 1.21,  # SD of provider reff in glmer model 
                               base_probs = c(0.06, 0.04, 0.03, 0.02), # Baseline Rates (from analysis)
                               # Order: Cardio, Internal Med, Family Med, Neuro) 
                               pts_per_step = 100/5, # Patients per Provider per step/phase of study
                               n_steps = 5, # Stepped Wedge Design with n_steps=5 steps
                               link = "logit"
) {
 
  # A. Define Real Group Sizes (4 Specialties)
  
  n_providers <- n_providers_per_specialty  
  specialty_names <- specialty_names
  
  # B. Define Baseline Rates (from my table)
  
  base_probs <- base_probs
  
  base_intercepts <- qlogis(base_probs) # Convert to log-odds
  
  # C. Simulation Parameters
  pts_per_step <- pts_per_step  # Patients per Provider per Visit (fixed)
  n_steps <- n_steps  # Total Visits (Stepped Wedge Design)
  
  # Treatment Effect (Log-Odds Scale)
  beta_treat <- log(effect_size_OR) 
  
  
  tau_provider <- tau_provider 
  
  glmer.link = link
  # GENERATE DATA
  
  # Create Provider List (Total n = sum(n_providers))
  total_prov <- sum(n_providers)
  prov_df <- data.frame(
    PID = 1:total_prov,
    specialty_idx = rep(1:length(n_providers), times = n_providers)
  )
  
  # Assign Random Intercepts
  prov_df$b_i <- rnorm(total_prov, 0, tau_provider)
  
  # Expand to Longitudinal (Provider x Visit)
  sim_data <- expand_grid(
    PID = prov_df$PID,
    visit = 1:n_steps
  ) |>
    left_join(prov_df, by = "PID")
  
  # Assign Treatment (Stepped Wedge Logic for 4 Groups)
  # V1: Baseline (All OFF)
  # V2: Cardio ON
  # V3: Cardio + IntMed ON
  # V4: Cardio + IntMed + FamMed ON
  # V5: All 4 ON
  
  sim_data <- sim_data |>
    mutate(
      treat = case_when(
        visit == 1 ~ 0,
        visit == 2 & specialty_idx <= 1 ~ 1,
        visit == 3 & specialty_idx <= 2 ~ 1,
        visit == 4 & specialty_idx <= 3 ~ 1,
        visit == 5 ~ 1, # All 4 groups are on
        TRUE ~ 0
      )
    )
  
  # Calculate True Probability
  sim_data <- sim_data |>
    mutate(
      base_logit = base_intercepts[specialty_idx],
      true_logit = base_logit + (beta_treat * treat) + b_i,
      true_prob = plogis(true_logit)
    )
  
  # Simulate Outcome (Aggregated Binomial)
  sim_data$n_patients <- pts_per_step
  sim_data$n_positive <- rbinom(nrow(sim_data), size = pts_per_step, prob = sim_data$true_prob)
  
  
  # FIT MODEL & EXTRACT P-VALUE
  
  fit <- tryCatch({
    glmer(cbind(n_positive, n_patients - n_positive) ~ treat + factor(visit) + factor(specialty_idx) + (1|PID),
          family = binomial(link=glmer.link),
          data = sim_data)
  }, error = function(e) NULL)
  
  if(is.null(fit)) return(NA)
  
  # Extract P-value for Treatment
  coefs <- summary(fit)$coefficients
  if("treat" %in% rownames(coefs)) {
    return(coefs["treat", "Pr(>|z|)"])
  } else {
    return(NA)
  }
}

# ---------------------------------------------------------
# 2. RUN POWER CALCULATION LOOP
# ---------------------------------------------------------
target_OR <- 2 # Detecting a 100% increase, at physician level

n_simulations <- 100 
p_values <- replicate(n_simulations, 
              run_one_simulation(effect_size_OR=2.11,
                                 n_providers_per_specialty=c(40,40,40,40)*0.25,
                                 tau_provider=1.21,
                                 base_probs=c(0.05,0.05,0.05,0.05),
                                 pts_per_step=50/5))
(power <- mean(p_values < 0.05, na.rm = TRUE))

# With 10 patients/step (50 patients total per provider) and 10 providers per specialty
# we have 82% power


n_simulations <- 100 
p_values <- replicate(n_simulations, 
                      run_one_simulation(effect_size_OR=2.11,
                                         n_providers_per_specialty=c(40,40,40,40)*0.25,
                                         tau_provider=1.21,
                                         base_probs=c(0.03,0.03,0.03,0.03),
                                         pts_per_step=50/5))
(power <- mean(p_values < 0.05, na.rm = TRUE))


n_simulations <- 100 
p_values <- replicate(n_simulations, 
                      run_one_simulation(effect_size_OR=2.81, # from p=0.03 to p=0.08
                                         n_providers_per_specialty=c(40,40,40,40)*0.25,
                                         tau_provider=1.21,
                                         base_probs=c(0.03,0.03,0.03,0.03),
                                         pts_per_step=50/5))
(power <- mean(p_values < 0.05, na.rm = TRUE))



p_values <- replicate(n_simulations, 
                      run_one_simulation(effect_size_OR=2.34, # marginal OR=2
                                         n_providers_per_specialty=c(40,40,40,40)*0.25,
                                         tau_provider=1.21,
                                         base_probs=c(0.07, 0.04, 0.03, 0.02),
                                         pts_per_step=100/5))
(power <- mean(p_values < 0.05, na.rm = TRUE))


p_values <- replicate(n_simulations, 
                      run_one_simulation(effect_size_OR=exp(log(1.7)*sqrt(1+0.346*1.21^2)),
                                         n_providers_per_specialty=c(40,40,40,40)*0.25,
                                         tau_provider=1.21,
                                         base_probs=c(0.07, 0.04, 0.03, 0.02),
                                         pts_per_step=100/5))
(power <- mean(p_values < 0.05, na.rm = TRUE))


## Power to increase by 2% or more for departments whose rates are at 1% or more
p_values <- replicate(n_simulations, 
                      run_one_simulation(effect_size_OR=exp(log(2.5)*sqrt(1+0.346*1.21^2)), # marginal OR=3
                                         n_providers_per_specialty=c(40,40,40,40)*0.25,
                                         tau_provider=1.21,
                                         base_probs=c(0.01, 0.01, 0.01, 0.01),
                                         pts_per_step=100/5))
(power <- mean(p_values < 0.05, na.rm = TRUE))


# ---------------------------------------------------------
# 3.Calculate both Type I Error for a given scenario
# ---------------------------------------------------------


n_simulations <- 100
type1_error_pvalues <- replicate(n_simulations, 
                      run_one_simulation(effect_size_OR = 1, # no effect
                                         n_providers_per_specialty=c(40,40,40,40)*0.25,
                                         tau_provider=1.21,
                                         base_probs=c(0.07, 0.04, 0.03, 0.02),
                                         pts_per_step=100/5))

type1_error <- mean(type1_error_pvalues < 0.05, na.rm = TRUE)
print(type1_error)




