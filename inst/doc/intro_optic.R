## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-----------------------------------------------------

library(optic)

knitr::opts_chunk$set(echo = TRUE)

# this tidy option might be an issue.
knitr::opts_chunk$set(tidy.opts = list(width.cutoff = 60))


## ---- echo = FALSE------------------------------------------------------------
knitr::kable(overdoses[c(1:3, 20:22),], format = "markdown")

## -----------------------------------------------------------------------------

data(overdoses)

# Calculate 5% and 10% changes in mean opioid_death_rate, across states and years. 
five_percent_effect <- 0.05*mean(overdoses$crude.rate, na.rm = T)
ten_percent_effect  <- 0.10*mean(overdoses$crude.rate, na.rm = T)

# Calculate a co-occuring policy effect
cooccur_effect <- -0.02*mean(overdoses$crude.rate, na.rm = T)

# Scenario object for "no confounding" evaluation scenario:
scenarios_no_confounding <- list(five_percent_effect, ten_percent_effect)

# Scenario object for "co-occuring policy" evaluation scenario:
scenarios_co_occur <- list(c(five_percent_effect, cooccur_effect),
                           c(ten_percent_effect, cooccur_effect))

## -----------------------------------------------------------------------------

# Specify 3 models to simulate treatment effects: Linear fixed effect model,
# with and without covariate adjusters, and a linear model using ar-terms
# and no fixed-effects

lm_fe_unadj <- optic_model(
    name = "fixed_effect_linear",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ as.factor(year) + as.factor(state) + treatment_level,
    se_adjust = "cluster-unit"
)

lm_fe_adj <- optic_model(
    name = "fixed_effect_linear_adj",
    type = "reg",
    call = "lm",
    formula = crude.rate ~ unemploymentrate + as.factor(year) + as.factor(state) + treatment_level,
    se_adjust = "cluster-unit"
)
  
lm_ar <- optic_model(
    name = "auto_regressive_linear",
    type = "autoreg",
    call = "lm",
    formula = crude.rate ~ unemploymentrate + as.factor(year) + treatment_change,
    se_adjust = "none"
)

sim_models <- list(lm_fe_unadj, lm_fe_adj, lm_ar)


## -----------------------------------------------------------------------------

sim_config <- optic_simulation(
  
  x                        = overdoses,
  models                   = sim_models,
  iters                    = 10, 
  method                   = "no_confounding",
  unit_var                 = "state",
  treat_var                = "state",
  time_var                 = "year",
  effect_magnitude         = scenarios_no_confounding,
  n_units                  = c(30, 40, 50),
  effect_direction         = c("neg"),
  policy_speed             = c("instant"),
  n_implementation_periods = c(1)

)


## -----------------------------------------------------------------------------

results <- dispatch_simulations(
  
  sim_config,
  use_future = T,
  seed = 9782,
  verbose = 2,
  future.globals=c("cluster_adjust_se"),
  future.packages=c("MASS", "dplyr", "optic")
  
)


## -----------------------------------------------------------------------------

knitr::kable(results[[1]][c(2, 4, 6), 1:9], format = "markdown")


## -----------------------------------------------------------------------------

# Compare point estimates across models for the 5% change scenario, with instantaneous policy adoption:
df_compare <- results[[1]][(results[[1]]$se_adjustment == "cluster-unit")|(results[[1]]$model_name == "auto_regressive_linear"),]

true_est <- -round(five_percent_effect, 3)

grab_mean_and_se <- function(model_name){
  
  sim_mean <- round(mean(df_compare[df_compare$model_name == model_name,]$estimate), 3)
  sim_se   <- round(sd(df_compare[df_compare$model_name == model_name,]$estimate), 3)
  
  res <- paste0(sim_mean, " (", sim_se, ")")
  return(res)
  
}

print(paste0("True effect size: ", true_est))
print(paste0("FE LM effect: ", grab_mean_and_se('fixed_effect_linear')))
print(paste0("FE LM adjusted effect: ", grab_mean_and_se('fixed_effect_linear_adj')))
print(paste0("AR LM effect: ", grab_mean_and_se('auto_regressive_linear')))


