% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optic-model-class.R
\name{optic_model}
\alias{optic_model}
\title{Optic Model}
\usage{
optic_model(name, type, call, formula, se_adjust, ...)
}
\arguments{
\item{name}{Name of the model object, used to identify the model when reviewing simulation results}

\item{type}{Estimator used to identify the treatment effect using simulated data. Specified as a string, which can either be 'reg' (regression), 'autoreg' (autoregression, which adds a lag for the outcome variable to a regression model), 'drdid' (doubly-robust difference-in-difference estimator), or 'multisynth' (augmented synthetic control)}

\item{call}{String which specifies the R function to call for applying the estimator. Package currently supports  either 'lm' (linear model), 'feols' (fixed-effect OLS), 'multisynth' (pooled synthetic controls), or 'glm.nb' (negative-binomial generalized nearlized linear model)}

\item{formula}{Model specification, using R formula formatting. Must include a variable labeled 'treatment' for the 'nonconf' & 'selbias' simulation method or variables labeled 'treatment1' & 'treatment2' for the simulation method 'concurrent'}

\item{se_adjust}{Adjustments applied to standard errors following model estimation. Specified as a string, OPTIC currently support 'none' for no adjustment or 'cluster' for clustered standard errors. Clustered standard errors will use the 'unit_var' specified in optic_simulation for determining unit used for clustering standard errors.}

\item{...}{Additional arguments that are passed to the model call. Please refer to documentation for each model call for additional details. If the model call expects a name, you may need to pass your parameter using param = as.name("variable_name") as opposed to param = variable_name.}
}
\value{
optic_model An optic_model object to be used as an input within optic_simulations. Details model calls and parameters.
}
\description{
Generates model object to apply to each simulated dataset
}
\examples{

# Set up a simple linear model
form <- formula(crude.rate ~ state + year + population + treatment_level)
mod <- optic_model(name = 'lin', 
                   type = 'reg', 
                   call = 'lm', 
                   formula = form, 
                   se_adjust = 'none')

# Deploy an auto-regressive model.
# type = "autoreg" will make AR term 
# automatically when the model is deployed; also note
# in formula the use of "treatment_change" as the treatment variable 
# rather than "treatment_level" like in the previous example:

form_ar <- formula(crude.rate ~ state + year + population + treatment_change)
mod_ar <- optic_model(name = "auto_regressive_linear", 
                      type = "autoreg", 
                      call = "lm", 
                      formula = form_ar,
                      se_adjust = "none")

# One could also use a different call, assuming the right packages 
# are installed and the model uses a familiar formula framework. 
# Example with random intercept for states, using lme4 package.

form_me <- formula(crude.rate ~ 
                   population + year + treatment_level + (1|state))
                   
mod_me <- optic_model(name = "mixed_effect", 
                      type = "reg", 
                      call = "lmer", 
                      formula = form_me, 
                      se_adjust = "none")

}
