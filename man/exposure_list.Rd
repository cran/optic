% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exposure-list.R
\name{exposure_list}
\alias{exposure_list}
\title{Applies a time-varying treatment effect}
\usage{
exposure_list(
  sampled_time_period,
  mo,
  available_periods,
  policy_speed,
  n_implementation_periods
)
}
\arguments{
\item{sampled_time_period}{Year that treatment is first enacted}

\item{mo}{Month that treatment is first enacted}

\item{available_periods}{Maximum number of time periods in the data (e.g. if policy is between 1950-2000, then available_periods == 50)}

\item{policy_speed}{A string which is either "instant" for the policy going into immediate effect or "slow"
for the policy effect phasing in linearly across n_implement_periods}

\item{n_implementation_periods}{Number of periods until full treatment effect is applied. Only used if policy_speed is 'slow'.}
}
\value{
A list, containing a vector of policy years of implementation, an integer of the starting policy implementation month,
         and the effect of treatment within a given implementation year (as a fraction of the total policy effect)
}
\description{
Simulates a time-varying treatment effect that starts at zero in time period zero, then linearly increases to a 'full treatment' effect, based on
analyst-provided choices concerning time until full treatment effect and 'speed'
}
\examples{
# Set up a policy that starts in first-year of data, in July and takes 
# 2 years for full implementation:
exposure_list(1, 7, 3, policy_speed = 'slow', n_implementation_periods = 2)

# Same scenario but effect happens instantaneously:
exposure_list(1, 7, 3, policy_speed = 'instant')
}
