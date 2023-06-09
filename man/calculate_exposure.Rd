% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate-exposure.r
\name{calculate_exposure}
\alias{calculate_exposure}
\title{Calculates the exposure rate applied to each year provided month
of policy implementation and number of years to full implementation}
\usage{
calculate_exposure(month, n_years, monthly_effect = (1/n_years)/12)
}
\arguments{
\item{month}{month of year (as integer) that policy takes effect}

\item{n_years}{number of months until full implementation in effect}

\item{monthly_effect}{increment of exposure to apply each month; default is
((1/n_years) / 12) (constant over the period)}
}
\value{
A vector of percentages, indicating change in exposure by year (relative to start month)
}
\description{
Calculates the exposure rate applied to each year provided month
of policy implementation and number of years to full implementation
}
\examples{

# Calculate uniform increase in policy effect which ramps up across 10 years

# Assume policy starts in July of the first year, then continues for 10 years
starting_month <- 7
implementation_years <- 10

# Assume some policy effect (which is the target effect for simulations)
policy_effect <- 2

exposure_by_year <- calculate_exposure(starting_month, implementation_years)

# Based on exposure by year, calculate policy effect by year:
plot(policy_effect*exposure_by_year)

}
