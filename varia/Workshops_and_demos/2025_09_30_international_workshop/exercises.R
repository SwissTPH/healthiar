# Setup ########################################################################
install.packages("remotes")
library(remotes)
remotes::install_github(repo = "SwissTPH/healthiar", build_vignettes = TRUE)
library(healthiar)

# Relative risk 1 - single exposure value ######################################

## Specify the exposure (8.85) and determine the attributable COPD cases
results_rr_1 <- attribute_health(
  exp_central = , # μg / m^3
  erf_shape = "log_linear", # erf = exposure-response function
  rr_central = 1.369,
  rr_increment = 10,  # μg / m^3
  cutoff_central = 5, # μg / m^3
  bhd_central = 30747 # bhd = baseline health data (here: COPD incidence)
)

## Attributable cases: ...

# Relative risk 2 - single exposure value
results_rr_2

# Comparison
results_comparison

# Multiple geo units 1 - single level
results_geo_1

# Multiple geo units 2 - multiple levels
results_geo_2

# Relative risk 3 - exposure distribution
results_rr_3

# Absolute risk
results_ar
