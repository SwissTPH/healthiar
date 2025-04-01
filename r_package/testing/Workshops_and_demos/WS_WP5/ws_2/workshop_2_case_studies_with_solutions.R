# healthiar BEST-COST WORKSHOP (PART 2) ########################################

## SETUP #######################################################################
library(healthiar) ; library(dplyr)
## Set working directory to other than project default
setwd("C:/Users/luytax/switchdrive/BEST-COST/best-cost_WPs/r_package/testing/Workshops_and_demos/WS_WP5/ws_2/")
load(file = "data_clean.RData")

# CASE STUDY 1 - PM2.5 & COPD (CHRONIC OBSTRUCTIVE PULMONARY DISEASE) ##########

## DATA ########################################################################
### Population-weighted mean PM2.5 exposure in Norway: 4.831251 micrograms / m^3
### RR: 1.369 per 10 micrograms / m^3 Pm2.5 increment (95% CI: 1.124 - 1.664)
### Baseline DALYs from COPD in Norway: 448216.7 (95% CI: 401924 – 481141.1)
### Categorical PM2.5 exposure in Norway: variable "data_pm_norway"
### Alt. scenario pop-weighted PM2.5 exposure in Norway: 4 micrograms / m^3
### Approach comparison: delta
### Population-weighted mean PM2.5 exposure in Oslo: 6.59998 4 micrograms / m^3
### Baseline DALYs from COPD in Oslo: 42099.07 (95% CI: 34658.31 – 49513.7)


### Shape ERF: log-linear

## 1.1 #########################################################################
## Attribute DALYs from COPD to pop-weighted mean PM2.5 exposure in Norway using
## RR for increment
pm_copd_pwm <- attribute_health(
  exp_central = 4.831251,
  cutoff_central = 0,
  rr_central = 1.369,
  rr_lower = 1.124,
  rr_upper = 1.664,
  rr_increment = 10,
  erf_shape = "log_linear",
  bhd_central = 448216.70
)
## Attr. DALYs from COPD
pm_copd_pwm$health_main$impact_rounded
# 63104 (95% CI: 24611 - 97753)

## 1.2 #########################################################################
## Attribute DALYs from COPD to PM2.5 categorical exposure in Norway
pm_copd_cat <- attribute_health(
  exp_central = data_pm_norway$exp,
  cutoff_central = 0,
  pop_exp = data_pm_norway$population,
  rr_central = 1.03,
  rr_lower = 1.124,
  rr_upper = 1.664,
  rr_increment = 10,
  erf_shape = "log_linear",
  bhd_central = 448216.70
)
## Attr. DALYs from COPD
pm_copd_cat$health_main$impact_rounded
## 6362 (95% CI: 24710 - 99299)

## 1.3 #########################################################################
## Comparison scenario from exercise 1.1 with an alternative scenario in Norway
## with lower PM2.5 exposure
### Tip: you can use the attribute_mod() function to create an alternative
### scenario based on the existing healthiar output variable "pm_copd_pwm"
pm_copd_alt <- attribute_mod(
  output_attribute_1 = pm_copd_pwm,
  exp_central = 4
)
pm_copd_diff <- compare(
  output_attribute_1 = pm_copd_pwm,
  output_attribute_2 = pm_copd_alt,
  approach_comparison = "delta"
)
## Difference in DALYs from COPD between scenarios
pm_copd_diff$health_main$impact
## 10186.921 (95% CI: 4136.154 - 15153.354)

## 1.4 #########################################################################
## Iteration through whole of Norway and Oslo
pm_iteration <- attribute_health(
  geo_id_disaggregated = c("Norway", "Oslo"),
  exp_central = list(4.831251, 6.59998),
  cutoff_central = 0,
  rr_central = 1.369,
  rr_lower = 1.124,
  rr_upper = 1.664,
  rr_increment = 10,
  erf_shape = "log_linear",
  bhd_central = list(448216.7, 42099.07)
)
## Attr. DALYs from COPD in Norway
pm_iteration$health_main$impact[1:3]
## Attr. DALYs from COPD in Oslo
pm_iteration$health_main$impact[4:6]

## 1.5 (Advanced) ##############################################################
## Monte Carlo (MC) simulation (n=1000) to obtain summary uncertainty (combining
## uncertainty in incremental RR & baseline health data) with pop-weighted mean
## PM exposure in Norway
### Tip: you must specify all the necessary "..._lower" & "..._upper" arguments
pm_copd_mc <- attribute_health(
  exp_central = 4.831251,
  cutoff_central = 0,
  rr_central = 1.369,
  rr_lower = 1.124,
  rr_upper = 1.664,
  rr_increment = 10,
  erf_shape = "log_linear",
  bhd_central = 448216.70,
  bhd_lower = 401924,
  bhd_upper = 481141.1
)
pm_copd_mc <- summarize_uncertainty(
  results = pm_copd_pwm, n_sim = 1000
)
## Summary uncertainty estimates & confidence intervals:
pm_copd_mc$uncertainty_main

## 1.6 (Advanced) ##############################################################
## Attribute DALYs from COPD to categorical PM2.5 exposure in Norway with the
## MR-BRT curve
### Tip: use the "erf_eq_..." function arguments & data from "data_pm_mr_brt"
pm_mr_brt <- attribute_health(
  exp_central = data_pm_norway$exp,
  cutoff_central = 0,
  erf_eq_central = stats::splinefun(
    x = data_pm_mr_brt$exposure, y = data_pm_mr_brt$mean),
  erf_eq_lower = stats::splinefun(
    x = data_pm_mr_brt$exposure, y = data_pm_mr_brt$lower),
  erf_eq_upper = stats::splinefun(
    x = data_pm_mr_brt$exposure, y = data_pm_mr_brt$upper),
  bhd_central = 448216.70
)
## Attr. DALYs from COPD
pm_mr_brt$health_main$impact_rounded

# CASE STUDY 2 - NOISE & HIGH ANNOYANCE (HA) ###################################

## DATA ########################################################################
### Categorical noise exposure: variable "data_noise"
### Exposed population per exposure category: variable "data_noise"
### ERF: number of HA cases = 78.927-3.1162*c+0.0342*c^2
### Disability weight HA: 0.02
### Financial damage of 1 HA case: 350 euro
### Cost of intervention in Oslo to reduce noise exposure 10 million euro

## 2.1 #########################################################################
## Attribute HA cases in Oslo to categorical noise exposure
noise_ha_oslo <- ...

## 2.2 #########################################################################
## Determine YLD due to noise-attributable HA cases in Oslo
noise_ha_yld <- ...

## 2.3 #########################################################################
## Monetize noise-attributable HA cases in Oslo
noise_monetization <- ...

## 2.4 (Advanced) ##############################################################
## Cost-benefit analysis of a noise reduction intervention in Oslo
noise_cba <- ...

## 2.5 (Advanced) ##############################################################
## Attribute HA cases to noise exposure in all parts of Oslo agglomeration
## (iteration) and aggregate the impacts
### Tip: the variable "data_noise" contains the exposure in the different parts
noise_ha_agglo <- ...
