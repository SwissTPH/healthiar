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
### Cutoff: 0 micrograms / m^3
### Categorical PM2.5 exposure in Norway: variable "data_pm_norway"
### Alt. scenario pop-weighted PM2.5 exposure in Norway: 4 micrograms / m^3
### Population-weighted mean PM2.5 exposure in Oslo: 6.59998 4 micrograms / m^3
### Approach comparison: delta
### Baseline DALYs from COPD: 448216.7 DALYs (95% CI: 401924 – 481141.1))
### Shape ERF: log-linear

## 1.1 #########################################################################
## Attribute DALYs from COPD to pop-weighted mean PM2.5 exposure in Norway using
## RR for increment
pm_copd_pwm <- ...

## 1.2 #########################################################################
## Attribute DALYs from COPD to PM2.5 categorical exposure in Norway
pm_copd_cat <- ...

## 1.3 #########################################################################
## Comparison scenario from exercise 1.1 with an alternative scenario in Norway
## with lower PM2.5 exposure
### Tip: you can use the attribute_mod() function to create an alternative
### scenario based on the existing healthiar output variable "pm_copd_pwm"
pm_copd_alt <- ...

pm_copd_diff <- ...

## 1.4 #########################################################################
## Iteration through whole of Norway and Oslo
pm_iteration <- ...

## 1.5 (Advanced) ##############################################################
## Monte Carlo (MC) simulation to obtain summary uncertainty (combining
## uncertainty in incremental RR & baseline health data) with pop-weighted mean
## PM exposure in Norway
### Tip: you must specify all the necessary "..._lower" & "..._upper" arguments
pm_copd_pwm_mc <- ...

## 1.6 (Advanced) ##############################################################
## Attribute DALYs from COPD to categorical PM2.5 exposure in Norway with the
## MR-BRT curve
### Tip: use the "erf_eq_..." function arguments & data from  "data_pm_mr_brt"
pm_mr_brt <- ...

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

