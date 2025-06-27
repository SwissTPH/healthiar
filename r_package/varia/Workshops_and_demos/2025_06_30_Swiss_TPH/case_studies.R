# healthiar CASE STUDIES WITH SOLUTIONS ########################################

# SETUP ########################################################################
## INSTALL & LOAD healthiar:
### For the installation see the Appendix of the healthiar presentation
library(healthiar)
## INSTALL PACKAGES THAT HEALTHIAR RELIES ON
pkgs <- c("readxl", "dplyr", "tidyr", "purrr", "stringr",
          "tibble", "zoo", "rlang", "devtools", "credentials")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs)
## LOAD DATA SETS: run the 4 lines below to load the case study data
pm_lc_ch <- pm_lc_ch
pm_lc_cantons <- pm_lc_cantons
noise_ha_ch <- noise_ha_ch
noise_ha_cantons <- noise_ha_cantons



# CASE STUDY 1 #################################################################
# AIR POLLUTION (PM2.5) & LUNG CANCER (LC) INCIDENCE IN SWITZERLAND (CH)

## CONTEXT
## Currently a reduction of the Swiss air quality guideline annual mean limit
## for PM2.5 from 10 to 6 is discussed in parliament.
## The lobby group Lung Health Switzerland wants to know how many lung cancer
## cases and direct health care costs could have been avoided by the new limit
## in 2023 on a national level.
## The costs per lung cancer case are estimated at ~ 20'000 CHF per year.
## ADVANCED: Additionally, the group wants to know the number attributable LC
## cases (absolute number of cases and cases per 100'000 inhabitants) in both
## scenarios in each canton.

## DATA SETS TO BE USED: pm_lc_ch & pm_lc_cantons

## RESEARCH QUESTIONS ##########################################################
### 1.1 How many LC cases were attributable to PM2.5 exposure in CH in 2023?
### 1.2 How many LC cases attributable to PM2.5 exposure could have been avoided
### in 2023 with the new limit value?
### 1.3 How many direct lung cancer health care costs could have been avoided in
### CH with the new limit value?
### ADVANCED: 1.4 How many LC cases were attributable to PM2.5 exposure in each
### canton in 2023 (absolute number and rate per 100'000 inhabitants)?
### ADVANCED: 1.5 How many direct lung cancer health care costs could have been
### avoided in each canton with the new limit value?

# ...



# CASE STUDY 2 #################################################################
# NOISE & HIGH ANNOYANCE CASES (HA) IN SWITZERLAND

## DATA SETS TO BE USED: pm_lc_ch & pm_lc_cantons

## CONTEXT
## The interest group Silent Switzerland wants to know how many HA cases can be
## attributed to noise exposure in Switzerland and in each canton in 2023.
## The group also want to know what the attributable impact in years lived with
## disability (YLD; disability weight for HA case: 0.02) associated with the HA
## cases is on a national level.
## ADVANCED: They say that with their new noise initiative Quiet Nights nobody
## would be exposed to the highest noise exposure category. They estimate
## implementation costs for the whole of Switzerland at 1'000'000 CHF and they
## want to know whether it's worth it (yearly cost per HA case: 250 CHF).

## RESEARCH QUESTIONS ##########################################################
### 2.1 How many HA cases were attributable to noise exposure in CH in 2023?
### 2.2 How many HA cases were due to noise exposure in each canton in 2023?
### 2.3 How many YLD from HA cases due to noise were there in CH in 2023?
### ADVANCED: 2.4 Is the Quiet Nights initiative financially advantageous in CH?

# ...
