# healthiar CASE STUDIES WITH SOLUTIONS#############################################################

# SETUP
## INSTALL & LOAD healthiar: we'll install healthiar together.
library(healthiar)
## LOAD DATA SETS: run the 4 lines below to load the case study data
pm_lc_ch <- pm_lc_ch
pm_lc_cantons <- pm_lc_cantons
noise_ha_ch <- noise_ha_ch
noise_ha_cantons <- noise_ha_cantons

# CASE STUDY 1: AIR POLLUTION (PM2.5) & LUNG CANCER (LC) INCIDENCE IN SWITZERLAND (CH) #############

## CONTEXT
## Currently a reduction of the Swiss air quality guideline limit for PM2.5 from XXX to XXX is disucssed in parliament.
## Implementation costs are estimated at XXX.
## An activist from the lobby group Lung Health Switzerland wants to know how many lung cancer cases
## and direct health care costs could have been avoided by this reduction in 2023 on national level.
## Her lobby group estimates direct health care costs per lung cancer case to be ~ 20'000 CHF per year.
## ADVANCED: Additionally, she would like to know the same numbers in each canton and also in which canton the savings from avoided LC cases would be largest (per 100'000 inhabitants).

## DATA SETS TO BE USED: pm_lc_ch & pm_lc_cantons

## RESEARCH QUESTIONS:
### 1.1 How many LC cases were attributable to PM2.5 exposure in CH in 2023?
### 1.2 How many LC cases attributable to PM2.5 exposure could have been avoided in 2023 with the new limit value?
### 1.3 How many direct lung cancer health care costs could have been avoided in CH with the new limit value?
### ADVANCED: 1.4 How many LC cases were attributable to PM2.5 exposure in in each canton in 2023?
### ADVANCED: 1.5 How many direct lung cancer health care costs could have been avoided in each canton with the new limit value?
### ADVANCED: 1.6 In which canton would the savings from avoided LC cases be largest (per 100'000 inhabitants)?

# ...

# CASE STUDY 2: NOISE & HIGH ANNOYANCE IN SWITZERLAND ##############################################

## CONTEXT
## The interest group Silent Switzerland wants to know how many high annoyance cases can be attributed to noise exposure in Switzerland in 2023, and its burden expressed in years lived with disability (YLD).
## ADVANCED: Additionally, the interest group would like to know the same numbers for each canton and in which three cantons there were the most cases in 2023.

