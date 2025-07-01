# healthiar CASE STUDIES WITH SOLUTIONS ########################################

# SETUP ########################################################################
## INSTALL healthiar:
### 1. Go to the TRANS folder "Axel Luyten" and copy "healthiar_0.0.2.861.zip"
### to a local folder
### 2. In RStudio, go to the "Packages" tab, click "Install", and choose "Package Archive File (.zip; .tar.gz)" ### at the upper drop-down menu
### 3. In the Windows explorer window that pops up go to the local folder from step 1 and click on the ### "healthiar_0.0.2.861.zip" file
### 4. Click on "Install" (leave the Install Dependencies box ticked). Done!
### LOAD healthiar:
library(healthiar)
## INSTALL PACKAGES THAT HEALTHIAR RELIES ON:
pkgs <- c("readxl", "dplyr", "tidyr", "purrr", "stringr",
          "tibble", "zoo", "rlang", "devtools", "credentials")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs)) install.packages(new_pkgs)
rm(pkgs, new_pkgs)
## LOAD DATA SETS: run the 4 lines below to load the case study data
pm_lc_ch <- pm_lc_ch
pm_lc_cantons <- pm_lc_cantons
noise_ha_ch <- noise_ha_ch
noise_ha_cantons <- noise_ha_cantons



# CASE STUDY 1 #################################################################
# AIR POLLUTION (PM2.5) & LUNG CANCER (LC) INCIDENCE IN SWITZERLAND (CH)

## CONTEXT
## A new Swiss air quality guideline annual mean limit of 6 μg/m^3 for PM2.5 is
## discussed in parliament. The lobby group Lung Health Switzerland wants to
## know how many lung cancer cases could have been avoided in Switzerland if the
## PM2.5 population-weighted mean concentration would have been at 6 μg/m^3
## (i.e. the new limit value) in 2023 instead of the actual level of 7.538.
## ADVANCED: Additionally, the group wants to know the number attributable LC
## cases (absolute number of cases and cases per 100'000 inhabitants) in both
## scenarios in each canton.

## DATA SETS TO BE USED: pm_lc_ch & pm_lc_cantons

## RESEARCH QUESTIONS ##########################################################
### 1.1 How many LC cases were attributable to PM2.5 exposure in CH in 2023?
### 1.2 How many LC cases attributable to PM2.5 exposure could have been avoided
### in 2023 with the new limit value?
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
## The group also wants to know what that attributable impact measured in years
## lived with disability (YLD) was in Switzerland, using a disability weight of
## 0.02 for HA cases.

## RESEARCH QUESTIONS ##########################################################
### 2.1 How many HA cases were attributable to noise exposure in CH in 2023?
### 2.2 How many HA cases were due to noise exposure in each canton in 2023?
### 2.3 How many YLD from HA cases due to noise were there in CH in 2023?

## 2.1 #########################################################################
results_2.1 <- attribute_health(
  approach_risk = "absolute_risk",
  erf_eq_central = noise_ha_ch$formula,
  exp_central = noise_ha_ch$exposure_level,
  pop_exp = noise_ha_ch$population_exposed,
)
## Attributable HA cases in CH
results_2.1$health_main$impact # 299485

## 2.2 #########################################################################
results_2.2 <- attribute_health(
  geo_id_disaggregated = noise_ha_cantons$canton,
  approach_risk = "absolute_risk",
  erf_eq_central = noise_ha_cantons$formula,
  exp_central = noise_ha_cantons$exposure_level,
  pop_exp = noise_ha_cantons$population_exposed
)
## Attributable HA cases in each canton
results_2.2$health_detailed$results_raw |>
  dplyr::group_by(geo_id_disaggregated) |>
  dplyr::summarize(impact = sum(impact)) |>
  dplyr::arrange(dplyr::desc(impact))

## 2.3 #########################################################################
results_2.3 <- attribute_health(
  approach_risk = "absolute_risk",
  erf_eq_central = noise_ha_ch$formula,
  exp_central = noise_ha_ch$exposure_level,
  pop_exp = noise_ha_ch$population_exposed,
  dw_central = 0.02
)
## Attributable YLD in CH
results_2.3$health_main$impact_rounded # 5990 YLD
