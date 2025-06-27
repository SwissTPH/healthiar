# healthiar CASE STUDIES WITH SOLUTIONS ########################################

# SETUP ########################################################################
## INSTALL & LOAD healthiar:
### For the installation see Appendix of the healthiar presentation.
library(healthiar)
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

## 1.1 #########################################################################
results_1.1 <- attribute_health(
  erf_shape = "log_linear",
  rr_central = 1.16,
  rr_lower = 1.1,
  rr_upper = 1.23,
  rr_increment = 10,
  exp_central = 7.538,
  cutoff_central = 5,
  bhd_central = 14992
)
## Attributable LC cases
results_1.1$health_main$impact_rounded ## 554 (358 - 767)

## 1.2 #########################################################################
results_interim_1.2 <- attribute_health(
  erf_shape = "log_linear",
  rr_central = 1.16,
  rr_lower = 1.1,
  rr_upper = 1.23,
  rr_increment = 10,
  exp_central = 6,
  cutoff_central = 5,
  bhd_central = 14992
)
## Attributable LC cases with new limit
results_interim_1.2$health_main$impact_rounded ## 221 (142 - 307)
## Difference
results_1.2 <- compare(
  output_attribute_1 = results_1.1,
  output_attribute_2 = results_interim_1.2
)
results_1.2$health_main$impact_rounded # 333 (216 - 460)

## 1.3 #########################################################################
results_1.3 <- monetize(
  output_attribute = results_1.2,
  valuation = 20000
)
# Avoided direct health case costs (in CHF)
results_1.3$monetization_main$monetized_impact # 6'667'226 (4321820 - 9203650)

## 1.4 #########################################################################
results_1.4 <- attribute_health(
  geo_id_disaggregated = pm_lc_cantons$canton,
  erf_shape = "log_linear",
  rr_central = 1.16,
  rr_lower = 1.1,
  rr_upper = 1.23,
  rr_increment = 10,
  exp_central = pm_lc_cantons$exposure,
  cutoff_central = 5,
  bhd_central = pm_lc_cantons$lung_cancer_incidence,
  population = pm_lc_cantons$population
)
## Attributable LC cases in each canton
### Absolute number
results_1.4$health_main |>
  dplyr::filter(erf_ci == "central") |>
  dplyr::select(geo_id_disaggregated, impact_rounded) |>
  dplyr::arrange(desc(impact_rounded))
### Rate per 100'000 inhabitants
results_1.4$health_main |>
  dplyr::filter(erf_ci == "central") |>
  dplyr::select(geo_id_disaggregated, impact_per_100k_inhab) |>
  dplyr::arrange(dplyr::desc(impact_per_100k_inhab))

## 1.5 #########################################################################
results_1.5 <- attribute_health(
  geo_id_disaggregated = pm_lc_cantons$canton,
  erf_shape = "log_linear",
  rr_central = 1.16,
  rr_lower = 1.1,
  rr_upper = 1.23,
  rr_increment = 10,
  exp_central = rep(6, times = nrow(pm_lc_cantons)),
  cutoff_central = 5,
  bhd_central = pm_lc_cantons$lung_cancer_incidence,
  population = pm_lc_cantons$population
)
## Attributable LC cases in each canton with new limit value
results_1.5$health_main |>
  dplyr::filter(erf_ci == "central") |>
  dplyr::select(geo_id_disaggregated, impact_rounded) |>
  dplyr::arrange(desc(impact_rounded))



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
results_2.2$health_detailed$impact_raw |>
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

## 2.4 #########################################################################
results_interim_2.4 <- monetize(
  output_attribute = results_2.1,
  valuation = 250
)
## Benefit of implementing Quiet Nights initiative
results_interim_2.4 <- results_interim_2.4$monetization_detailed$health_raw |>
  dplyr::filter(exposure_dimension == 5) |>
  dplyr::select(monetized_impact_rounded) |>
  dplyr::pull()
results_interim_2.4 # 1'417'311 CHF
## Net benefit of implementing the Quiet Nights initiative
results_interim_2.4 - 1000000 # 417'311 CHF
## The initiative is financially advantageous.
