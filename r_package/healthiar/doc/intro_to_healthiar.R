## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)
options(knitr.kable.max_rows = 10)
set.seed(1)

## Avoid using pacman here, as it causes error in installation if it's not installed already
library(healthiar)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(knitr)
# library(pillar) # Used in some code that's currently commented out

## -----------------------------------------------------------------------------
results_pm_copd <- attribute_health(
  erf_shape = "log_linear", # shape of the exposure-response function (ERF)
  rr_central = 1.369, # relative risk (RR) central estimate
  rr_increment = 10,  # increment for which relative risk is valid (in μg / m^3)
  exp_central = 8.85, # PM2.5 exposure (in μg / m^3) (here: population-weighted)
  cutoff_central = 5, # cutoff (in μg / m^3) below which no health effects occur 
  bhd_central = 30747 # baseline health data (BHD; here: COPD incidence)
)

## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
results_pm_copd <- attribute_health(
  erf_shape = "log_linear",
  rr_central = exdat_pm_copd$relative_risk, 
  rr_increment = 10, 
  exp_central = exdat_pm_copd$mean_concentration,
  cutoff_central = exdat_pm_copd$cut_off_value,
  bhd_central = exdat_pm_copd$incidence
)

## ----include=TRUE, eval=TRUE, echo=TRUE---------------------------------------
results_pm_copd$health_main

## -----------------------------------------------------------------------------
results_pm_copd$health_main |> 
  select(exp, bhd, rr, erf_ci, pop_fraction, impact_rounded) |> 
  knitr::kable() # For formatting reasons only: prints tibble in nice layout

## -----------------------------------------------------------------------------
results_pm_copd <- attribute_health(
    erf_shape = "log_linear",
    rr_central = 1.369, 
    rr_lower = 1.124, # lower 95% confidence interval (CI) bound of RR
    rr_upper = 1.664, # upper 95% CI bound of RR
    rr_increment = 10, 
    exp_central = 8.85, 
    exp_lower = 8, # lower 95% CI bound of exposure
    exp_upper = 10, # upper 95% CI bound of exposure
    cutoff_central = 5,
    bhd_central = 30747, 
    bhd_lower = 28000, # lower 95% confidence interval estimate of BHD
    bhd_upper = 32000 # upper 95% confidence interval estimate of BHD
) 

## ----eval=FALSE, echo=TRUE, include=FALSE-------------------------------------
# results_pm_copd$health_detailed$results_raw

## ----echo=FALSE---------------------------------------------------------------
results_pm_copd$health_detailed$results_raw |> 
  dplyr::select(erf_ci, exp_ci, bhd_ci, impact_rounded) |> 
  dplyr::slice(1:9) |> 
  knitr::kable() # Prints tibble in a minimal layout

## -----------------------------------------------------------------------------
results_noise_ha <- attribute_health(
  approach_risk = "absolute_risk", # default is "relative_risk"
  exp_central = c(57.5, 62.5, 67.5, 72.5, 77.5), # mean of the exposure categories
  pop_exp = c(387500, 286000, 191800, 72200, 7700), # population exposed per exposure category
  erf_eq_central = "78.9270-3.1162*c+0.0342*c^2" # exposure-response function
)

## ----echo=FALSE---------------------------------------------------------------
results_noise_ha |> 
  pluck("health_main") |>
  select(erf_eq, erf_ci, impact_rounded) |> 
  knitr::kable() # Prints tibble in a minimal layout

## ----echo=TRUE, eval=FALSE, include=TRUE--------------------------------------
# results_noise_ha$health_detailed$results_raw

## ----echo=FALSE, eval=TRUE, include=TRUE--------------------------------------
results_noise_ha[["health_detailed"]][["results_raw"]] |> 
  select(exposure_dimension, exp, pop_exp, impact) |> 
  knitr::kable()

## -----------------------------------------------------------------------------
results_noise_ha <- attribute_health(
  approach_risk = "absolute_risk",
  exp_central = 57.5,
  pop_exp = 387500,
  erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
)

## ----echo=TRUE, eval=FALSE, include=TRUE--------------------------------------
# results_noise_ha$health_main

## ----echo=FALSE, eval=TRUE, include=TRUE--------------------------------------
results_noise_ha[["health_main"]] |> 
  dplyr::select(exposure_dimension, impact) |> 
  knitr::kable()

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_noise_ha)

## -----------------------------------------------------------------------------
results_iteration <- attribute_health(
    geo_id_disaggregated = c("Zurich", "Basel", "Geneva", "Ticino", "Valais"), 
    geo_id_aggregated = c("Ger","Ger","Fra","Ita","Fra"),
    rr_central = 1.369,
    rr_increment = 10, 
    cutoff_central = 5,
    erf_shape = "log_linear",
    exp_central = c(11, 11, 10, 8, 7),
    bhd_central = c(4000, 2500, 3000, 1500, 500)
)

## ----echo=TRUE,eval=FALSE,include=TRUE----------------------------------------
# results_iteration$health_main

## ----echo=FALSE,include=TRUE,eval=TRUE----------------------------------------
results_iteration[["health_main"]] |> 
  dplyr::select(geo_id_aggregated, impact_rounded, erf_ci, exp_ci, bhd_ci) |> 
  knitr::kable()

## ----echo=TRUE,eval=FALSE,include=TRUE----------------------------------------
# results_iteration$health_detailed$results_raw

## ----echo=FALSE,include=TRUE,eval=TRUE----------------------------------------
results_iteration[["health_detailed"]][["results_raw"]] |> 
  dplyr::select(geo_id_disaggregated, geo_id_aggregated, impact_rounded) |> 
  knitr::kable()

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_iteration)

## -----------------------------------------------------------------------------
results_iteration_ar <- attribute_health(
    geo_id_disaggregated = c(rep("rural", times = 5), rep("urban", times = 5)), # 2 geographic areas
    ## Both the rural and urban areas belong to the higher-level "total" region
    geo_id_aggregated = "total",
    approach_risk = "absolute_risk",
    exp_central = 
      ## List of 2 elements
      ## Each element a numeric vector that contains the 5 exposure means
      rep(exdat_noise_ha$exposure_mean, times = 2), 
    pop_exp = c(
      exdat_noise_ha$population_exposed_rural, # Rural population exposed
      exdat_noise_ha$population_exposed_urban # Urban population exposed
      ),
    erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
)

## ----echo=TRUE,eval=FALSE,include=TRUE----------------------------------------
# results_iteration_ar$health_main

## ----echo=FALSE,include=TRUE,eval=TRUE----------------------------------------
results_iteration_ar[["health_main"]] |> 
  dplyr::select(geo_id_aggregated, impact_rounded, erf_ci, exp_ci) |> 
  knitr::kable()

## ----echo=TRUE,eval=FALSE,include=TRUE----------------------------------------
# results_iteration_ar$health_detailed$results_raw

## ----echo=FALSE,include=TRUE,eval=TRUE----------------------------------------
results_iteration_ar[["health_detailed"]][["results_raw"]] |> 
  select(geo_id_disaggregated, geo_id_aggregated, impact) |> 
  knitr::kable()

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_iteration_ar)

## -----------------------------------------------------------------------------
scenario_A <- attribute_health(
    exp_central = 8.85,   # EXPOSURE 1
    cutoff_central = 5, 
    bhd_central = 25000,
    approach_risk = "relative_risk",
    erf_shape = "log_linear",
    rr_central = 1.118,
    rr_increment = 10)

## -----------------------------------------------------------------------------
scenario_B <- attribute_health(
    exp_central = 6,     # EXPOSURE 2
    cutoff_central = 5, 
    bhd_central = 25000,
    approach_risk = "relative_risk",
    erf_shape = "log_linear",
    rr_central = 1.118,
    rr_increment = 10)

## -----------------------------------------------------------------------------
scenario_B <- attribute_mod(
  output_attribute_1 = scenario_A, 
  exp_central = 6
)

## -----------------------------------------------------------------------------

results_comparison <- compare(
  
  approach_comparison = "delta", # or "pif" (population impact fraction)
  
  output_attribute_1 = scenario_A,
  
  output_attribute_2 = scenario_B
)

## ----eval=FALSE---------------------------------------------------------------
# results_comparison$health_main

## ----echo=FALSE---------------------------------------------------------------
results_comparison[["health_main"]] |> 
  dplyr::select(
    impact, impact_rounded,
    impact_1, impact_2,
    bhd,
    dplyr::starts_with("exp_"),
    -dplyr::starts_with("exp_ci"), # remove col "exp_ci"
    dplyr::starts_with("rr_con")) |> 
  dplyr::slice_head() |> 
  knitr::kable()

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_comparison, scenario_A, scenario_B)

## -----------------------------------------------------------------------------
results_pm_copd_summarized <- 
  summarize_uncertainty(
    output_attribute = results_pm_copd,
    n_sim = 100
)

## -----------------------------------------------------------------------------
print(
  results_pm_copd_summarized |> 
  purrr::pluck("uncertainty_main")
  )

## -----------------------------------------------------------------------------
results_pm_copd_summarized$uncertainty_detailed$by_simulation$impact[1]

## -----------------------------------------------------------------------------
results_pm_copd_summarized$uncertainty_detailed$by_simulation$health_main[1]

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_pm_copd_summarized)

## -----------------------------------------------------------------------------
results_pm_copd <- 
  monetize(
    output_attribute = results_pm_copd,
    discount_shape = "exponential",
    discount_rate = 0.03,
    discount_years = 5,
    valuation = 50000 # E.g. EURO
  )

## -----------------------------------------------------------------------------
results_pm_copd$monetization_main |> 
  select(erf_ci, monetized_impact) |> 
  knitr::kable()

## -----------------------------------------------------------------------------
results <- monetize(
  impact = 1151,
  valuation = 100
)

## -----------------------------------------------------------------------------
cba <- 
  cba(
    output_attribute = results_pm_copd,
    valuation = 50000,
    cost = 100000000,
    discount_shape = "exponential",
    discount_rate_benefit = 0.03,
    discount_rate_cost = 0.03,
    discount_years_benefit = 5,
    discount_years_cost = 5
  )

## -----------------------------------------------------------------------------
cba$cba_main |>  
  select(benefit, cost, net_benefit) |> 
  knitr::kable()

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(cba)

## -----------------------------------------------------------------------------
results_pm_yll <- attribute_lifetable(
  health_outcome = "yll",
  approach_exposure = "single_year",
  exp_central = 8.85,
  prop_pop_exp = 1,
  cutoff_central = 5,
  rr_central =  1.118, 
  rr_increment = 10,
  erf_shape = "log_linear",
  sex = rep(c("male", "female"), each = 100),
  age_group = rep(0:99, times = 2),
  bhd_central = c(exdat_pop_1$number_of_deaths_male, exdat_pop_1$number_of_deaths_female),
  population = c(exdat_pop_male$population_2019, exdat_pop_female$population_2019),
  year_of_analysis = 2019, 
  min_age = 20
) 

## -----------------------------------------------------------------------------
results_pm_yll$health_main$impact

## ----eval=FALSE---------------------------------------------------------------
# results_pm_yll$health_detailed$results_raw$yll_nest$total_a_central_central_central

## ----echo=FALSE---------------------------------------------------------------
results_pm_yll$health_detailed$results_raw$yll_nest$female_a_central_central_central_central |> 
  select(age_start, population_2019, population_2020, population_2021) |> 
  slice( ( nrow(results_pm_yll$health_detailed$results_raw$yll_nest$female_a_central_central_central_central)-8 ) : 
           nrow(results_pm_yll$health_detailed$results_raw$yll_nest$female_a_central_central_central_central)) |> 
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_deaths <- attribute_lifetable(
#   health_outcome = "deaths",
#   approach_exposure = "single_year",
#   exp_central = 8.85,
#   prop_pop_exp = 1,
#   cutoff_central = 5,
#   rr_central =  1.118,
#   rr_increment = 10,
#   erf_shape = "log_linear",
#   first_age_pop = 0,
#   last_age_pop = 99,
#   deaths_male = exdat_pop_1$number_of_deaths_male,
#   deaths_female = exdat_pop_1$number_of_deaths_female,
#   population_midyear_male = exdat_pop_male$population_2019,
#   population_midyear_female = exdat_pop_female$population_2019,
#   year_of_analysis = 2019,
#   min_age = 20
# )

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_deaths$health_main$impact

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_deaths$health_detailed$results_raw$premature_deaths_nest$total_a_central_central_central

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_deaths$health_detailed$results_raw$premature_deaths_nest$total_a_central_central_central |>
#   slice( ( nrow(results_pm_deaths$health_detailed$results_raw$premature_deaths_nest$total_a_central_central_central)-8 ) :
#            nrow(results_pm_deaths$health_detailed$results_raw$premature_deaths_nest$total_a_central_central_central)) |>
#   knitr::kable()

## ----echo=FALSE, eval=FALSE, include=FALSE------------------------------------
# rm(results_pm_deaths)

## -----------------------------------------------------------------------------
results_pm_copd_yld  <- attribute_health(
    exp_central = 8.85,
    prop_pop_exp = 1,
    cutoff_central = 5,
    bhd_central = 1000,
    rr_central = 1.1, 
    rr_increment = 10, 
    erf_shape = "log_linear",
    info = "pm2.5_yld",
    duration_central = 100,
    dw_central = 1
)

## ----eval=FALSE---------------------------------------------------------------
# results_pm_copd_yld$health_main

## ----echo=FALSE---------------------------------------------------------------
results_pm_copd_yld$health_main |> select(erf_ci, impact) |> kable()

## -----------------------------------------------------------------------------
results_daly <- daly(
     output_attribute_yll = results_pm_yll,
     output_attribute_yld = results_pm_copd_yld
)

## -----------------------------------------------------------------------------
## YLL
results_daly$health_main$impact_yll
## YLD
results_daly$health_main$impact_yld
## DALY
results_daly$health_main$impact_rounded

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_daly, results_pm_yll, results_pm_copd_yld)

## -----------------------------------------------------------------------------
results_pm_copd <- attribute_health(
  erf_shape = "log_linear",
  rr_central = 1.369, 
  rr_increment = 10,
  exp_central = 8.85,
  cutoff_central = 5,
  bhd_central = 30747
) 

results_no2_copd <- attribute_mod(
  output_attribute_1 = results_pm_copd,
  exp_central = 10.9,
  rr_central = 1.031
)

results_multiplicative <- multiexpose(
  output_attribute_1 = results_pm_copd,
  output_attribute_2 = results_no2_copd,
  exposure_name_1 = "pm2.5",
  exposure_name_2 = "no2",
  approach = "multiplicative"
)

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_no2_copd, results_pm_copd)

## ----eval=FALSE---------------------------------------------------------------
# results_multiplicative$health_main

## ----echo=FALSE---------------------------------------------------------------
## Multiexposure impact
results_multiplicative$health_main |> select(impact_rounded) |> kable()

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_multiplicative)

## ----echo=TRUE, eval=TRUE, include=FALSE--------------------------------------
mdi <- prepare_mdi(
  geo_id_disaggregated = exdat_get_mdi$id,
  edu = exdat_get_mdi$edu,
  unemployed = exdat_get_mdi$unemployed,
  single_parent = exdat_get_mdi$single_parent,
  pop_change = exdat_get_mdi$pop_change,
  no_heating = exdat_get_mdi$no_heating,
  n_quantile = 10
)

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(mdi)

## ----eval=FALSE---------------------------------------------------------------
# write.csv(x = results_pm_copd$health_main, file = "exported_results/results_pm_copd.csv")

## ----eval=FALSE---------------------------------------------------------------
# save(results_pm_copd, file = "exported_results/results_pm_copd.Rdata")

## ----eval=FALSE---------------------------------------------------------------
# openxlsx::write.xlsx(x = results_pm_copd$health_main, file = "exported_results/results_pm_copd.xlsx")

