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
library(knitr)

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
  rr_central = exdat_pm$relative_risk, 
  rr_increment = 10, 
  exp_central = exdat_pm$mean_concentration,
  cutoff_central = exdat_pm$cut_off_value,
  bhd_central = exdat_pm$incidence
)

## ----include=TRUE, eval=TRUE, echo=TRUE---------------------------------------
results_pm_copd$health_main

## -----------------------------------------------------------------------------
results_pm_copd$health_main |> 
  dplyr::select(exp, bhd, rr, erf_ci, pop_fraction, impact_rounded) |> 
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
  purrr::pluck("health_main") |>
  dplyr::select(erf_eq, erf_ci, impact_rounded) |> 
  knitr::kable() # Prints tibble in a minimal layout

## ----eval=FALSE, include=TRUE, echo=TRUE--------------------------------------
# results_noise_ha$health_detailed$results_raw

## ----echo=FALSE, include=TRUE, eval=TRUE--------------------------------------
results_noise_ha[["health_detailed"]][["results_raw"]] |> 
  select(exp_category, exp, pop_exp, impact) |> 
  knitr::kable()

## -----------------------------------------------------------------------------
results_noise_ha <- attribute_health(
  approach_risk = "absolute_risk",
  exp_central = 57.5,
  pop_exp = 387500,
  erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_noise_ha$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_noise_ha[["health_main"]] |> 
  dplyr::select(exp_category, impact) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_noise_ha)

## -----------------------------------------------------------------------------
results_iteration <- attribute_health(
    # Names of Swiss cantons
    geo_id_micro = c("Zurich", "Basel", "Geneva", "Ticino", "Jura"),
    # Names of languages spoken in the selected Swiss cantons
    geo_id_macro = c("German","German","French","Italian","French"),
    rr_central = 1.369,
    rr_increment = 10, 
    cutoff_central = 5,
    erf_shape = "log_linear",
    exp_central = c(11, 11, 10, 8, 7),
    bhd_central = c(4000, 2500, 3000, 1500, 500)
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_iteration$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_iteration[["health_main"]] |> 
  dplyr::select(geo_id_macro, impact_rounded, erf_ci, exp_ci, bhd_ci) |> 
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_iteration$health_detailed$results_raw

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_iteration[["health_detailed"]][["results_raw"]] |> 
  dplyr::select(geo_id_micro, impact_rounded, geo_id_macro) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_iteration)

## -----------------------------------------------------------------------------
data <- exdat_noise |> 
  ## Filter for urban and rural regions
  dplyr::filter(region == "urban" | region == "rural")

## -----------------------------------------------------------------------------
results_iteration_ar <- attribute_health( 
    # Both the rural and urban areas belong to the higher-level "total" region
    geo_id_macro = "total",
    geo_id_micro = data$region,
    approach_risk = "absolute_risk",
    exp_central = data$exposure_mean,
    pop_exp = data$exposed,
    erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_iteration_ar$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_iteration_ar[["health_main"]] |> 
  dplyr::select(geo_id_macro, impact_rounded, erf_ci, exp_ci) |> 
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_iteration_ar$health_detailed$results_by_geo_id_micro

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_iteration_ar[["health_detailed"]][["results_by_geo_id_micro"]] |> 
  dplyr::select(geo_id_micro, geo_id_macro, impact) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_iteration_ar, data)

## -----------------------------------------------------------------------------
results_age_group <- attribute_health(
        approach_risk = "relative_risk",
        age = c("below_65", "65_plus"),
        exp_central = c(8, 7),
        cutoff_central = c(5, 5),
        bhd_central = c(1000, 5000),
        rr_central = 1.06,
        rr_increment = 10,
        erf_shape = "log_linear"
      )
results_age_group$health_detailed$results_by_age_group |> 
  dplyr::select(age_group, impact_rounded, exp, bhd) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_age_group)

## ----eval=TRUE, include=TRUE, echo=TRUE---------------------------------------
results_sex <- attribute_health(
        approach_risk = "relative_risk",
        sex = c("female", "male"),
        exp_central = c(8, 8),
        cutoff_central = c(5, 5),
        bhd_central = c(1000, 1100),
        rr_central = 1.06,
        rr_increment = 10,
        erf_shape = "log_linear"
      )
results_sex$health_detailed$results_by_sex |> 
  dplyr::select(sex, impact_rounded, exp, bhd) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_sex)

## -----------------------------------------------------------------------------
info <- data.frame(
  education = rep(c("secondary", "bachelor", "master"), each = 5) # education level
)
output_attribute <- attribute_health(
  rr_central = 1.063,
  rr_increment = 10,
  erf_shape = "log_linear",
  cutoff_central =  0,
  exp_central = sample(6:10, 15, replace = TRUE),
  bhd_central = sample(100:500, 15, replace = TRUE),
  geo_id_micro = c(1:nrow(info)), # (random) ID must be entered
  info = info
)
output_stratified <- output_attribute$health_detailed$results_raw |>
  dplyr::group_by(info_column_1) |>
  dplyr::summarize(mean_impact = mean(impact)) |>
  print()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(info, output_attribute, output_stratified)

## -----------------------------------------------------------------------------
results_pm_copd_mr_brt <- attribute_health(
  exp_central = 8.85,
  bhd_central = 30747,
  cutoff_central = 0,
  # Specify the function based on x-y point pairs that lie on the ERF
  erf_eq_central = splinefun(
    x = c(0, 5, 10, 15, 20, 25, 30, 50, 70, 90, 110),
    y = c(1.00, 1.04, 1.08, 1.12, 1.16, 1.20, 1.23, 1.35, 1.45, 1.53, 1.60),
    method = "natural")
)

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------

x <- c(0, 5, 10, 15, 20, 25, 30, 50, 70, 90, 110)
y <- c(1.00, 1.04, 1.08, 1.12, 1.16, 1.20, 1.23, 1.35, 1.45, 1.53, 1.60)
spline_func <- 
  stats::splinefun(
    x = x,
    y = y,
    method = "natural")
## Generate finer x-values
x_dense <- seq(min(x), max(x), length.out = 200)
## Evaluate the spline function at these points
y_dense <- spline_func(x_dense)
## Plot
plot(x, 
     y, 
     # main = "User-defined ERF", 
     main = "", 
     xlab = "Exposure",
     ylab = "Relative risk",
     col = "blue", 
     pch = 19)
lines(x_dense, y_dense, col = "red", lwd = 2)
legend("topleft", legend = c("Original points", "Spline curve"),
       col = c("blue", "red"), pch = c(19, NA), lty = c(NA, 1), lwd = c(NA, 2))

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
  output_attribute = scenario_A, 
  exp_central = 6
)

## -----------------------------------------------------------------------------

results_comparison <- compare(
  approach_comparison = "delta", # or "pif" (population impact fraction)
  output_attribute_scen_1 = scenario_A,
  output_attribute_scen_2 = scenario_B
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_comparison$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_comparison[["health_main"]] |> 
  dplyr::select(
    impact, impact_rounded,
    impact_scen_1, impact_scen_2,
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

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_copd_summarized$uncertainty_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_copd_summarized$uncertainty_main |> 
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_copd_summarized$uncertainty_detailed$attribute_by_sim

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_copd_summarized$uncertainty_detailed$attribute_by_sim |> 
  dplyr::select(-input, -output) |> 
  dplyr::relocate(impact, .after = geo_id_micro) |> 
  knitr::kable()

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_pm_copd_summarized)

## -----------------------------------------------------------------------------
results_pm_copd <- monetize(
    output_attribute = results_pm_copd,
    discount_shape = "exponential",
    discount_rate = 0.03,
    n_years = 5,
    valuation = 50000 # E.g. EURO
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_copd$monetization_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_copd$monetization_main |> 
  select(erf_ci, monetized_impact) |> 
  knitr::kable()

## -----------------------------------------------------------------------------
results <- monetize(
  impact = 1151,
  valuation = 100
)

## -----------------------------------------------------------------------------
cba <- cba(
    output_attribute = results_pm_copd,
    valuation = 50000,
    cost = 100000000,
    discount_shape = "exponential",
    discount_rate_benefit = 0.03,
    discount_rate_cost = 0.03,
    n_years_benefit = 5,
    n_years_cost = 5
)

## -----------------------------------------------------------------------------
cba$cba_main |>  
  dplyr::select(benefit, cost, net_benefit) |> 
  knitr::kable()

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(cba)

## -----------------------------------------------------------------------------
results_pm_yll <- attribute_lifetable(
  year_of_analysis = 2019, 
  health_outcome = "yll",
  rr_central =  1.118, 
  rr_increment = 10,
  erf_shape = "log_linear",
  exp_central = 8.85,
  cutoff_central = 5,
  min_age = 20, # age from which population is affected by the exposure
  # Life table information
  age_group = exdat_lifetable$age_group,
  sex = exdat_lifetable$sex,
  population = exdat_lifetable$midyear_population,
  # In the life table case, BHD refers to deaths
  bhd_central = exdat_lifetable$deaths
) 

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_yll$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_yll$health_main |> 
  dplyr::slice_head() |> 
  dplyr::select(sex, age_group, impact_rounded, erf_ci, exp_ci, bhd_ci) |> 
  knitr::kable()

## -----------------------------------------------------------------------------
results_pm_yll$health_detailed$results_raw |>
  dplyr::summarize(
    .by = year, 
    impact = sum(impact, na.rm = TRUE)
  )

## -----------------------------------------------------------------------------
results_pm_yll$health_detailed$results_raw |>
  dplyr::summarize(
    .by = year, 
    impact = sum(impact, na.rm = TRUE)) |>
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_yll$health_detailed$intermediate_calculations |>
#   dplyr::filter(sex == "female") |>
#   dplyr::pull(impact_by_age_and_year) |>
#   purrr::pluck(1)

## ----echo=FALSE---------------------------------------------------------------
results_pm_yll$health_detailed$intermediate_calculations |> 
  dplyr::filter(sex == "female") |>
  dplyr::pull(impact_by_age_and_year) |>
  purrr::pluck(1) |>
  dplyr::select(age_start, age_end, impact_2019) |> 
  dplyr::slice( ( dplyr::n() - 8 ) : dplyr::n() ) |> 
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_yll$health_detailed$intermediate_calculations|>
#   dplyr::filter(sex == "female") |>
#   dplyr::pull(projection_if_exposed_by_age_and_year) |>
#   purrr::pluck(1)

## ----echo=FALSE---------------------------------------------------------------
results_pm_yll$health_detailed$intermediate_calculations|> 
  dplyr::filter(sex == "female") |>
  dplyr::pull(projection_if_exposed_by_age_and_year) |>
  purrr::pluck(1) |>   
  dplyr::select(age_start, midyear_population_2019, midyear_population_2020, 
                midyear_population_2021, midyear_population_2022) |>    
  dplyr::slice( ( dplyr::n() - 8 ) : dplyr::n() ) |>  
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_yll$health_detailed$intermediate_calculations|>
#   dplyr::filter(sex == "female") |>
#   dplyr::pull(projection_if_unexposed_by_age_and_year) |>
#   purrr::pluck(1)

## ----echo=FALSE---------------------------------------------------------------
results_pm_yll$health_detailed$intermediate_calculations|> 
  dplyr::filter(sex == "female") |>
  dplyr::pull(projection_if_unexposed_by_age_and_year) |>
  purrr::pluck(1) |>   
  dplyr::select(age_start, midyear_population_2019, midyear_population_2020, 
                midyear_population_2021, midyear_population_2022) |>    
  dplyr::slice( ( dplyr::n() - 8 ) : dplyr::n() ) |>  
  knitr::kable()

## -----------------------------------------------------------------------------
results_pm_deaths <- attribute_lifetable(
  health_outcome = "deaths",
  year_of_analysis = 2019,
  rr_central =  1.118, 
  rr_increment = 10,
  erf_shape = "log_linear",
  exp_central = 8.85,
  cutoff_central = 5,
  min_age = 20, # age from which population is affected by the exposure   
  # Life table information
  age_group = exdat_lifetable$age_group,   
  sex = exdat_lifetable$sex,
  population = exdat_lifetable$midyear_population, 
  bhd_central = exdat_lifetable$deaths
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_deaths$health_main$impact

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_deaths$health_main |> 
  dplyr::slice_head() |> 
  dplyr::select(sex, age_group, impact_rounded, erf_ci, exp_ci, bhd_ci) |> 
  knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_deaths$health_detailed$intermediate_calculations|>
#   dplyr::filter(sex == "female") |>
#   dplyr::pull(projection_if_unexposed_by_age_and_year) |>
#   purrr::pluck(1)

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_yll$health_detailed$intermediate_calculations|> 
  dplyr::filter(sex == "female") |>
  dplyr::pull(projection_if_unexposed_by_age_and_year) |>
  purrr::pluck(1) |>
  dplyr::slice( ( dplyr::n() - 8 ) : dplyr::n() ) |>  
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_pm_deaths)

## -----------------------------------------------------------------------------
results_pm_copd_yld  <- attribute_health(
  rr_central = 1.1, 
  rr_increment = 10, 
  erf_shape = "log_linear",  
  exp_central = 8.85,
  cutoff_central = 5,
  bhd_central = 1000,
  duration_central = 10,
  dw_central = 0.2
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_pm_copd_yld$health_main

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_pm_copd_yld$health_main |> 
  dplyr::select(erf_ci, impact) |> 
  knitr::kable()

## -----------------------------------------------------------------------------
results_daly <- daly(
     output_attribute_yll = results_pm_yll,
     output_attribute_yld = results_pm_copd_yld
)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_daly$health_main |>
#   select(impact_yll_rounded, impact_yld_rounded, impact_rounded)

## ----eval=TRUE, include=TRUE, echo=FALSE--------------------------------------
results_daly$health_main |> 
  dplyr::select(impact_yll_rounded, impact_yld_rounded, impact_rounded) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
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
  output_attribute = results_pm_copd,
  exp_central = 10.9,
  rr_central = 1.031
)

results_multiplicative <- multiexpose(
  output_attribute_exp_1 = results_pm_copd,
  output_attribute_exp_2 = results_no2_copd,
  exp_name_1 = "pm2.5",
  exp_name_2 = "no2",
  approach_multiexposure = "multiplicative"
)

## ----echo=FALSE, eval=TRUE, include=FALSE-------------------------------------
rm(results_no2_copd, results_pm_copd)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# results_multiplicative$health_main

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
results_multiplicative$health_main |> 
  dplyr::select(impact_rounded) |> 
  knitr::kable()

## ----eval=TRUE, include=FALSE, echo=FALSE-------------------------------------
rm(results_multiplicative)

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# mdi <- prepare_mdi(
#   geo_id_micro = exdat_prepare_mdi$id,
#   edu = exdat_prepare_mdi$edu,
#   unemployed = exdat_prepare_mdi$unemployed,
#   single_parent = exdat_prepare_mdi$single_parent,
#   pop_change = exdat_prepare_mdi$pop_change,
#   no_heating = exdat_prepare_mdi$no_heating,
#   n_quantile = 10
# )

## ----eval=FALSE, include=FALSE, echo=FALSE------------------------------------
# rm(mdi)

## ----eval=FALSE, include=TRUE, echo=FALSE-------------------------------------
# mdi <- prepare_mdi(
#   geo_id_micro = exdat_prepare_mdi$id,
#   edu = exdat_prepare_mdi$edu,
#   unemployed = exdat_prepare_mdi$unemployed,
#   single_parent = exdat_prepare_mdi$single_parent,
#   pop_change = exdat_prepare_mdi$pop_change,
#   no_heating = exdat_prepare_mdi$no_heating,
#   n_quantile = 10
# )

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# mdi |>
#   select(geo_id_micro, MDI, MDI_index)

## ----eval=FALSE, include=TRUE, echo=FALSE-------------------------------------
# mdi |>
#   dplyr::select(geo_id_micro, MDI, MDI_index) |>
#   knitr::kable()

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# write.csv(x = results_pm_copd$health_main, file = "exported_results/results_pm_copd.csv")

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# save(results_pm_copd, file = "exported_results/results_pm_copd.Rdata")

## ----eval=FALSE, include=FALSE, echo=TRUE-------------------------------------
# openxlsx::write.xlsx(x = results_pm_copd$health_main, file = "exported_results/results_pm_copd.xlsx")

