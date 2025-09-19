####################################################################################################
# EXERCISES ########################################################################################
####################################################################################################

# SETUP ############################################################################################
install.packages("remotes")
remotes::install_github(repo = "SwissTPH/healthiar", build_vignettes = TRUE)
library(healthiar)
library(dplyr)



# EXERCISE 1) Relative risk 1 ######################################################################

## Determine COPD (chronic obstructive pulmonary disease) cases attributable to long-term PM2.5
## exposure. In the year of analysis, population-weighted mean PM2.5 exposure was 8.85 μg / m^3 and
## COPD incidence 30747 cases. You decided to use a relative risk (RR) of 1.369 (Liu 2021) for an
## exposure increment of 10 μg / m^3, which you apply using a cutoff value of 5 μg / m^3 and a
## "log_linear" exposure-response function (ERF) shape.

# EXERCISE
results_rr_1 <- attribute_health(
  exp_central = ,
  erf_shape = "log_linear",
  rr_central = ,
  rr_increment = ,
  cutoff_central = ,
  bhd_central =
)

# SOLUTION
results_rr_1 <- attribute_health(
  exp_central = 8.85, # μg / m^3
  erf_shape = "log_linear", # erf = exposure-response function
  rr_central = 1.369,
  rr_increment = 10,  # μg / m^3
  cutoff_central = 5, # μg / m^3
  bhd_central = 30747 # bhd = baseline health data (here: COPD incidence)
)

results_rr_1$health_main$impact_rounded # 3502



# EXERCISE 2) Relative risk 2 ######################################################################

## With a new air pollution policy the annual mean PM2.5 exposure is reduced to 6 μg / m^3.
## Determine the attributable cases with the new exposure level (keeping the calculation
## specifications as in the first exercise).

# EXERCISE
?attribute_mod # run this line to open the help page of the attribute_mod() function

results_rr_2 <- attribute_mod(
  output_attribute = ,
  exp_central =
)

## HINT: the function attribute_mod() can be used to modify and existing assessment; alternatively,
## you can copy and adjust the attribute_health() function call from exercise 1.

# SOLUTION
results_rr_2 <- attribute_health(
  exp_central = 6,
  erf_shape = "log_linear",
  rr_central = 1.369,
  rr_increment = 10,
  cutoff_central = 5,
  bhd_central = 30747
)

results_rr_2 <- attribute_mod(
  output_attribute = results_rr_1,
  exp_central = 6
)

results_rr_2$health_main$impact_rounded # 951



# EXERCISE 3) Comparison ###########################################################################

## Now compare the number of attributable cases from before (exercises 1) & after (exercise 2)
## the policy using the "delta" approach.

# EXERCISE
results_comparison <- compare(...)

?compare # HINT: run this line to open the help page of the compare() function

# SOLUTION
results_comparison <- compare(
  output_attribute_scen_1 = results_rr_1,
  output_attribute_scen_2 = results_rr_2
)

results_comparison$health_main$impact_rounded # 2551



# EXERCISE 4) Multiple geo units 1 #################################################################

## Assess the number of attributable lung cancer cases due to PM2.5 in all Swiss cantons (~ regions)
##  in 2023 using the input data from the example data set "exdat_cantons".

# EXERCISE
data("exdat_cantons") # this line loads the variable into your RStudio environment tab
View(exdat_cantons) # this line let's you inspect the variable

results_geo_1 <- attribute_health(
  geo_id_micro = ,
  ...
)

## HINT: the function arguments can be specified via exdat_cantons$<column_name>

# SOLUTION
results_geo_1 <- attribute_health(
  geo_id_micro = exdat_cantons$canton,
  exp_central = exdat_cantons$exposure,
  erf_shape = exdat_cantons$function_shape,
  rr_central = exdat_cantons$rr,
  rr_increment = exdat_cantons$increment,
  cutoff_central = exdat_cantons$cutoff,
  bhd_central = exdat_cantons$lung_cancer_incidence
)

results_geo_1$health_main |>
  select(geo_id_micro, impact_rounded) |>
  arrange(desc(impact_rounded)) |>
  print(n = 26)
## Note that in the assessment we used the national exposure for each canton (due to lack of
## canton-specific population-weighted exposure), so the results are a very rough approximation



# EXERCISE 5) Multiple geo units 2 #################################################################

## Now, in addition to the canton-specific impacts from exercise 4, also determine the attributable
## lung cancer cases by language region by adding a second level to the assessment using the input
## data from the example data set "exdat_cantons".

# EXERCISE
results_geo_2 <- attribute_health(
  geo_id_micro = exdat_cantons$canton,
  geo_id_macro = ,
  exp_central = exdat_cantons$exposure,
  erf_shape = exdat_cantons$function_shape,
  rr_central = exdat_cantons$rr,
  rr_increment = exdat_cantons$increment,
  cutoff_central = exdat_cantons$cutoff,
  bhd_central = exdat_cantons$lung_cancer_incidence
)

## HINT: the function attribute_mod() can be used to modify and existing assessment; alternatively,
## you can copy and adjust the attribute_health() function call from exercise 4.

# SOLUTION
results_geo_2 <- attribute_mod(
  output_attribute = results_geo_1,
  geo_id_macro = exdat_cantons$language_main
)
results_geo_2$health_main$impact_rounded
## German-speaking: 385, French-speaking: 138, Italian-speaking: 31
## Note that in the assessment we used the national exposure for each canton (due to lack of
## canton-specific population-weighted exposure), so the results are a very rough approximation

results_geo_2 <- attribute_health(
  geo_id_micro = exdat_cantons$canton,
  geo_id_macro = exdat_cantons$language_main,
  exp_central = exdat_cantons$exposure,
  erf_shape = exdat_cantons$function_shape,
  rr_central = exdat_cantons$rr,
  rr_increment = exdat_cantons$increment,
  cutoff_central = exdat_cantons$cutoff,
  bhd_central = exdat_cantons$lung_cancer_incidence
)
results_geo_2$health_main$impact_rounded
## German-speaking: 385, French-speaking: 138, Italian-speaking: 31
## Note that in the assessment we used the national exposure for each canton (due to lack of
## canton-specific population-weighted exposure), so the results are a very rough approximation



# EXERCISE 6) Relative risk 3 ######################################################################

## Determine the number of deaths from COPD attributable to ozone exposure (exposure categories) in
## Germany in 2016 using the input data from the example data set "exdat_ozone".

data("exdat_ozone")
View("exdat_ozone")

# EXERCISE
results_rr_3 <- attribute_health(
  exp_central = ,
  prop_pop_exp = ,
  erf_shape = ,
  rr_central = ,
  rr_increment = ,
  cutoff_central = ,
  bhd_central =
)

# SOLUTION
results_rr_3 <- attribute_health(
  exp_central = exdat_ozone$exposure,
  prop_pop_exp = exdat_ozone$proportion_population_exposed,
  erf_shape = exdat_ozone$erf_shape,
  rr_central = exdat_ozone$rr_central,
  rr_increment = exdat_ozone$rr_increment,
  cutoff_central = exdat_ozone$cutoff,
  bhd_central = exdat_ozone$mortality_copd_total_year
)

results_rr_3$health_main$impact_rounded # 4340



# EXERCISE 7) Absolute risk ########################################################################

## Determine the number of high annoyance cases due to noise exposure (exposure categories) in
## Norway using the input data from the example data set "exdat_noise".

data("exdat_noise")
View("exdat_noise")
exdat_noise <- exdat_noise |> filter(region == "total") # Filter for total (= country-wide) entries

# EXERCISE
results_ar <- attribute_health(
  approach_risk = ,
  exp_central = ,
  pop_exp = ,
  erf_eq_central =
)

# SOLUTION
results_ar <- attribute_health(
  approach_risk = exdat_noise$risk_estimate_type[1],
  exp_central = exdat_noise$exposure_mean,
  pop_exp = exdat_noise$exposed,
  erf_eq_central = exdat_noise$erf
)

results_ar$health_main$impact_rounded # 174232
