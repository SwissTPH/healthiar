# Setup ############################################################################################
library(dplyr)

# PM2.5 ############################################################################################

# Layout for data
## One data set each for RR & AR
### Columns: year, canton, rr, bhd, exp, population,
## Multiple geo IDs (cantons)
## Simulate exposed population / BHD using HiMoMo population data

## Load data ####

load("C:/Users/luytax/switchdrive/Hitze/HiMoMo/2025_himomo2024/data/clean/canton_key.RData")

## Prepare datasets ####

## RR


## AR
noise_ha <- pm_lc |>
  dplyr::filter(year == 2023) |>
  dplyr::mutate(
    exposure_category_1 = 57.5,
    exposure_category_2 = 62.5,
    exposure_category_3 = 67.5,
    exposure_category_4 = 72.5,
    exposure_category_5 = 77.5,
    ) |>
  dplyr::select(-exposure, -lung_cancer_incidence)

noise_ha <- noise_ha |>
  tidyr::pivot_longer(
    data = _,
    cols = dplyr::starts_with("exposure_category"),
    names_to = "exposure_category",
    values_to = "exposure_level"
  )  |>
  dplyr::mutate(exposure_category = as.numeric(gsub("exposure_category_", "", exposure_category)))

noise_ha <- noise_ha |>
  dplyr::mutate(population_exposed = round(
    dplyr::case_when(
    exposure_category == 1 ~ population * 0.074319355,
    exposure_category == 2 ~ population * 0.054852478,
    exposure_category == 3 ~ population * 0.036785683,
    exposure_category == 4 ~ population * 0.013847374,
    exposure_category == 5 ~ population * 0.001476797
    )
  )) |>
  dplyr::mutate(exposure_category_range = dplyr::case_when(
    exposure_category == 1 ~ "55 ≤ exposure < 60",
    exposure_category == 2 ~ "60 ≤ exposure < 65",
    exposure_category == 3 ~ "65 ≤ exposure < 70",
    exposure_category == 4 ~ "70 ≤ exposure < 75",
    exposure_category == 5 ~ "75 ≤"
  )) |>
  dplyr::mutate(exposure_type = "population_exposed_to_exposure_level") |>
  dplyr::arrange(year, canton) |>
  dplyr::select(-population) |>
  dplyr::relocate(exposure_category_range, .before = exposure_type) |>
  dplyr::select(-rr, -rr_u, -rr_l, -increment, -function_shape) |>
  dplyr::mutate(formula = "78.927-3.1162*c+0.0342*c^2") |>
  dplyr::relocate(exposure_type, .after = formula) |>
  dplyr::mutate(disability_weight = 0.02) |>
  dplyr::mutate(cost_per_case = 150) |>
  dplyr::mutate(curreny = "Swiss Francs")

## Save data sets ####
pm_lc_ch <- pm_lc |>
  dplyr::filter(canton == "CH") |>
  dplyr::select(-canton)

# pm_lc_cantons <- pm_lc |>


noise_ha_ch <- noise_ha |>
  dplyr::filter(canton == "CH") |>
  dplyr::select(-canton)

noise_ha_cantons <- noise_ha |>
  dplyr::filter(canton != "CH") |>
  dplyr::filter(year == 2023)

rm(canton_key, data_20_plus, noise_ha, pm_lc, population_1969_2024_canton)

# exdat_pm #########################################################################################

# NOTE: extra info about this data set is found in the testthat data set airqplus_pm_copd

exdat_pm <- exdat_pm |>
  mutate(year_of_analysis = 2019, .before = 1) |>
  mutate(rr_source = "Liu 2020") |>
  mutate(rr_doi = "doi.org/10.1016/j.envint.2020.106267") |>
  mutate(erf_shape = "log_linear", .after = relative_risk_upper) |>
  select(-calculation_method)

save(exdat_pm, file = "data/exdat_pm.rda")

# exdat_prepare_exposure ###########################################################################

## Files sent by AP on 2025-09-22 are saved in /inst/extdata, from which they are loaded for the
## example in the prepare_exposure fun doc

## For some reason it doesn't work when
### 1) you save the .tif and .gpkg in the data folder (then they are not automatically loaded when loading the package)
### 2) you save them as .rda files (then they are loaded correctly but the pm2.5.tif file gets somehow corrupted and there's an error when you run prepare_exposure())
#### Error: external pointer is not valid

## data sets sent by AP on 2025-09-22

# exdat_pwm_1 <- terra::rast("tests/testthat/data/pm25.tif")
# exdat_pwm_2 <- sf::st_read("tests/testthat/data/municipalities_brussels.gpkg", quiet = TRUE)
#
# save(exdat_pwm_1, file = "data/exdat_pwm_1.rda")
# save(exdat_pwm_2, file = "data/exdat_pwm_2.rda")
#
# test <- healthiar::prepare_exposure(
#   poll_grid = exdat_pwm_1,
#   geo_units = exdat_pwm_2,
#   population = sf::st_drop_geometry(exdat_pwm_2$population),
#   geo_id_macro = sf::st_drop_geometry(exdat_pwm_2$region)
# )

usethis::use_data(DATASET, overwrite = TRUE)
