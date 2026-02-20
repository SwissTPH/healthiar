# exdat_ozone ##################################################################

## NOTE AL 2026-02-20: the original data set was called
## LMU_O3_COPD_mort_2016.rda; renamed to exdat_ozone_LMU_O3_COPD_mort_2016.rda
exdat_ozone <- base::readRDS("data-raw/exdat_ozone_LMU_O3_COPD_mort_2016.rds")

exdat_ozone <- exdat_ozone |>
  dplyr::select(
    exposure = Mean.O3,
    proportion_population_exposed = Population.affected
  ) |>
  dplyr::mutate(
    exposure = exposure - 0.05
  ) |>
  dplyr::mutate(
    pollutant = "O3", .before = exposure
  ) |>
  dplyr::mutate(
    exp_unit = "\\mu g/m^3", .after = exposure
  ) |>
  dplyr::mutate(
    mortality_copd_total_year = 29908,
    rr_central = 1.081,
    rr_lower = 1.075,
    rr_upper = 1.086,
    rr_increment = 10,
    cutoff = 64,
    erf_shape = "log_linear",
    exposure_type = "population-weighted_mean_of_maximum_daily_8-hour_averages_april_september)",
    rr_source = "Kazemiparkouhi (2020)",
    country = "germany",
    year = 2016
  )

# save data ####################################################################
usethis::use_data(exdat_ozone, overwrite = TRUE)
