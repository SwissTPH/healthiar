# exdat_noise ######################################################################################

## The assessment for noise and high annoyance (HA) was provided by NIPH (Norway)
## NOTE AL 2026-02-19: the original data set was called example_road_noise_niph.xlsx; renamed to exdat_cantons_example_road_noise_niph.xlsx

niph_noise_ha_excel <- readxl::read_xlsx(
  path = "data-raw/exdat_noise_example_road_noise_niph.xlsx",
  sheet = "Absolute_risk_HA"
)

exdat_noise <- niph_noise_ha_excel |>
  ## convert to longer format
  dplyr::select(-erf_percent,-number,-yld) |>
  tidyr::pivot_longer( cols = tidyselect::starts_with("population_exposed_"), names_to = "region", values_to = "exposed" ) |>
  ## further data wrangling
  dplyr::mutate(
    region = stringr::str_remove(region, "^population_exposed_"),
    exposure = "noise",
    exposure_metric = "L_den",
    exposure_unit = "dB(A)",
    .before = exposure_category) |>
  dplyr::mutate(
    risk_estimate_type = "absolute_risk",
    erf = "78.9270-3.1162*c+0.0342*c^2",
    health_outcome = "high_annoyance",
    country = "Norway",
    year = "unknown"
  ) |>
  dplyr::relocate(region, .before = 1) |>
  tidyr::drop_na()

# save data ########################################################################################
usethis::use_data(exdat_noise, overwrite = TRUE)
