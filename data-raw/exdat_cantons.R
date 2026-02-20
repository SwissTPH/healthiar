# exdat_cantons ###############################################################

# load raw data sets

## NOTE AL 2026-02-19: the original data set was called _115a_rz.csv; renamed to exdat_cantons__115a_rz.csv
pm_lc <- utils::read.csv("data-raw/exdat_cantons__115a_rz.csv")

## NOTE AL 2026-02-19: the original data set was called data_20_plus.Rdata and originated from the CHair project; renamed to exdat_cantons_data_20_plus.Rdata
load("data-raw/exdat_cantons_data_20_plus.Rdata")

## NOTE AL 2026-02-19: the original data set was called population_1969_2024_canton.Rdata and orginated from the HiMoMo project; renamed to exdat_cantons_population_1969_2024_canton.Rdata
load("data-raw/exdat_cantons_population_1969_2024_canton.Rdata")

# prepare data #####################################################################################

population_1969_2024_canton <- population_1969_2024_canton |>
  dplyr::filter(year == 2023) |>
  dplyr::mutate(CH = rowSums(dplyr::across(AG:ZH)))

population_1969_2024_canton <- tidyr::pivot_longer(
  data = population_1969_2024_canton,
  cols = 2:ncol(population_1969_2024_canton),
  names_to = "canton",
  values_to = "population"
)

pm_lc <- pm_lc |>
  ## Rename columns
  dplyr::select(
    year,
    canton = region_name,
    lung_cancer_incidence = n,
    geo
  ) |>
  ## Add data
  dplyr::filter(year == 2023) |>
  dplyr::filter(geo == "kt") |>
  dplyr::select(-geo) |>
  dplyr::mutate(exposure = dplyr::if_else(
    year == 2023,
    true = data_20_plus |> dplyr::filter(year == 2023) |> dplyr::pull(pm2.5),
    false = 0)
  ) |>
  dplyr::mutate(exposure_type = "population_weighted_mean") |>
  dplyr::left_join(
    x = _,
    y = population_1969_2024_canton,
    by = dplyr::join_by(year, canton)
  ) |>
  dplyr::arrange(year, canton) |>
  dplyr::mutate(
    rr = 1.16,
    rr_l = 1.10,
    rr_u = 1.23,
    increment = 10,
    function_shape = "log_linear"
  ) |>
  dplyr::mutate(cutoff = 5) |>
  dplyr::mutate(
    pollutant = "PM2.5", .after = exposure
  )

exdat_cantons <- pm_lc |>
  dplyr::filter(canton != "CH") |>
  dplyr::filter(year == 2023) |>
  dplyr::mutate(
    language_main = dplyr::case_when(
      canton %in% c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GR", "GL", "ZG", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR",
                    "AG", "TG") ~ "German",
      canton %in% c("FR", "VD", "NE", "JU", "GE", "VS") ~ "French",
      canton %in% c("TI") ~ "Italian",
      TRUE ~ NA_character_
    ),
    canton_long = dplyr::case_when(
      canton == "ZH" ~ "Zurich",
      canton == "BE" ~ "Bern",
      canton == "LU" ~ "Lucerne",
      canton == "UR" ~ "Uri",
      canton == "SZ" ~ "Schwyz",
      canton == "OW" ~ "Obwalden",
      canton == "NW" ~ "Nidwalden",
      canton == "GL" ~ "Glarus",
      canton == "ZG" ~ "Zug",
      canton == "FR" ~ "Fribourg",
      canton == "SO" ~ "Solothurn",
      canton == "BS" ~ "Basel-Stadt",
      canton == "BL" ~ "Basel-Landschaft",
      canton == "SH" ~ "Schaffhausen",
      canton == "AR" ~ "Appenzell Ausserrhoden",
      canton == "AI" ~ "Appenzell Innerrhoden",
      canton == "SG" ~ "St. Gallen",
      canton == "GR" ~ "Grisons",
      canton == "AG" ~ "Aargau",
      canton == "TG" ~ "Thurgau",
      canton == "TI" ~ "Ticino",
      canton == "VD" ~ "Vaud",
      canton == "VS" ~ "Valais",
      canton == "NE" ~ "Neuchâtel",
      canton == "GE" ~ "Geneva",
      canton == "JU" ~ "Jura",
      TRUE ~ NA_character_
    )
  )

# save data ########################################################################################
usethis::use_data(exdat_cantons, overwrite = TRUE)

