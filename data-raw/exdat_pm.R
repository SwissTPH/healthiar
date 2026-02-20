# exdat_pm ##################################################################

## NOTE AL 2026-02-20: the original data set was called
## airqplus_COPD.csv; renamed to exdat_pm_airqplus_COPD.rda

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)


airqplus_copd_export <-
  ## Read csv file from AirQ+ export for incidence of COPD
  readr::read_delim(
    "data-raw/airqplus_COPD.csv",
    delim = ";",
    col_types = readr::cols()
    )

airqplus_export <-
  airqplus_copd_export |>
  # Rename variables
  stats::setNames(c("variable", "value")) |>
  dplyr::rowwise() |>
  # Add and edit columns
  dplyr::mutate(
    # To identify the sections in the table (all capital letters)
    section_break = !stringr::str_detect(variable, "[[:lower:]]") & stringr::str_detect(variable, "[[:alpha:]]"),
    # To give section name to the first row (replacing empty spaces with _ and making lower case)
    section_name = ifelse(section_break, gsub(" ", "_", tolower(variable)), NA)
  ) |>
  # Stop rowwise
  dplyr::ungroup() |>
  # Remove line breaks
  dplyr::mutate(
    section_name = gsub("\r", "", section_name)
  ) |>
  # Fill the NAs in section_name with the first non-NA above
  tidyr::fill(section_name) |>
  # Remove the rows with the section name (not needed anymore since they are in a column now)
  dplyr::filter(!section_break) |>
  # Remove column section_break (not needed anymore because section_name is defined)
  dplyr::select(-section_break)

# The results need to editing
airqplus_output <-
  airqplus_export |>
  # Keep only rows from output
  dplyr::filter(
    section_name %in% c("evaluation_results")
  ) |>
  # Remove rows with NA
  na.omit() |>
  # Remove last ; (no value behind)
  dplyr::mutate(value = gsub("\\;$", "", value)) |>
  # Split columns that are separated by ;
  tidyr::separate(
    col = value,
    sep = ";",
    into = c("value_central", "value_lower", "value_upper")
  ) |>
  # Remove the first row without numeric value
  dplyr::slice(-1) |>
  # Make the table longer to fit into the same structrue as the input table
  tidyr::pivot_longer(cols = starts_with("value_"),
                      names_to = "suffix",
                      names_prefix = "value_",
                      values_to = "value") |>
  # Paste columns variable and suffix to get the specific resulty by CI
  tidyr::unite("variable", variable, suffix)

exdat_pm <-
  airqplus_export |>
  # Keep only rows from input
  dplyr::filter(
    section_name %in%
      c("analysis_properties", "pollution_concentration", "evaluation_parameters")
  ) |>
  # Add rows of results
  dplyr::bind_rows(airqplus_output) |>
  # Edit string of variable to make it standard
  dplyr::mutate(
    # Remove : or parenthesis
    variable = gsub(":|\\(|\\)", "", variable),
    # Remove line breaks
    variable = gsub("\r", "", variable),
    # no capital letters
    variable = tolower(variable),
    # Replace space with _
    variable = gsub(" ", "_", variable ),
    #Replace - with _
    variable = gsub("-", "_", variable),
    # Remove comma from value
    value = gsub(",", "", value)
  ) |>
  # Pivot wider to have specific format for each value
  dplyr::select(variable, value)|>
  tidyr::pivot_wider(
    names_from = variable,
    values_from = value
  )|>
  # Convert percent in proportion
  dplyr::mutate(
    dplyr::across(dplyr::starts_with("estimated_attributable_proportion"),
                  readr::parse_number)
  ) |>
  # Divide by 100 because the columns were a percentage originally (e.g. 5% = 0.05)
  dplyr::mutate(
    dplyr::across(dplyr::starts_with("estimated_attributable_proportion"),
                  ~./100)
  ) |>
  # Convert to numeric
  dplyr::mutate(
    dplyr::across(-c(analysis_name, type, pollutant, location,
                     analyis_comment, evaluation_type, evaluation_name,
                     health_endpoint, calculation_method),
                  as.numeric)
  ) |>
  mutate(incidence = incidents_per_100_000_per_year/1E5*population_at_risk)

exdat_pm <- exdat_pm |>
  mutate(rr_source = "Liu 2020") |>
  mutate(rr_doi = "doi.org/10.1016/j.envint.2020.106267") |>
  mutate(erf_shape = "log_linear") |>
  mutate(rr_increment = 10) |>
  mutate(year_of_analysis = 2019) |>
  select(
    pollutant,
    mean_concentration,
    incidence,
    relative_risk,
    relative_risk_lower,
    relative_risk_upper,
    rr_increment,
    erf_shape,
    cut_off_value,
    total_population,
    rr_source,
    rr_doi,
    analysis_name,
    year_of_analysis,
    location
  )

# save data ####################################################################
usethis::use_data(exdat_pm, overwrite = TRUE)
