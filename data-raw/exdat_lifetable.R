# exdat_lifetable ##############################################################

## NOTE AL 2026-02-20: the original data set was called
## airqplus_deaths_yll_lifetable_adults.csv; renamed to
## exdat_lifetable_airqplus_deaths_yll_lifetable_adults.csv

# Read csv file from AirQ+ export for deaths and YLLs
airqplus_deaths_yll_export <-
  readr::read_delim(
    "data-raw/exdat_lifetable_airqplus_deaths_yll_lifetable_adults.csv",
    delim = ";",
    col_types = readr::cols())

airqplus_export_edited <- airqplus_deaths_yll_export |>
  # Rename variables
  stats::setNames(c("variable", "value")) |>
  # Remove ";" at the end of the string
  # Important to be done before
  dplyr::mutate(
    value = gsub("\\;$" ,"", value)
    ) |>
  # Add columns for dimension and dimension_below
  dplyr::mutate(
    # Identify dimension
    dimension = stringr::str_count(value, "\\;")+1,
    # Take the dimension from the row below
    # (this is to be identify sub-sections)
    dimension_below = dplyr::lead(dimension, n=1)
    ) |>
  # Identify breaks
  dplyr::rowwise() |>
  dplyr::mutate(
    # To identify the sections in the table (all capital letters)
    section_break = !stringr::str_detect(variable, "[[:lower:]]") &
      stringr::str_detect(variable, "[[:alpha:]]"),
    subsection_break = is.na(value) & dimension_below > 1
    ) |>
  # Stop rowwise
  dplyr::ungroup() %>%

  # Edit columns to standardize the strings
  dplyr::mutate(
    # Remove : or parenthesis
    variable = gsub(":|\\(|\\)", "", variable),
    # Remove line breaks
    variable = gsub("\\\r", "", variable),
    # no capital letters
    variable = tolower(variable),
    # Replace space with _
    variable = gsub(" ", "_", variable ),
    # Replace - with _
    variable = gsub("-", "_", variable),
    # Remove comma from value
    value = gsub(",", "", value)
    ) %>%
  # Give names to sections and subsections
  dplyr::mutate(
    # To identify the sections in the table (all capital letters)
    section_name = ifelse(section_break,
                          variable,
                          NA),
    subsection_name = ifelse(subsection_break,
                             variable,
                             NA)
    ) |>
  # Fill the NAs in (sub)section_name with the first non-NA above
  tidyr::fill(c("section_name", "subsection_name")) |>
  # If subsection is NA assign section name
  dplyr::mutate(
    subsection_name = ifelse(is.na(subsection_name),
                                         section_name,
                                         subsection_name)
    ) %>%
  # Remove the rows with the (sub)section name
  # (not needed anymore since they are in a column now)
  dplyr::filter(!section_break) |>
  dplyr::filter(!subsection_break) |>
  # Remove column (sub)section_break
  # (not needed anymore because section_name is defined)
  dplyr::select(
    -section_break,
    -subsection_break,
    -dimension_below
    )

# Create nested list by section and subsecton name
airqplus_list <-
  split(
    airqplus_export_edited,
    airqplus_export_edited$section_name
    ) %>% # must be old pipe
  purrr::map(
    .,
    function(x) split(x, x$subsection_name)
    ) %>% # must be old pipe
  purrr::map(
    .,
    ~ purrr::map(
      ., function(x)
    tidyr::separate_wider_delim(
      x,
      cols = value,
      delim = ";",
      names = paste0("value_", 1:max(x$dimension, na.rm = TRUE)))
    )
    )

# POP
# population data including probability of dying
airqplus_pop <- airqplus_list[["evaluation_parameters"]][["population_data"]]

# Rename columns
names(airqplus_pop) <-
  # If starting with "value_" then adopt the name of the first row
  ifelse(
    grepl("value_|variable", names(airqplus_pop)),
         paste0(airqplus_pop[1,]),
         names(airqplus_pop)
         ) %>% # must be old pipe
  # Standardize names
  # Replace " -(" with "_"
  gsub(" ", "_",.) %>% # must be old pipe
  # Remove bracket and empty spaces
  gsub("\\(|\\)|\\-", "",.) %>% # must be old pipe
  # Lower case
  tolower(.)

# Continue editing
airqplus_pop <-
  airqplus_pop |>
  # Remove column dimension (not needed anymore)
  dplyr::select(-dimension) |>
  # Remove the first row without numeric value
  dplyr::slice(-1) %>%
  # Convert to numeric
  dplyr::mutate(
    across(
      -c(section_name,
              -subsection_name),
                       as.numeric
      )
    )

## Replace 0 with 1 for age 8 in airqplus_pop$number_of_deaths_female
airqplus_pop$number_of_deaths_female[9] <- 1 # row 9 corresponds to age 8

# Build the list
exdat_lifetable <- airqplus_pop |>
  dplyr::rename(age_group = age_from...) |>
  dplyr::rename(deaths_male = number_of_deaths_male) |>
  dplyr::rename(deaths_female = number_of_deaths_female) |>
  dplyr::select(-age_to_end_of..., -section_name, -subsection_name) |>
  tidyr::pivot_longer(
    cols = -age_group,
    names_to = c(".value", "sex"),
    names_pattern = "(midyear_population|deaths)_(male|female)"
    ) |>
  dplyr::arrange(sex)

# save data ####################################################################
usethis::use_data(exdat_lifetable, overwrite = TRUE)
