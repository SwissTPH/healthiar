# prepare_airqplus_export ######################################################

## Turns the csv that AirQ+ exports for a life table assessment into the list
## that the tests use. Unlike the other scripts of this folder, the result is
## test data, so it is written to tests/testthat/testdata/ and not to data/.

## The sections of the export have different numbers of columns, so the file
## cannot be read as one single table. prepare_airqplus_export() reads it as
## plain lines and returns a list with the three parts that a comparison with
## AirQ+ needs:
##   input   one row with the parameters of the evaluation as entered in AirQ+
##   pop     the life table entered in AirQ+, one row per age group
##   output  one row with the results of AirQ+, one column per measure, e.g.
##           value_central_male_yll_over_100_years_all_ages

prepare_airqplus_export <- function(path_to_export) {

  # Remove the semicolons that pad every line to the widest section
  lines <- base::gsub(";+$", "", base::readLines(path_to_export))

  # Split a line into its fields and drop the thousand separators
  split_line <- function(line) {
    base::gsub(",", "", base::strsplit(line, ";", fixed = TRUE)[[1]])
  }

  # Return the value of a "Key;value" line, given the key
  value_of <- function(key) {
    split_line(lines[base::startsWith(lines, base::paste0(key, ";"))][1])[2]
  }

  # Return the rows of a table, from the line after its header
  # until the first empty line
  table_after <- function(header) {
    first <- base::which(base::startsWith(lines, header))[1] + 1
    last <- first + base::which(lines[first:base::length(lines)] == "")[1] - 2
    purrr::map(lines[first:last], split_line)
  }

  # INPUT ######################################################################

  input <-
    tibble::tibble(
      pollutant = value_of("Pollutant:"),
      mean_concentration = base::as.numeric(value_of("Mean Concentration")),
      cut_off_value = base::as.numeric(value_of("Cut-off value")),
      calculation_method = value_of("Calculation Method:"),
      relative_risk = base::as.numeric(value_of("Relative Risk:")),
      relative_risk_lower = base::as.numeric(value_of("Relative Risk Lower:")),
      relative_risk_upper = base::as.numeric(value_of("Relative Risk Upper:")),
      # AirQ+ does not export the increment of the relative risk, which is
      # 10 ug/m3 for the health endpoint of these evaluations
      relative_risk_increment = 10,
      start_year = base::as.numeric(value_of("Start Year")),
      years_to_simulate = base::as.numeric(value_of("Years to simulate")),
      apply_rr_from_age = base::as.numeric(value_of("Apply RR from age")),
      apply_rr_to_age = base::as.numeric(value_of("Apply RR to age")))

  # POP ########################################################################

  pop <-
    table_after("Age from...;Age to end of...") |>
    purrr::map(
      ~ tibble::as_tibble_row(
        base::as.numeric(.x),
        .name_repair = ~ base::c("age_from", "age_to_end_of",
                                 "midyear_population_male",
                                 "number_of_deaths_male",
                                 "midyear_population_female",
                                 "number_of_deaths_female"))) |>
    purrr::list_rbind()

  # The exports have 0 deaths for the females aged 8, which healthiar warns
  # about (a survival probability of 100% has no conceptual logic). It is
  # replaced by 1 as in exdat_lifetable.R, which does the same for the export
  # of the constant exposure. This does not change the results of AirQ+ at the
  # two decimals that it exports
  pop$number_of_deaths_female[pop$age_from == 8] <- 1

  # OUTPUT #####################################################################

  yll_rows <- table_after("Years of Life Lost:")

  # The first row of the table holds the names of the estimate and the gender.
  # "Central - (Male)" becomes "central_male" and
  # "Central - (All genders)" becomes "central_allgenders"
  estimate_and_gender <-
    yll_rows[[1]][-1] |>
    base::tolower() |>
    base::gsub(" - \\(", "_", x = _) |>
    base::gsub("\\)| ", "", x = _)

  # Turns "YLL over 100 Years (all ages)" into "yll_over_100_years_all_ages"
  measure_name <- function(label) {
    label |>
      base::tolower() |>
      base::gsub("\\(|\\)", "", x = _) |>
      base::gsub(" ", "_", x = _)
  }

  output <-
    yll_rows[-1] |>
    # A named function (and not a formula) because the formula of .name_repair
    # would shadow the .x of the outer formula
    purrr::map(
      function(row) {
        names_of_row <-
          base::paste0("value_", estimate_and_gender, "_", measure_name(row[1]))
        tibble::as_tibble_row(base::as.numeric(row[-1]),
                              .name_repair = ~ names_of_row)}) |>
    purrr::list_cbind()

  base::list(input = input, pop = pop, output = output)
}

# BUILD THE TEST DATA ##########################################################

## NOTE: the original AirQ+ export was called
## Lifetable_CH_2019_PM_single_year_AP_no_newborns_default.csv; renamed to
## airqplus_pm_yll_single_year.csv.
## It is the AirQ+ life table assessment of the mortality attributable to PM2.5
## in Switzerland in 2019 with the exposure of one single year (its scenario
## sets the concentration change to -100% from the year after the year of
## analysis on) and without newborns.

airqplus_pm_yll_single_year <-
  prepare_airqplus_export("data-raw/airqplus_pm_yll_single_year.csv")

base::saveRDS(
  airqplus_pm_yll_single_year,
  "tests/testthat/testdata/airqplus_pm_yll_single_year.rds",
  compress = "xz")

## prepare_airqplus_export() also reads
## exdat_lifetable_airqplus_deaths_yll_lifetable_adults.csv, the export of the
## constant exposure. The test data built from it, airqplus_pm_deaths_yll.rds,
## is older and has other column names, so it is left as it is
