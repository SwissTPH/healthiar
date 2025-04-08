# YLL from lifetable ###########################################################

testthat::test_that("results correct pathway_lifetable|exp_single|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|varuncer_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        approach_exposure = "single_year",
        exp_central = data_mort$exp[2], #exp CH 2019
        prop_pop_exp = 1,
        cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
        rr_central = data_mort[2,"rr_central"],
        rr_lower = data_mort[2,"rr_lower"],
        rr_upper =data_mort[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 99,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]) |>
      purrr::pluck("health_main") |>
      dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
      dplyr::select(impact)  |>
      base::unlist() |>
      base::as.numeric(),
    expected =
      c(29274.89, 15328.16,	43118.30), # AirQ+ results from "Lifetable_CH_2019_PM_single_year_AP_no_newborns_default.csv"
    tolerance = 0.49 # I.e. less than 1 YLL
  )
})

testthat::test_that("results correct pathway_lifetable|exp_single|exp_time_constant|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|varuncer_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        approach_exposure = "constant",
        approach_newborns = "without_newborns",
        exp_central = data[["input"]]$mean_concentration,
        cutoff_central = data[["input"]]$cut_off_value,
        rr_central = data[["input"]]$relative_risk,
        rr_lower = data[["input"]]$relative_risk_lower,
        rr_upper = data[["input"]]$relative_risk_upper,
        rr_increment = 10,
        erf_shape = base::gsub("-", "_", data[["input"]]$calculation_method),
        first_age_pop = dplyr::first(data[["pop"]]$age_from...),
        last_age_pop = dplyr::last(data[["pop"]]$age_from...),
        population_midyear_male = data[["pop"]]$midyear_population_male,
        population_midyear_female = data[["pop"]]$midyear_population_female,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        year_of_analysis =  data[["input"]]$start_year,
        min_age = data[["input"]]$apply_rr_from_age
      ) |>
      purrr::pluck("health_main") |>
      dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
      dplyr::select(impact)  |>
      base::unlist() |>
      base::as.numeric(),
    expected =
      c(2776839.17,	1452410.83,	4094163.08), # AirQ+ results from "Lifetable_CH_2019_PM_constant_AP_no_newborns_default.csv"
    tolerance = 0.49 # I.e. less than 1 YLL
  )
})

testthat::test_that("results correct pathway_lifetable|exp_single|exp_time_constant|newborns_TRUE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|varuncer_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        approach_exposure = "constant",
        approach_newborns = "with_newborns",
        exp_central = data[["input"]]$mean_concentration,
        cutoff_central = data[["input"]]$cut_off_value,
        rr_central = data[["input"]]$relative_risk,
        rr_lower = data[["input"]]$relative_risk_lower,
        rr_upper = data[["input"]]$relative_risk_upper,
        rr_increment = 10,
        erf_shape = base::gsub("-", "_", data[["input"]]$calculation_method),
        first_age_pop = dplyr::first(data[["pop"]]$age_from...),
        last_age_pop = dplyr::last(data[["pop"]]$age_from...),
        population_midyear_male = data[["pop"]]$midyear_population_male,
        population_midyear_female = data[["pop"]]$midyear_population_female,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        year_of_analysis =  data[["input"]]$start_year,
        min_age = data[["input"]]$apply_rr_from_age) |>
      purrr::pluck("health_main") |>
      dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
      dplyr::select(impact)  |>
      base::unlist() |>
      base::as.numeric(),
    expected =
      c(3248408.53,	1700230.04,	4786195.41), # AirQ+ results from "Lifetable_CH_2019_PM_constant_AP_with_newborns_default.csv"
    tolerance = 0.49 # I.e. less than 1 YLL
  )
})

testthat::test_that("results correct pathway_lifetable|exp_dist|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|varuncer_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        exp_central = c(8, 9, 10), # Fake data just for testing purposes
        prop_pop_exp = c(0.2, 0.3, 0.5), # Fake data just for testing purposes
        cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
        rr_central = data_mort[2,"rr_central"],
        rr_lower = data_mort[2,"rr_lower"],
        rr_upper = data_mort[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 99,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]) |>
      purrr::pluck("health_main") |>
      dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
      dplyr::select(impact)  |>
      base::unlist() |>
      base::as.numeric(),
    expected =
      c(32704.07, 17121.94, 48173.38), # Result on 20 August 2024 (AirQ+ approach); no comparison study to
    tolerance = 0.1
  )
})

# DEATHS #######################################################################

testthat::test_that("results correct lifetable premature deaths single year exposure with newborns", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "deaths",
        approach_exposure = "single_year",
        approach_newborns = "with_newborns",
        exp_central = data[["input"]]$mean_concentration,
        cutoff_central = data[["input"]]$cut_off_value,
        rr_central = data[["input"]]$relative_risk,
        rr_lower = data[["input"]]$relative_risk_lower,
        rr_upper = data[["input"]]$relative_risk_upper,
        rr_increment = 10,
        erf_shape = base::gsub("-", "_", data[["input"]]$calculation_method),
        first_age_pop = dplyr::first(data[["pop"]]$age_from...),
        last_age_pop = dplyr::last(data[["pop"]]$age_from...),
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        population_midyear_male = data[["pop"]]$midyear_population_male,
        population_midyear_female = data[["pop"]]$midyear_population_female,
        year_of_analysis =  data[["input"]]$start_year,
        min_age = data[["input"]]$apply_rr_from_age) |>
      helper_extract_main_results(),
    expected =
      c(2601, 1371, 3804) # Rounded impacts from "airqplus_deaths_yll_lifetable_adults.xlsx" (the YLL impacts were multiplied by 2 to obtain the total premature deaths deaths)
  )
})

testthat::test_that("results correct lifetable premature deaths exposure distribution with constant exposure and without newborns", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "deaths",
        exp_central = c(8, 9, 10), # Fake data just for testing purposes
        prop_pop_exp = c(0.2, 0.3, 0.5), # Fake data just for testing purposes
        cutoff_central = data_mort$cutoff[2],   # PM2.5=5, NO2=10, i.e. WHO AQG 2021
        rr_central = data[["input"]]$relative_risk,
        rr_lower = data[["input"]]$relative_risk_lower,
        rr_upper = data[["input"]]$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 99,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = 20) |>
      helper_extract_main_results(),
    expected =
      c(2900, 1531, 4239) # Result on 20 August 2024; no comparison study
  )
})
