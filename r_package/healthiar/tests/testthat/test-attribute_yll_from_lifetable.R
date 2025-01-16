test_that("results correct lifetable yll single exposure and one year exposure and without newborns", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::attribute_yll_from_lifetable(
        approach_exposure = "single_year",
        exp_central = input_data_mortality$exp[2], #exp CH 2019
        prop_pop_exp = 1,
        cutoff_central = input_data_mortality$cutoff[2], # WHO AQG 2021
        rr_central = input_data_mortality[2,"rr_central"],
        rr_lower = input_data_mortality[2,"rr_lower"],
        rr_upper =input_data_mortality[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 99,
        population_midyear_male = lifetable_withPopulation[["male"]]$population,
        population_midyear_female = lifetable_withPopulation[["female"]]$population,
        deaths_male = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        year_of_analysis = 2019,
        info = input_data_mortality$pollutant[2],
        min_age = if(is.na(input_data_mortality$min_age[2])) NULL else input_data_mortality$min_age[2]) |>
      purrr::pluck("health_main") |>
      dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
      dplyr::select(impact)  |>
      base::unlist() |>
      base::as.numeric(),
    expected =
      c(2974.89, 15328.16,	43118.30), # AirQ+ results from "Lifetable_CH_2019_PM_single_year_AP_no_newborns_default.csv"
    tolerance = 0.49 # I.e. less than 1 YLL
  )
})

test_that("results correct lifetable yll single exposure and constant exposure and without newborns", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::attribute_yll_from_lifetable(
        approach_exposure = "constant",
        approach_newborns = "without_newborns",
        exp_central = airqplus_pm_deaths_yll[["input"]]$mean_concentration,
        cutoff_central = airqplus_pm_deaths_yll[["input"]]$cut_off_value,
        rr_central = airqplus_pm_deaths_yll[["input"]]$relative_risk,
        rr_lower = airqplus_pm_deaths_yll[["input"]]$relative_risk_lower,
        rr_upper = airqplus_pm_deaths_yll[["input"]]$relative_risk_upper,
        rr_increment = 10,
        erf_shape = gsub("-", "_", airqplus_pm_deaths_yll[["input"]]$calculation_method),
        first_age_pop = first(airqplus_pm_deaths_yll[["pop"]]$age_from...),
        last_age_pop = last(airqplus_pm_deaths_yll[["pop"]]$age_from...),
        population_midyear_male = airqplus_pm_deaths_yll[["pop"]]$midyear_population_male,
        population_midyear_female = airqplus_pm_deaths_yll[["pop"]]$midyear_population_female,
        deaths_male = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        year_of_analysis =  airqplus_pm_deaths_yll[["input"]]$start_year,
        min_age = airqplus_pm_deaths_yll[["input"]]$apply_rr_from_age
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

test_that("results correct lifetable yll single exposure and constant exposure and with newborns", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::attribute_yll_from_lifetable(
        approach_exposure = "constant",
        approach_newborns = "with_newborns",
        exp_central = airqplus_pm_deaths_yll[["input"]]$mean_concentration,
        cutoff_central = airqplus_pm_deaths_yll[["input"]]$cut_off_value,
        rr_central = airqplus_pm_deaths_yll[["input"]]$relative_risk,
        rr_lower = airqplus_pm_deaths_yll[["input"]]$relative_risk_lower,
        rr_upper = airqplus_pm_deaths_yll[["input"]]$relative_risk_upper,
        rr_increment = 10,
        erf_shape = gsub("-", "_", airqplus_pm_deaths_yll[["input"]]$calculation_method),
        first_age_pop = first(airqplus_pm_deaths_yll[["pop"]]$age_from...),
        last_age_pop = last(airqplus_pm_deaths_yll[["pop"]]$age_from...),
        population_midyear_male = airqplus_pm_deaths_yll[["pop"]]$midyear_population_male,
        population_midyear_female = airqplus_pm_deaths_yll[["pop"]]$midyear_population_female,
        deaths_male = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        year_of_analysis =  airqplus_pm_deaths_yll[["input"]]$start_year,
        min_age = airqplus_pm_deaths_yll[["input"]]$apply_rr_from_age) |>
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

test_that("results correct lifetable yll exposure distribution and one year exposure and without newborns", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::attribute_yll_from_lifetable(
        exp_central = c(8, 9, 10), # Fake data just for testing purposes
        prop_pop_exp = c(0.2, 0.3, 0.5), # Fake data just for testing purposes
        cutoff_central = input_data_mortality$cutoff[2], # WHO AQG 2021
        rr_central = input_data_mortality[2,"rr_central"],
        rr_lower = input_data_mortality[2,"rr_lower"],
        rr_upper =input_data_mortality[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 99,
        deaths_male = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male = lifetable_withPopulation[["male"]]$population,
        population_midyear_female = lifetable_withPopulation[["female"]]$population,
        year_of_analysis = 2019,
        info = input_data_mortality$pollutant[2],
        min_age = if(is.na(input_data_mortality$min_age[2])) NULL else input_data_mortality$min_age[2]) |>
      purrr::pluck("health_main") |>
      dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
      dplyr::select(impact)  |>
      base::unlist() |>
      base::as.numeric(),
    expected =
      c(32704.07, 17121.94, 48173.38), # Result on 20 August 2024 (AirQ+ approach); no study to compare bestcost results to
    tolerance = 0.1
  )
})

