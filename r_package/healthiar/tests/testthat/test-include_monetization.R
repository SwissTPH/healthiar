test_that("results correct simple monetization", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  bestcost_pm_copd <-
    healthiar::attribute_health(
      exp_central = airqplus_pm_copd$mean_concentration,
      cutoff_central = airqplus_pm_copd$cut_off_value,
      bhd_central = airqplus_pm_copd$incidents_per_100_000_per_year/1E5*airqplus_pm_copd$population_at_risk,
      rr_central = airqplus_pm_copd$relative_risk,
      rr_lower = airqplus_pm_copd$relative_risk_lower,
      rr_upper = airqplus_pm_copd$relative_risk_upper,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = paste0(airqplus_pm_copd$pollutant,"_", airqplus_pm_copd$evaluation_name))

  expect_equal(
    object =
      healthiar::include_monetization(output = bestcost_pm_copd,
                                      valuation = 1000) |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = round(1000 * bestcost_pm_copd[["health_main"]]$impact)
  )
})

test_that("results correct direct discounting with discount factor and exponential discount shape", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  bestcost_pm_copd <-
    healthiar::attribute_health(
      exp_central = airqplus_pm_copd$mean_concentration,
      cutoff_central = airqplus_pm_copd$cut_off_value,
      bhd_central = airqplus_pm_copd$incidents_per_100_000_per_year/1E5*airqplus_pm_copd$population_at_risk,
      rr_central = airqplus_pm_copd$relative_risk,
      rr_lower = airqplus_pm_copd$relative_risk_lower,
      rr_upper = airqplus_pm_copd$relative_risk_upper,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = paste0(airqplus_pm_copd$pollutant,"_", airqplus_pm_copd$evaluation_name))

  expect_equal(
    object =
      healthiar::include_monetization(approach_discount = "direct",
                                      output = bestcost_pm_copd,
                                      discount_shape = "exponential",
                                      discount_rate = 0.03,
                                      discount_years = 5,
                                      valuation = 20,
                                      discount_overtime = "last_year") |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = c(60416, 23343, 94436) # Result on 9 Jan 2025
  )
})

test_that("results correct direct discounting with impact vector with discount factor and exponential discount shape", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  expect_equal(
    object =
      healthiar::include_monetization(approach_discount = "direct",
                                      impact = 2E4,
                                      discount_shape = "exponential",
                                      discount_rate = 0.03,
                                      discount_years = 20,
                                      valuation = 1) |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = c(14877) # Result on 5 Dec 2024; from ChatGPT
  )
})

test_that("results correct direct discounting with impact vector discounting only one specific year and exponential discount shape", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  expect_equal(
    object =
      healthiar::include_monetization(approach_discount = "direct",
                                      impact = 50,
                                      discount_shape = "exponential",
                                      discount_rate = 0.03,
                                      discount_years = 5,
                                      discount_overtime = "last_year",
                                      valuation = 20) |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = c(863) # Excel file from University of Porto "WP2_Examples.xlsx"
  )
})

test_that("results correct indirect discounting with exponential discount shape", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  bestcost_pm_yll_exposure_single_year_lifetable_geluft <-
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
      min_age = if(is.na(input_data_mortality$min_age[2])) NULL else input_data_mortality$min_age[2])

  expect_equal(
    object =
      healthiar::include_monetization(approach_discount = "indirect",
                                      output = bestcost_pm_yll_exposure_single_year_lifetable_geluft,
                                      discount_shape = "exponential",
                                      discount_rate = 0.01,
                                      valuation = 1) |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = c(26493, 13877, 39006) # Result on 13 Dec 2024
  )
})

test_that("results correct direct discounting without valuation with exponential discount shape", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  expect_equal(
    object =
      healthiar::include_discount(
        approach_discount = "direct",
        impact = 2E4,
        discount_shape = "exponential",
        discount_rate = 0.03,
        discount_years = 20) |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = 14877 # Result on 15 Jan 2025
  )
})
