testthat::test_that("results correct simple monetization", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_copd <-
    healthiar::attribute_health(
      exp_central = data$mean_concentration,
      cutoff_central = data$cut_off_value,
      bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
      rr_central = data$relative_risk,
      rr_lower = data$relative_risk_lower,
      rr_upper = data$relative_risk_upper,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = paste0(data$pollutant,"_", data$evaluation_name))

  testthat::expect_equal(
    object =
      healthiar::include_monetization(
        output_healthiar = bestcost_pm_copd,
        approach_discount = "direct",
        valuation = 1000,
        discount_rate = NULL,
        discount_shape = NULL) |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = # 1000 * airqplus_pm_copd
      round(1000 * bestcost_pm_copd[["health_main"]]$impact)
  )
})

testthat::test_that("results the same fake_monetization|discount_appr_direct|discount_rate_TRUE|discount_shape_exp|discount_overtime_last_year|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_copd <-
    healthiar::attribute_health(
      exp_central = data$mean_concentration,
      cutoff_central = data$cut_off_value,
      bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
      rr_central = data$relative_risk,
      rr_lower = data$relative_risk_lower,
      rr_upper = data$relative_risk_upper,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = paste0(data$pollutant,"_", data$evaluation_name))

  testthat::expect_equal(
    object =
      healthiar::include_monetization(approach_discount = "direct",
                                      output_healthiar = bestcost_pm_copd,
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
    expect = c(60416, 23343, 94436) # Result on 9 Jan 2025 ; no comparison study
  )
})

testthat::test_that("results correct pathway_monetization|discount_appr_direct|discount_rate_TRUE|discount_shape_exp|discount_overtime_all_years|", {

  testthat::expect_equal(
    object =
      healthiar::include_monetization(approach_discount = "direct",
                                      impact = 2E4,
                                      discount_shape = "exponential",
                                      discount_rate = 0.03,
                                      discount_years = 20,
                                      discount_overtime = "all_years",
                                      valuation = 1) |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = c(14877) # Result on 5 Dec 2024; from ChatGPT
  )
})

testthat::test_that("results correct pathway_monetization|discount_appr_direct|discount_rate_TRUE|discount_shape_exp|discount_overtime_last_year|", {

  testthat::expect_equal(
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

testthat::test_that("results the same fake_monetization|discount_appr_indirect|discount_rate_TRUE|discount_shape_exp|discount_overtime_all_years|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  bestcost_pm_yll_exposure_single_year_lifetable_geluft <-
    healthiar::attribute_yll_from_lifetable(
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
      min_age = if(is.na(data_mort$min_age[2])) NULL else input_data_mortality$min_age[2])

  testthat::expect_equal(
    object =
      healthiar::include_monetization(approach_discount = "indirect",
                                      output_healthiar = bestcost_pm_yll_exposure_single_year_lifetable_geluft,
                                      discount_shape = "exponential",
                                      discount_rate = 0.01,
                                      valuation = 1,
                                      discount_overtime = "all_years") |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = c(26493, 13877, 39006) # Result on 13 Dec 2024 ; no comparison study
  )
})

testthat::test_that("results the same fake_monetization|discount_appr_direct|discount_rate_TRUE|discount_shape_exp|discount_overtime_all_years|", {

  testthat::expect_equal(
    object =
      healthiar::include_discount(
        approach_discount = "direct",
        impact = 2E4,
        discount_shape = "exponential",
        discount_rate = 0.03,
        discount_years = 20,
        discount_overtime = "all_years") |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = 14877 # Result on 15 Jan 2025 ; no comparison study
  )
})
