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
      healthiar::monetize(
        output_healthiar = bestcost_pm_copd,
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

testthat::test_that("results the same fake_monetization|discount_appr_direct|discount_rate_TRUE|discount_shape_exp|", {

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
      healthiar::monetize(output_healthiar = bestcost_pm_copd,
                          discount_shape = "exponential",
                          discount_rate = 0.03,
                          discount_years = 5,
                          valuation = 20) |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = c(60416, 23343, 94436) # Result on 9 Jan 2025 ; no comparison study
  )
})

testthat::test_that("results correct pathway_monetization|discount_appr_direct|discount_rate_TRUE|discount_shape_exp|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(impact = 2E4,
                          discount_shape = "exponential",
                          discount_rate = 0.03,
                          discount_years = 20,
                          valuation = 1) |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = c(11074) # Result on 2024-03-10; from ChatGPT ## OLD results from when it was include_monetization
    # expect = c(14877) # Result on 5 Dec 2024; from ChatGPT ## OLD results from when it was include_monetization
  )
})

testthat::test_that("results correct pathway_monetization|discount_appr_direct|discount_rate_TRUE|discount_shape_exp|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(impact = 50,
                          discount_shape = "exponential",
                          discount_rate = 0.03,
                          discount_years = 5,
                          valuation = 20) |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = c(863) # Excel file from University of Porto "WP2_Examples.xlsx"
  )
})

# TODO: FIX DISCREPANCY ####
# testthat::test_that("results the same fake_monetization|discount_appr_indirect|discount_rate_TRUE|discount_shape_exp|", {
#
#   data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
#   data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
#   data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))
#
#   bestcost_pm_yll_exposure_single_year_lifetable_geluft <-
#     healthiar::attribute_lifetable(
#       health_outcome = "yll",
#       approach_exposure = "single_year",
#       exp_central = data_mort$exp[2], #exp CH 2019
#       prop_pop_exp = 1,
#       cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
#       rr_central = data_mort[2,"rr_central"],
#       rr_lower = data_mort[2,"rr_lower"],
#       rr_upper =data_mort[2,"rr_upper"],
#       rr_increment = 10,
#       erf_shape = "log_linear",
#       first_age_pop = 0,
#       last_age_pop = 99,
#       population_midyear_male = data_lifetable[["male"]]$population,
#       population_midyear_female = data_lifetable[["female"]]$population,
#       deaths_male = data[["pop"]]$number_of_deaths_male,
#       deaths_female = data[["pop"]]$number_of_deaths_female,
#       year_of_analysis = 2019,
#       info = data_mort$pollutant[2],
#       min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2])
#
#   testthat::expect_equal(
#     object =
#       healthiar::monetize(output_healthiar = bestcost_pm_yll_exposure_single_year_lifetable_geluft,
#                           discount_shape = "exponential",
#                           discount_rate = 0.01,
#                           valuation = 1) |>
#       purrr::pluck("monetization_main") |>
#       dplyr::select(monetized_impact_rounded) |>
#       base::unlist() |>
#       base::as.numeric() |>
#       base::round(),
#     expect = c(26493, 13877, 39006) # Result on 13 Dec 2024 ; no comparison study
#   )
# })

testthat::test_that("results the same fake_monetization|discount_appr_direct|discount_rate_TRUE|discount_shape_exp|", {

  testthat::expect_equal(
    object =
      healthiar::discount(
        impact = 2E4,
        discount_shape = "exponential",
        discount_rate = 0.03,
        discount_years = 20) |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = 11074 # Result on 15 Jan 2025 ; no comparison study
    # expect = 14877 # Result on 15 Jan 2025 ; no comparison study # OLD results
  )
})
