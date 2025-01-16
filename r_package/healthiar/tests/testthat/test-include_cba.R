testthat::test_that("results correct cba with direct discounting and exponential discounting shape", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::include_cba(
        approach_discount = "direct",
        positive_impact = 50,
        valuation = 20,
        cost = 100,
        discount_shape = "exponential",
        corrected_discount_rate_benefit = 0.03,
        corrected_discount_rate_cost = 0.03,
        discount_years_benefit = 5,
        discount_years_cost = 5,
        discount_overtime = "last_year") |>
      purrr::pluck("cba_main") |>
      dplyr::select(benefit_minus_cost_rounded) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = 776 # Excel file from University of Porto "WP2_Examples.xlsx"
  )
})

testthat::test_that("results correct cba discounting only one specific year with direct discounting and exponential discounting shape", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  bestcost_pm_copd <-
    healthiar::attribute_health(
      exp_central = airqplus_pm_copd$mean_concentration,
      cutoff_central = airqplus_pm_copd$cut_off_value,
      bhd_central = airqplus_pm_copd$incidents_per_100_000_per_year/1E5*airqplus_pm_copd$population_at_risk,
      rr_central = airqplus_pm_copd$relative_risk,
      rr_lower = airqplus_pm_copd$relative_risk_lower,
      rr_upper = airqplus_pm_copd$relative_risk_upper,
      erf_increment = 10,
      erf_shape = "log_linear",
      info = paste0(airqplus_pm_copd$pollutant,"_", airqplus_pm_copd$evaluation_name))

  testthat::expect_equal(
    object =
      healthiar::include_cba(
        approach_discount = "direct",
        output = bestcost_pm_copd,
        valuation = 20,
        cost = 100,
        discount_shape = "exponential",
        corrected_discount_rate_benefit = 0.03,
        corrected_discount_rate_cost = 0.03,
        discount_years_benefit = 5,
        discount_years_cost = 5,
        discount_overtime = "last_year") |>
      purrr::pluck("cba_main") |>
      dplyr::select(benefit_minus_cost_rounded) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = c(60416, 23343, 94436) - 86 # Excel file from University of Porto "WP2_Examples.xlsx"
  )
})

