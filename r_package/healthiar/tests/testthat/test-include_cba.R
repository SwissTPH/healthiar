testthat::test_that("results correct cba with direct discounting and exponential discounting shape", {

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

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_copd <-
    healthiar::attribute_health(
      exp_central = data$mean_concentration,
      cutoff_central = data$cut_off_value,
      bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
      rr_central = data$relative_risk,
      rr_lower = data$relative_risk_lower,
      rr_upper = data$relative_risk_upper,
      erf_increment = 10,
      erf_shape = "log_linear",
      info = paste0(data$pollutant,"_", data$evaluation_name))

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

