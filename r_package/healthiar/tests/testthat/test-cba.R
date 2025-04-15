# RAW INPUT ####################################################################

testthat::test_that("results correct pathway_cba|discount_appr_direct|discount_shape_exp|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      healthiar::cba(
        positive_impact = 50,
        valuation = 20,
        cost = 100,
        discount_shape = "exponential",
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_years_benefit = 5,
        discount_years_cost = 5
        )$cba_main$net_benefit_rounded,
    expect = 776 # Excel file from University of Porto "WP2_Examples.xlsx"
  )
})

# HEALTHIAR INPUT ##############################################################

testthat::test_that("results the same discounting only one specific year", {

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
      healthiar::cba(
        output_healthiar = bestcost_pm_copd,
        valuation = 20,
        cost = 100,
        discount_shape = "exponential",
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_years_benefit = 5,
        discount_years_cost = 5
        )$cba_main$net_benefit_rounded,
    expect = c(60330,23257,94350) # Results on 2025-03-06; no comparison study
  )
})

testthat::test_that("results the same fake_cba|discount_appr_direct|discount_shape_exp|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

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
      healthiar::cba(
        output_healthiar = bestcost_pm_copd,
        valuation = 20,
        cost = 100,
        discount_shape = "exponential",
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_years_benefit = 5,
        discount_years_cost = 5
        )$cba_main$net_benefit_rounded,
    expect = c(60416, 23343, 94436) - 86 # Results on 2025-02-05 ; no comparison study
  )
})

