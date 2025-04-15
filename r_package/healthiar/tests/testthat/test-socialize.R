testthat::test_that("results correct", {

  data <- base::readRDS(testthat::test_path("data", "social_data.rds"))

  bestcost_pm_death <-
    healthiar::attribute_health(
      exp_central = as.list(data$PM25_MEAN),
      cutoff_central = 0,
      rr_central = 1.08, # The data set contains the RR for the exposure but not per increment. Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
      erf_shape = "log_linear",
      rr_increment = 10,
      bhd_central = as.list(data$MORTALITY_TOTAL),
      population = data$POPULATION,
      geo_id_disaggregated = data$CS01012020)

  testthat::expect_equal(
    object =
      healthiar::socialize(
        output_healthiar = bestcost_pm_death,
        geo_id_disaggregated = data$CS01012020,
        social_indicator = data$score,
        n_quantile = 10,
        approach = "quantile"
      )$social_main$difference_value,
    expect = c(22.52416423, 0.32236823, 14.5680866,0.17252793) # Results on 21 Nov 2024
  )
})

testthat::test_that("results the same twice a socialize call", {

  data <- base::readRDS(testthat::test_path("data", "social_data.rds"))

  bestcost_pm_death <-
    healthiar::attribute_health(
      exp_central = as.list(data$PM25_MEAN),
      cutoff_central = 0,
      rr_central = 1.08, # The data set contains the RR for the exposure but not per increment. Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
      erf_shape = "log_linear",
      rr_increment = 10,
      bhd_central = as.list(data$MORTALITY_TOTAL),
      population = data$POPULATION,
      geo_id_disaggregated = data$CS01012020)

  testthat::expect_equal(
    object =
      healthiar::socialize(
        output = bestcost_pm_death,
        geo_id_disaggregated = data$CS01012020,
        social_indicator = data$score,
        n_quantile = 10,
        approach = "quantile"
        )$social_main$difference_value,
    expect = healthiar::socialize(
      impact = bestcost_pm_death[["health_main"]]$impact,
      population = bestcost_pm_death[["health_main"]]$population,
      bhd = bestcost_pm_death[["health_main"]]$bhd,
      exp = bestcost_pm_death[["health_main"]]$exp,
      pop_fraction = bestcost_pm_death[["health_main"]]$pop_fraction,
      geo_id_disaggregated = data$CS01012020,
      social_indicator = data$score,
      n_quantile = 10,
      approach = "quantile")$social_main$difference_value
  )
})
