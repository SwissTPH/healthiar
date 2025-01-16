testthat::test_that("results correct", {

  data <- base::readRDS(testthat::test_path("data", "social_data.rds"))

  bestcost_pm_death <-
    healthiar::attribute_health(
      exp_central = as.list(data$PM25_MEAN),
      cutoff_central = 0,
      rr_central = 1.08, # The data set contains the RR for the exposure but not per increment. Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
      erf_shape = "log_linear",
      erf_increment = 10,
      bhd_central = as.list(data$MORTALITY_TOTAL),
      population = data$POPULATION,
      geo_id_raw = data$CS01012020)

  testthat::expect_equal(
    object =
      healthiar::include_social(
        output = bestcost_pm_death,
        geo_id_raw = data$CS01012020,
        social_indicator = data$score,
        n_quantile = 10,
        approach = "quantile"
        ) |>
      purrr::pluck("social_main") |>
      dplyr::select(difference_value)  |>
      base::unlist() |>
      base::as.numeric(),
    expect = c(22.52416423, 0.32236823, 14.5680866,0.17252793) # Results on 21 Nov 2024
  )
})
