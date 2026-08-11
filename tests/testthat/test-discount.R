# QUANTITATIVE TEST ############################################################
testthat::test_that("results the same |direct discounting without valuation with exponential discount shape", {

  testthat::expect_equal(
    object =
      healthiar::discount(
        impact = 2E4,
        discount_shape = "exponential",
        discount_rate = 0.03,
        n_years = 20)$monetization_main$monetized_impact |> base::round(),
    expect = 11074 # Result on 10 March 2025 according to ChatGPT
  )
})

testthat::test_that("results the same discount existing attribute_health() output", {

  results <- attribute_health(
    erf_shape = "log_linear",
    rr_central = 1.369,
    rr_increment = 10,
    exp_central = 8.85,
    cutoff_central = 5,
    bhd_central = 30747
  )
  testthat::expect_equal(
    object =
      healthiar::discount(
        output_attribute = results,
        discount_shape = "exponential",
        discount_rate = 0.03,
        n_years = 20)$monetization_main$monetized_impact |> base::round(),
    expect = 1939.0 # Result on 10 March 2025 according to ChatGPT
  )
})

testthat::test_that("results the same discount existing attribute_health() output", {

  # EKV2010 data
  data <- base::readRDS(testthat::test_path("data", "lifetable_male_ekv_2010.rds"))


  health_impact <- healthiar::attribute_lifetable(
    health_outcome = "yll",
    exp_central = 10,
    cutoff_central = 0,
    rr_central = 1.045,
    rr_increment = 10,
    erf_shape = "log_linear",
    age_group = data$age,
    sex = base::rep(c("male"), each = 106),
    population = data$population_male,
    bhd_central = as.numeric(data$deaths_natural_male),
    year_of_analysis = 2010,
    min_age = 20)

  testthat::expect_equal(
    object =
      healthiar::discount(
        output_attribute = health_impact,
        discount_shape = "exponential",
        discount_rate = 0.0099)$monetization_main$monetized_impact |> base::round(),
    expect = 13453)
  # The result in the EKV2010 project was 12600.
  # Similar deviation as when calculating only health impacts (without discounting)
})




# ERROR OR WARNING ########
## ERROR #########

## WARNING #########
