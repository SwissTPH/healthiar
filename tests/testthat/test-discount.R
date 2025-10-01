# QUANTITATIVE TEST ############################################################
testthat::test_that("results correct direct discounting without valuation with exponential discount shape", {

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

# ERROR OR WARNING ########
## ERROR #########

## WARNING #########
