testthat::test_that("results correct direct discounting without valuation with exponential discount shape", {

  testthat::expect_equal(
    object =
      healthiar::discount(
        impact = 2E4,
        discount_shape = "exponential",
        discount_rate = 0.03,
        discount_years = 20)$monetization_main$monetized_impact |> base::round(),
    expect = 11074 # Result on 10 March 2025 according to ChatGPT
  )
})
