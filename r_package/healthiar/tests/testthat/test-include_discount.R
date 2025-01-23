testthat::test_that("results correct direct discounting without valuation with exponential discount shape", {

  testthat::expect_equal(
    object =
      healthiar::include_discount(
        approach_discount = "direct",
        impact = 2E4,
        discount_shape = "exponential",
        discount_rate = 0.03,
        discount_years = 20) |>
      purrr::pluck("monetization_main") |>
      dplyr::select(monetized_impact) |>
      base::unlist() |>
      base::as.numeric() |>
      base::round(),
    expect = 14877 # Result on 15 Jan 2025
  )
})
