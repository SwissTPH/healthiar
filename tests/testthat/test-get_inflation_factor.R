testthat::test_that("results the same", {

  testthat::expect_equal(
    object =
      healthiar::get_inflation_factor(
        inflation_rate = 0.02,
        n_years = 5
      ),
    expect = 1.10408080) # Results on 2025-10-01; no comparison study
})

testthat::test_that("results the same |year_specific_inflation_rate|", {

  testthat::expect_equal(
    object =
      healthiar::get_inflation_factor(
        inflation_rate = c(0.02, 0.03, 0.05, 0.04, 0.02),
        n_years = 5
      ),
    expect = 1.17020030) # Results on 2026-09-10; no comparison study
})
