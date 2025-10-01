testthat::test_that("results the same", {

  testthat::expect_equal(
    object =
      healthiar::get_inflation_factor(
        inflation_rate = 0.02,
        n_years = 5
      ),
    expect = 1.10408080) # Results on 2025-10-01; no comparison study
})
