testthat::test_that("results the same", {

  testthat::expect_equal(
    object =
      healthiar::get_discount_factor(
        discount_rate = 0.07,
        n_years = 5
      ),
    expect = 0.712986179) # Results on 2025-10-01; no comparison study

})
