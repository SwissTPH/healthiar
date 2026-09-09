testthat::test_that("results the same", {

  testthat::expect_equal(
    object =
      healthiar::get_discount_factor(
        discount_rate = 0.07,
        n_years = 5
      ),
    expect = 0.712986179) # Results on 2025-10-01; no comparison study
})

testthat::test_that("results the same |get_discount_factor|vector_of_years|", {

  # One discount factor per entered year. Before, the nested ifelse() returned
  # a result of the length of its condition, i.e. of the length of the single
  # discount_shape, so the whole vector of years was silently truncated to its
  # first element
  testthat::expect_equal(
    object =
      healthiar::get_discount_factor(
        discount_rate = 0.03,
        n_years = 0:5
      ),
    expected = 1 / (1.03 ^ (0:5))) # Results on 2026-09-09; no comparison study
})
