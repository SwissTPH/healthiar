# QUANTITATIVE TEST ############################################################

testthat::test_that("working with single exposure", {


  testthat::expect_equal(
    object = healthiar::get_paf(
      rr_at_exp = 1.2, 
      prop_pop_exp = 1),
    expected = 0.166666667
    )

  }
)

testthat::test_that("working with exposure distribution", {


  testthat::expect_equal(
    object = healthiar::get_paf(
      rr_at_exp = c(1.1, 1.2), 
      prop_pop_exp = c(0.5, 0.5)),
    expected = 0.130434783
    )

  }
)