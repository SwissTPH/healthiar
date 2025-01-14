test_that("linear rescaling works", {
  testthat::expect_equal(
    object = healthiar::get_risk(
      rr = 1.1,
      erf_shape = "linear",
      cutoff = 5,
      exp = 10,
      erf_increment = 10
    ),
    expected = 1.05
    )
}
)
