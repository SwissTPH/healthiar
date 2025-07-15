testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::prepare_lifetable(
        age_group = c(0, 5, 10, 15),
        population = c(3387900, 3401300, 3212300, 3026100),
        bhd = c(4727, 472, 557, 1323))$bhd_for_attribute |>
      base::round(),
    expected =
      # Example from AirQ+ Life Table Manual
      c(948, 947, 945, 944, 943,
        94, 94, 94, 94, 94,
        111, 111, 111, 111, 111,
        265, 265, 265, 264, 264)
  )
})
