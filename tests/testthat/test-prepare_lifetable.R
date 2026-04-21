# QUANTITATIVE TEST ############################################################

testthat::test_that("results correct|prepare_lifetable", {

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

testthat::test_that("fractional positive values work|prepare_lifetable", {

  testthat::expect_no_error(
    out <- healthiar::prepare_lifetable(
      age_group = c(0, 2),
      population = c(0.8, 1.6),
      bhd = c(0.2, 0.4)
    )
  )

  testthat::expect_equal(nrow(out), 4)
  testthat::expect_equal(sum(out$bhd_for_attribute), 0.6, tolerance = 1e-10)
  testthat::expect_true(all(is.finite(out$population_for_attribute)))
  testthat::expect_true(all(is.finite(out$bhd_for_attribute)))
  testthat::expect_true(all(out$population_for_attribute > 0))
  testthat::expect_true(all(out$bhd_for_attribute >= 0))
})

# ERROR OR WARNING ########
## ERROR #########

testthat::test_that("error if lenght different|prepare_lifetable", {

  testthat::expect_error(
    object =
      healthiar::prepare_lifetable(
        age_group = c(0, 5, 10, 15, 20), # 20 should not be there
        population = c(3387900, 3401300, 3212300, 3026100),
        bhd = c(4727, 472, 557, 1323))$bhd_for_attribute |>
      base::round(),
    regexp =
      "The following variables must all have the same length: age_group, population, bhd."
  )
})

testthat::test_that("error if lower than min for age_group|prepare_lifetable", {

  testthat::expect_error(
    object =
      healthiar::prepare_lifetable(
        age_group = c(-1, 5, 10, 15),
        population = c(3387900, 3401300, 3212300, 3026100),# 0 should not be there
        bhd = c(4727, 472, 557, 1323))$bhd_for_attribute |>
      base::round(),
    regexp =
      "The values of age_group cannot be lower than 0."
  )
})


testthat::test_that("error if lower than min for population|prepare_lifetable", {

  testthat::expect_error(
    object =
      healthiar::prepare_lifetable(
        age_group = c(0, 5, 10, 15),
        population = c(0, 3401300, 3212300, 3026100),# 0 should not be there
        bhd = c(4727, 472, 557, 1323))$bhd_for_attribute |>
      base::round(),
    regexp =
      "The values of population cannot be lower than"
  )
})

## NOTE 2025-09-09 AL: commented out this test as decided that enabling fraction_lived is a future development.
# testthat::test_that("error if lenght different|prepare_lifetable", {
#
#   testthat::expect_error(
#     object =
#       healthiar::prepare_lifetable(
#         age_group = c(0, 5, 10, 15),
#         population = c(3387900, 3401300, 3212300, 3026100),
#         bhd = c(4727, 472, 557, 1323),
#         fraction_lived = 1.1 # This value should not be higher than 1
#         )$bhd_for_attribute |>
#       base::round(),
#     regexp =
#       "The values of fraction_lived cannot be higher than 1."
#   )
# })



## WARNING #########
