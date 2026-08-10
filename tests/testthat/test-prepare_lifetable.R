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

# Now with different franction_lived depending on the age
testthat::test_that("results the same|prepare_lifetable", {

  testthat::expect_equal(
    object =
      healthiar::prepare_lifetable(
        age_group = c(0, 5, 10, 15),
        population = c(3387900, 3401300, 3212300, 3026100),
        bhd = c(4727, 472, 557, 1323),
        fraction_lived = c(0.1, 0.5, 0.5, 0.5))$bhd_for_attribute |>
      base::round(),
    expected = c(
      4254, 118, 118, 118, 118,  # Ages 0-4  
      94,  94,  94,  94,  94,  # Ages 5-9 
      111, 111, 111, 111, 111,  # Ages 10-14
      265, 265, 265, 264, 264   # Ages 15-19
  ))
})

# Benchmark test derived from WHO AirQ+ methodology manual dataset
# using standard Chiang 1984 low-mortality infant fraction (a0 = 0.1)
testthat::test_that("results the same|prepare_lifetable", {

  testthat::expect_equal(
    object =
      healthiar::prepare_lifetable(
        age_group = c(0, 5, 10, 15),
        population = c(3387900, 3401300, 3212300, 3026100),
        bhd = c(4727, 472, 557, 1323),
        fraction_lived = c(0.1, 0.5, 0.5, 0.5))$bhd_for_attribute |>
      base::round(),
    expected = c(
      4254, 118, 118, 118, 118,  # Ages 0-4   
       94,  94,  94,  94,  94,  # Ages 5-9   
       111, 111, 111, 111, 111,  # Ages 10-14 
       265, 265, 265, 264, 264   # Ages 15-19 
  ))
})

# Now with 10-year age intervals and custom infant fraction_lived (a0 = 0.1)
testthat::test_that("results the same|prepare_lifetable", {

  testthat::expect_equal(
    object =
      healthiar::prepare_lifetable(
        age_group = c(0, 10, 20),
        population = c(2000000, 1800000, 1600000),
        bhd = c(10000, 2000, 5000),
        fraction_lived = c(0.1, 0.5, 0.5))$bhd_for_attribute |>
      base::round(),
    expected = c(
      # Group [0, 10) 
      9000, 111, 111, 111, 111, 111, 111, 111, 111, 111,
      
      # Group [10, 20) 
       201, 201, 201, 200, 200, 200, 200, 199, 199, 199,
      
      # Group [20, 30) 
       507, 506, 504, 502, 501, 499, 498, 496, 495, 493
  ))
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
      "The values of age_group must be greater than or equal to 0."
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
      "The values of population must be greater than 0."
  )
})


testthat::test_that("error if lenght different|prepare_lifetable", {

  testthat::expect_error(
    object =
      healthiar::prepare_lifetable(
        age_group = c(0, 5, 10, 15),
        population = c(3387900, 3401300, 3212300, 3026100),
        bhd = c(4727, 472, 557, 1323),
        fraction_lived = 1.1 # This value should not be higher than 1
        )$bhd_for_attribute |>
      base::round(),
    regexp =
      "The values of fraction_lived cannot be greater than 1."
  )
})



## WARNING #########
