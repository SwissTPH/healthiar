testthat::test_that("results correct", {

  municip <- exdat_pwm_2
  pm25 <- terra::rast(system.file("extdata", "exdat_pwm_1.tif", package = "healthiar"))
  pop <- terra::rast(testthat::test_path("testdata", "population.tif"))
  results <- utils::read.csv(testthat::test_path("testdata", "expected_exposure_grid.csv"))

  testthat::expect_equal(
    object =
      healthiar::prepare_exposure(
        poll_grid = pm25,
        geo_units = municip,
        pop_grid = pop,
        geo_id_micro = sf::st_drop_geometry(municip$name)
      )$exposure_main$exposure_mean,
    expect = results$exposure
  )
})

testthat::test_that("results correct", {

  municip <- exdat_pwm_2
  pm25 <- terra::rast(system.file("extdata", "exdat_pwm_1.tif", package = "healthiar"))
  results <- utils::read.csv(testthat::test_path("testdata", "expected_exposure_pwm.csv"))

  testthat::expect_equal(
    object =
      healthiar::prepare_exposure(
        poll_grid = pm25,
        geo_units = municip,
        population = sf::st_drop_geometry(municip$population),
        geo_id_macro = sf::st_drop_geometry(municip$region)
      )$exposure_main$exposure_mean,
    expect = results$exposure
  )
})

testthat::test_that("results correct", {

  municip <- exdat_pwm_2
  pm25 <- terra::rast(system.file("extdata", "exdat_pwm_1.tif", package = "healthiar"))
  results <- utils::read.csv(testthat::test_path("testdata", "expected_exposure_simple.csv"))

  testthat::expect_equal(
    object =
      healthiar::prepare_exposure(
        poll_grid = pm25,
        geo_units = municip,
        geo_id_micro = sf::st_drop_geometry(municip$name)
      )$exposure_main$exposure_mean,
    expect = results$exposure
  )
})
