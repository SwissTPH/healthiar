testthat::test_that("results correct", {

  municip <- sf::st_read(testthat::test_path("data", "municipalities_brussels.gpkg"), quiet = TRUE)
  pm25 <- terra::rast(testthat::test_path("data", "pm25.tif"))
  pop <- terra::rast(testthat::test_path("data", "population.tif"))
  results <- utils::read.csv(testthat::test_path("data", "exp_grid_results.csv"))

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

  municip <- sf::st_read(testthat::test_path("data", "municipalities_brussels.gpkg"), quiet = TRUE)
  pm25 <- terra::rast(testthat::test_path("data", "pm25.tif"))
  results <- utils::read.csv(testthat::test_path("data", "exp_pwm_results.csv"))

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

  municip <- sf::st_read(testthat::test_path("data", "municipalities_brussels.gpkg"), quiet = TRUE)
  pm25 <- terra::rast(testthat::test_path("data", "pm25.tif"))
  results <- utils::read.csv(testthat::test_path("data", "exp_simple_results.csv"))

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
