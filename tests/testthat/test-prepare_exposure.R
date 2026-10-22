testthat::test_that("results correct", {

  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("exactextractr")

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

  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("exactextractr")

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

  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("exactextractr")

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

testthat::test_that("results the same |prepare_exposure|bins_cover_the_whole_range|", {

  # The bins are defined from the minimum and the maximum of the pollutant.
  # Rounding them could place the lowest break above the minimum and the
  # highest break below the maximum, and cut(right = FALSE) then left those
  # cells without a bin. In the pathway with gridded population they appeared
  # in the results with an exposure of NA, and in the pathway with a
  # population vector they were dropped together with their population
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("exactextractr")

  # A pollutant whose minimum and maximum are deliberately not on a bin edge
  poll_grid <- terra::rast(nrows = 4, ncols = 4, xmin = 0, xmax = 4,
                           ymin = 0, ymax = 4, crs = "EPSG:3035")
  terra::values(poll_grid) <- seq(5.02, 9.94, length.out = 16)

  pop_grid <- poll_grid
  terra::values(pop_grid) <- rep(100, 16)

  polygon_of <- function(x, y, size){
    sf::st_polygon(list(rbind(
      c(x, y), c(x + size, y), c(x + size, y + size), c(x, y + size), c(x, y))))
  }

  # * With gridded population: no cell may end up without a bin ###############

  geo_unit <- sf::st_as_sf(sf::st_sfc(polygon_of(0, 0, 4), crs = "EPSG:3035"))

  exposure_with_pop_grid <-
    healthiar::prepare_exposure(
      poll_grid = poll_grid,
      pop_grid = pop_grid,
      geo_units = geo_unit,
      geo_id_micro = "a",
      bin_width = 0.1)

  testthat::expect_equal(
    object =
      sum(is.na(exposure_with_pop_grid$exposure_detailed$exposure_mid)),
    expected = 0)

  # * With a population vector: no population may be lost #####################

  # One sub-unit covers the cell with the maximum, which used to fall outside
  # the bins and was therefore dropped with its whole population
  geo_sub_units <-
    sf::st_as_sf(sf::st_sfc(polygon_of(0, 3, 1),   # cell with the minimum
                            polygon_of(3, 0, 1),   # cell with the maximum
                            polygon_of(1, 1, 1),
                            crs = "EPSG:3035"))
  population <- c(500, 700, 300)

  exposure_with_population <-
    healthiar::prepare_exposure(
      poll_grid = poll_grid,
      geo_units = geo_sub_units,
      population = population,
      geo_id_macro = rep("M", 3),
      bin_width = 0.1)

  testthat::expect_equal(
    object = sum(exposure_with_population$exposure_detailed$population),
    expected = sum(population))
})
