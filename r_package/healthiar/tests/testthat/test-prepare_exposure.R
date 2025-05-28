testthat::test_that("results correct", {

  sector <- sf::st_read(testthat::test_path("data", "sector_brussels.gpkg"), quiet = TRUE)
  pm_be <- terra::rast(testthat::test_path("data", "pm25_brussels_region_2020.tif"))
  results <- utils::read.csv(testthat::test_path("data", "exp_pwm_results.csv"))

  testthat::expect_equal(
    object =
      healthiar::prepare_exposure(
        poll_grid = pm_be,
        geo_units = sector,
        population = sf::st_drop_geometry(sector$POP),
        geo_id_aggregated = sf::st_drop_geometry(sector$MUNICIP)
      )$main$exposure_value,
    expect = results$EXP_BE
  )
})

testthat::test_that("results correct", {

  sector <- sf::st_read(testthat::test_path("data", "sector_brussels.gpkg"), quiet = TRUE)
  pm_eu <- terra::rast(testthat::test_path("data", "sce0.nc"))
  results <- utils::read.csv(testthat::test_path("data", "exp_pwm_results.csv"))

  testthat::expect_equal(
    object =
      suppressWarnings( # 2025-05-21 AL: added to prevent failure of test
        healthiar::prepare_exposure(
        poll_grid = pm_eu$SURF_ug_PM25_rh50,
        geo_units = sector,
        population = sf::st_drop_geometry(sector$POP),
        geo_id_aggregated = sf::st_drop_geometry(sector$MUNICIP)
      )$main$exposure_value
      ),
    expect = results$EXP_EU
  )
})
