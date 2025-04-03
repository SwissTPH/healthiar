testthat::test_that("results correct delta comparison rr single exposure", {

  output_attribute_1 =
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2010")

  output_attribute_2 =
    healthiar::attribute_health(
      exp_central = 6,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2020")



  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = output_attribute_1,
        output_attribute_2 = output_attribute_2,
        approach_comparison = "delta"
      ) |>
      helper_extract_main_results(),
    expected =
      c(774, 409, 1127) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct delta comparison when two scenarios are identical", {

  output_attribute_1 =
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2010")

  output_attribute_2 =
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2020")

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = output_attribute_1,
        output_attribute_2 = output_attribute_2,
        approach_comparison = "delta"
      ) |>
      helper_extract_main_results(),
    expected =
      c(0, 0, 0) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct delta comparison iteration rr single exposures", {

  scen_1_singlebhd_rr_geo <-
    healthiar::attribute_health(
      exp_central = list(8.85, 8.0),
      cutoff_central = 5,
      bhd_central = list(25000, 20000),
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2))

  scen_2_singlebhd_rr_geo <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_singlebhd_rr_geo,
      # What is different in scenario 2 compared to scenario 1
      exp_central = list(6, 6.5))

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_singlebhd_rr_geo,
        output_attribute_2 = scen_2_singlebhd_rr_geo) |>
      helper_extract_main_results(),
    expected =
      c(1100, 582, 1603) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct pif comparison rr single exposure", {

  output_attribute_1 =
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2010")

  output_attribute_2 =
    healthiar::attribute_health(
      exp_central = 6,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2020")

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = output_attribute_1,
        output_attribute_2 = output_attribute_2,
        approach_comparison = "pif"
        ) |>
      helper_extract_main_results(),
    expected =
      c(782, 412, 1146) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct pif comparison iteration rr single exposures", {

  scen_1_singlebhd_rr_geo <-
    healthiar::attribute_health(
      exp_central = list(8.85, 8.0),
      cutoff_central = 5,
      bhd_central = list(25000, 20000),
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2))

  scen_2_singlebhd_rr_geo <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_singlebhd_rr_geo,
      # What is different in scenario 2 compared to scenario 1
      exp_central = list(6, 6.5))

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_singlebhd_rr_geo,
        output_attribute_2 = scen_2_singlebhd_rr_geo,
        approach_comparison = "pif") |>
      helper_extract_main_results(),
    expected =
      c(1114, 586, 1634) # Results on 19 June 2024; no comparison study
  )
})
