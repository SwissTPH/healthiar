# RELATIVE RISK ################################################################

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

testthat::test_that("results correct delta comparison iteration (high number of geo units) rr single exposures", {

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
      comparison_singlebhd_rr_delta_geo <-
      healthiar::compare(
        output_attribute_1 = scen_1_singlebhd_rr_geo,
        output_attribute_2 = scen_2_singlebhd_rr_geo) |>
      helper_extract_main_results(),
    expected =
      c(1100, 582, 1603) # Result on 26 June 2024; no comparison study
  )
})

# ABSOLUTE RISK ################################################################

testthat::test_that("results correct delta comparison ar exposure distribution", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  scen_1_singlebhd_ar <-
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = data$exposure_mean,
        pop_exp = data$population_exposed_total,
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance"))

  scen_2_singlebhd_ar <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_singlebhd_ar,
      exp_central = c(50, 55, 60, 65, 75))

  testthat::expect_equal(
    object =
      comparison_singlebhd_ar_delta <-
      healthiar::compare(
        scen_1_singlebhd_ar,
        scen_2_singlebhd_ar) |>
      helper_extract_main_results(),
    expected =
      c(62531) # Result on 23 May 2024; no comparison study
  )
})

testthat::test_that("results correct delta comparison iteration ar exposure distribution", {

  scen_1_singlebhd_ar_geo <-
    healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = list(c(57.5, 62.5, 67.5, 72.5, 77.5),
                         c(57, 62, 67, 72, 77)),# Fake values
      population = list(945200, 929800),
      pop_exp= list(c(387500, 286000, 191800, 72200, 7700),
                    c(380000, 280000, 190800, 72000, 7000)), # Fake values
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      info = data.frame(pollutant = "road_noise",
                        outcome = "highly_annoyance",
                        year = 2020),
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2)
    )

  scen_2_singlebhd_ar_geo <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_singlebhd_ar_geo,
      exp_central = list(c(50, 55, 60, 65, 75),
                         c(50.5, 55.5, 60.5, 65.5, 75.5)), # Fake values
      info = data.frame(pollutant = "road_noise",
                        outcome = "highly_annoyance",
                        year = 2022))

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_singlebhd_ar_geo,
        output_attribute_2 = scen_2_singlebhd_ar_geo) |>
      helper_extract_main_results(),
    expected =
      c(115869) # Results on 19 June 2024; no comparison study
  )
})
