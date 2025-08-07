# QUANTITATIVE TEST ############################################################
## RELATIVE RISK ################################################################

### DELTA #######################################################################

testthat::test_that("results correct |pathway_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

  output_attribute_scen_1 =
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2010")

  output_attribute_scen_2 =
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
        output_attribute_scen_1 = output_attribute_scen_1,
        output_attribute_scen_2 = output_attribute_scen_2,
        approach_comparison = "delta"
      )$health_main$impact_rounded,
    expected =
      c(774, 409, 1127) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct |pathway_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

  output_attribute_scen_1 <-
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2010")

  output_attribute_scen_2 <-
    healthiar::attribute_mod(
      output_attribute = output_attribute_scen_1,
      ## What is different in scenario 2 compared to scenario 1
      exp_central = 6)

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = output_attribute_scen_1,
        output_attribute_scen_2 = output_attribute_scen_2,
        approach_comparison = "delta"
      )$health_main$impact_rounded,
    expected =
      c(774, 409, 1127) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("zero difference when scenarios are identical |meta_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

  output_attribute_scen_1 =
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2010")

  output_attribute_scen_2 =
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
        output_attribute_scen_1 = output_attribute_scen_1,
        output_attribute_scen_2 = output_attribute_scen_2,
        approach_comparison = "delta"
      )$health_main$impact_rounded,
    expected =
      c(0, 0, 0) # Results on 16 May 2024; no comparison study
  )
})

#### ITERATION ##################################################################

testthat::test_that("results correct |pathway_compare|comp_appr_delta|exp_single|iteration_TRUE|", {

  scen_1_singlebhd_rr_geo <-
    healthiar::attribute_health(
      exp_central = c(8.85, 8.0),
      cutoff_central = 5,
      bhd_central = c(25000, 20000),
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2))

  scen_2_singlebhd_rr_geo <-
    healthiar::attribute_mod(
      output_attribute = scen_1_singlebhd_rr_geo,
      # What is different in scenario 2 compared to scenario 1
      exp_central = c(6, 6.5))

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_singlebhd_rr_geo,
        output_attribute_scen_2 = scen_2_singlebhd_rr_geo
      )$health_main$impact_rounded,
    expected =
      c(1100, 582, 1603) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct |pathway_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

  scen_1_singlebhd_rr_geo_large <-
    healthiar::attribute_health(
      exp_central = c(runif_with_seed(100, 8.0, 9.0, 1)),
      exp_lower = c(runif_with_seed(100, 8.0, 9.0, 1)-0.1),
      exp_upper = c(runif_with_seed(100, 8.0, 9.0, 1)+0.1),
      cutoff_central = 5,
      bhd_central = c(runif_with_seed(100, 25000, 35000, 1)),
      bhd_lower = c(runif_with_seed(100, 25000, 35000, 1)),
      bhd_upper = c(runif_with_seed(100, 25000, 35000, 1)),
      rr_central = 1.369,
      rr_lower = 1.124,
      rr_upper = 1.664,
      rr_increment = 10,
      erf_shape = "log_linear",
      geo_id_disaggregated = 1:100,
      geo_id_aggregated = rep("CH", 100),
      info = "PM2.5_mortality_2010")

  scen_2_singlebhd_rr_geo_large <-
    healthiar::attribute_mod(
      output_attribute = scen_1_singlebhd_rr_geo_large,
      exp_central = c(runif_with_seed(100, 8.0, 9.0, 2)),
      exp_lower = c(runif_with_seed(100, 8.0, 9.0, 2)-0.1),
      exp_upper = c(runif_with_seed(100, 8.0, 9.0, 2)+0.1))

  testthat::expect_equal(
    object =
      comparison_singlebhd_rr_delta_geo <-
      healthiar::compare(
        output_attribute_scen_1 = scen_1_singlebhd_rr_geo_large,
        output_attribute_scen_2 = scen_2_singlebhd_rr_geo_large
        )$health_main$impact_rounded,
    expected =
      c(4166, 1656, 6330) # Result on 19 December 2024; no comparison study
  )
})

#### YLD ########################################################################

testthat::test_that("results correct yld |pathway_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

  scen_1_singlebhd_yld <-
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      dw_central = 0.5, dw_lower = 0.1, dw_upper = 1,
      duration_central = 1, duration_lower = 0.5, duration_upper = 10)

  scen_2_singlebhd_yld <-
    healthiar::attribute_mod(
      output_attribute = scen_1_singlebhd_yld,
      exp_central = 6)

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_singlebhd_yld,
        output_attribute_scen_2 = scen_2_singlebhd_yld
        )$health_main$impact_rounded,
    expected =
      c(387, 205, 564) # Result on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct yld |pathway_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

  scen_1_singlebhd_yld_geo <-
    healthiar::attribute_health(
      exp_central = c(8.85, 8.0),
      cutoff_central = 5,
      bhd_central = c(25000, 25000),
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      dw_central = 0.5,
      duration_central = 1,
      info = "PM2.5_yld_before",
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2))

  scen_2_singlebhd_yld_geo <-
    attribute_mod(
      output_attribute = scen_1_singlebhd_yld_geo,
      exp_central = c(6, 6.5),
      info = "PM2.5_yld_after")

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_singlebhd_yld_geo,
        output_attribute_scen_2 = scen_2_singlebhd_yld_geo
        )$health_main$impact_rounded,
    expected =
      c(591, 313, 861) # Result on 26 June 2024; no comparison study
  )
})

##### ITERATION #################################################################

testthat::test_that("results correct yld |pathway_compare|comp_appr_delta|exp_single|iteration_TRUE|", {

  scen_1_singlebhd_yld_geo <-
    healthiar::attribute_health(
      exp_central = c(8.85, 8.0),
      cutoff_central = 5,
      bhd_central = c(25000, 25000),
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      dw_central = 0.5,
      duration_central = 1,
      info = "PM2.5_yld_before",
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2))

  scen_2_singlebhd_yld_geo <-
    attribute_mod(
      output_attribute = scen_1_singlebhd_yld_geo,
      exp_central = c(6, 6.5),
      info = "PM2.5_yld_after")

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_singlebhd_yld_geo,
        output_attribute_scen_2 = scen_2_singlebhd_yld_geo
        )$health_main$impact_rounded,
    expected =
      c(591, 313, 861) # Result on 26 June 2024; no comparison study
  )
})

### PIF #########################################################################

testthat::test_that("results the same |pathway_compare|comp_appr_pif|exp_single|iteration_FALSE|", {

  output_attribute_scen_1 =
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2010")

  output_attribute_scen_2 =
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
        output_attribute_scen_1 = output_attribute_scen_1,
        output_attribute_scen_2 = output_attribute_scen_2,
        approach_comparison = "pif"
      )$health_main$impact_rounded,
    expected =
      c(782, 412, 1146) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("results the same yld |pathway_compare|comp_appr_pif|exp_single|iteration_FALSE|", {

  scen_1_singlebhd_yld <-
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      dw_central = 0.5, dw_lower = 0.1, dw_upper = 1,
      duration_central = 1, duration_lower = 0.5, duration_upper = 10)

  scen_2_singlebhd_yld <-
    healthiar::attribute_mod(
      output_attribute = scen_1_singlebhd_yld,
      exp_central = 6)

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_singlebhd_yld,
        output_attribute_scen_2 = scen_2_singlebhd_yld,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(391,206,573) # Result on 16 May 2024; no comparison study
  )
})

#### ITERATION ##################################################################

testthat::test_that("results the same |pathway_compare|comp_appr_pif|exp_single|iteration_TRUE|", {

  scen_1_singlebhd_rr_geo <-
    healthiar::attribute_health(
      exp_central = c(8.85, 8.0),
      cutoff_central = 5,
      bhd_central = c(25000, 20000),
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2))

  scen_2_singlebhd_rr_geo <-
    healthiar::attribute_mod(
      output_attribute = scen_1_singlebhd_rr_geo,
      # What is different in scenario 2 compared to scenario 1
      exp_central = c(6, 6.5))

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_singlebhd_rr_geo,
        output_attribute_scen_2 = scen_2_singlebhd_rr_geo,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(1114, 586, 1634) # Results on 19 June 2024; no comparison study
  )
})

testthat::test_that("results the same |pathway_compare|comp_appr_pif|exp_single|iteration_TRUE|", {

  scen_1_singlebhd_yld_geo <-
    healthiar::attribute_health(
      exp_central = c(8.85, 8.0),
      cutoff_central = 5,
      bhd_central = c(25000, 25000),
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      dw_central = 0.5,
      duration_central = 1,
      info = "PM2.5_yld_before",
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2))

  scen_2_singlebhd_yld_geo <-
    attribute_mod(
      output_attribute = scen_1_singlebhd_yld_geo,
      exp_central = c(6, 6.5),
      info = "PM2.5_yld_after")

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_singlebhd_yld_geo,
        output_attribute_scen_2 = scen_2_singlebhd_yld_geo,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(599, 315, 878) # Result on 20 June 2024; no comparison study
  )
})

#### YLD ########################################################################

testthat::test_that("results the same yld |pathway_compare|comp_appr_pif|exp_single|iteration_FALSE|", {

  scen_1_singlebhd_yld  <-
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      dw_central = 0.5, dw_lower = 0.1, dw_upper = 1,
      duration_central = 1, duration_lower = 0.5, duration_upper = 10)

  scen_2_singlebhd_yld <-
    healthiar::attribute_mod(
      output_attribute = scen_1_singlebhd_yld,
      exp_central = 6)

  testthat::expect_equal(
    object =
      healthiar::compare(
        approach_comparison = "pif",
        output_attribute_scen_1 = scen_1_singlebhd_yld,
        output_attribute_scen_2 = scen_2_singlebhd_yld
        )$health_main$impact_rounded,
    expected =
      c(391,206,573) # Result on 16 May 2024; no comparison study
  )
})

##### ITERATION #################################################################

testthat::test_that("results the same yld |pathway_compare|comp_appr_pif|exp_single|iteration_TRUE|", {

  scen_1_singlebhd_yld_geo <-
    healthiar::attribute_health(
      exp_central = c(8.85, 8.0),
      cutoff_central = 5,
      bhd_central = c(25000, 25000),
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      dw_central = 0.5,
      duration_central = 1,
      info = "PM2.5_yld_before",
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2))

  scen_2_singlebhd_yld_geo <-
    attribute_mod(
      output_attribute = scen_1_singlebhd_yld_geo,
      exp_central = c(6, 6.5),
      info = "PM2.5_yld_after")

  testthat::expect_equal(
    object =
      healthiar::compare(
        approach_comparison = "pif",
        output_attribute_scen_1 = scen_1_singlebhd_yld_geo,
        output_attribute_scen_2 = scen_2_singlebhd_yld_geo
        )$health_main$impact_rounded,
    expected =
      c(599, 315, 878) # Result on 20 June 2024; no comparison study
  )
})

## ABSOLUTE RISK ################################################################

## NOTE: no PIF option in AR pathway

### DELTA #######################################################################

testthat::test_that("results correct |pathway_compare|comp_appr_delta|exp_dist|iteration_FALSE|", {

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
      output_attribute = scen_1_singlebhd_ar,
      exp_central = c(50, 55, 60, 65, 75))

  testthat::expect_equal(
    object =
      comparison_singlebhd_ar_delta <-
      healthiar::compare(
        scen_1_singlebhd_ar,
        scen_2_singlebhd_ar
        )$health_main$impact_rounded,
    expected =
      c(62531) # Result on 23 May 2024; no comparison study
  )
})

#### YLD ########################################################################

testthat::test_that("results correct yld |pathway_compare|comp_appr_delta|exp_dist|iteration_FALSE|", {

  scen_1_singlebhd_yld  <-
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      dw_central = 0.5, dw_lower = 0.1, dw_upper = 1,
      duration_central = 1, duration_lower = 0.5, duration_upper = 10)

  scen_2_singlebhd_yld <-
    healthiar::attribute_mod(
      output_attribute = scen_1_singlebhd_yld,
      exp_central = 6)

  testthat::expect_equal(
    object =
      comparison_singlebhd_ar_delta <-
      healthiar::compare(
        output_attribute_scen_1 = scen_1_singlebhd_yld,
        output_attribute_scen_2 = scen_2_singlebhd_yld
        )$health_main$impact_rounded,
    expected =
      c(387, 205, 564) # Result on 16 May 2024; no comparison study
  )
})

#### ITERATION ##################################################################

testthat::test_that("results correct |pathway_compare|comp_appr_delta|exp_dist|iteration_TRUE|", {

  scen_1_singlebhd_ar_geo <-
    healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = c(c(57.5, 62.5, 67.5, 72.5, 77.5),
                      c(57, 62, 67, 72, 77)),# Fake values
      population = rep(c(945200, 929800), each = 5),
      pop_exp = c(c(387500, 286000, 191800, 72200, 7700),
                  c(380000, 280000, 190800, 72000, 7000)), # Fake values
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      info = data.frame(pollutant = "road_noise",
                        outcome = "highly_annoyance",
                        year = 2020),
      geo_id_disaggregated = rep(c("a", "b"), each = 5),
      geo_id_aggregated = rep("ch", each = 2*5)
    )

  scen_2_singlebhd_ar_geo <-
    healthiar::attribute_mod(
      output_attribute = scen_1_singlebhd_ar_geo,
      exp_central = c(c(50, 55, 60, 65, 75),
                      c(50.5, 55.5, 60.5, 65.5, 75.5)), # Fake values
      info = data.frame(pollutant = "road_noise",
                        outcome = "highly_annoyance",
                        year = 2022))

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_singlebhd_ar_geo,
        output_attribute_scen_2 = scen_2_singlebhd_ar_geo
        )$health_main$impact_rounded,
    expected =
      c(115869) # Results on 19 June 2024; no comparison study
  )
})

## LIFETABLE ####################################################################

### YLL #########################################################################

#### DELTA ######################################################################

testthat::test_that("results correct yll |pathway_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_yll_lifetable_test <-
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      exp_central = 8.85, # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100),
      age_group = base::rep(0:99, times = 2),
      bhd_central = c(data[["pop"]]$number_of_deaths_male,
              data[["pop"]]$number_of_deaths_female),
      population = c(data_lifetable[["male"]]$population,
                     data_lifetable[["female"]]$population),
      year_of_analysis = 2019,
      min_age = 20)

  scen_2_yll_lifetable_test <-
    healthiar::attribute_mod(
      output_attribute = scen_1_yll_lifetable_test,
      exp_central = 6) # Fake data just for testing purposes

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_yll_lifetable_test,
        output_attribute_scen_2 = scen_2_yll_lifetable_test
        )$health_main$impact_rounded,
    expected =
      c(21301, 11159, 31358) # Result on 7 July 2025; no comparison study to
  )
})

##### ITERATION #################################################################

testthat::test_that("results correct yll |pathway_compare|comp_appr_delta|exp_single|iteration_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_yll_lifetable_geo <-
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      exp_central = rep(c(8.85, 8.0), each = 2 * 100) , # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100, times = 2),
      age_group = base::rep(0:99, times = 2*2),
      bhd_central = base::rep(
        c(data[["pop"]]$number_of_deaths_male,
          data[["pop"]]$number_of_deaths_female),
        times = 2),
      population = base::rep(
        c(data_lifetable[["male"]]$population,
          data_lifetable[["female"]]$population),
        times = 2),
      year_of_analysis = 2019,
      min_age = 20,
      geo_id_disaggregated = rep(c("a", "b"), each = 2* 100),
      geo_id_aggregated = rep("ch", each = 2 * 2 * 100))

  scen_2_yll_lifetable_geo <-
    healthiar::attribute_mod(
      output_attribute = scen_1_yll_lifetable_geo,
      exp_central = rep(c(6, 6.5), each = 2*100)) # Fake data just for testing purposes

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_yll_lifetable_geo,
        output_attribute_scen_2 = scen_2_yll_lifetable_geo
        )$health_main$impact_rounded,
    expected =
      c(32517, 17033, 47873) # Result on 7 July 2025; no comparison study to
  )
})

#### PIF ########################################################################

testthat::test_that("results the same yll |pathway_compare|comp_appr_pif|exp_single|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_yll_lifetable <-
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      exp_central = 8.85, # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100),
      age_group = base::rep(0:99, times = 2),
      bhd_central = c(data[["pop"]]$number_of_deaths_male,
                      data[["pop"]]$number_of_deaths_female),
      population = c(data_lifetable[["male"]]$population,
                     data_lifetable[["female"]]$population),
      year_of_analysis = 2019,
      min_age = 20)

  scen_2_yll_lifetable <-
    healthiar::attribute_mod(
      output_attribute = scen_1_yll_lifetable,
      exp_central = 6) # Fake data just for testing purposes

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_yll_lifetable,
        output_attribute_scen_2 = scen_2_yll_lifetable,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(21353, 11173, 31471) # Result on 7 July 2025; no comparison study to
  )
})

##### ITERATION #################################################################

testthat::test_that("results the same yll |pathway_compare|comp_appr_pif|exp_single|iteration_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_yll_lifetable_geo <-
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      exp_central = rep(c(8.85, 8.0), each = 2 * 100) , # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100, times = 2),
      age_group = base::rep(0:99, times = 2*2),
      bhd_central = base::rep(
        c(data[["pop"]]$number_of_deaths_male,
          data[["pop"]]$number_of_deaths_female),
        times = 2),
      population = base::rep(
        c(data_lifetable[["male"]]$population,
          data_lifetable[["female"]]$population),
        times = 2),
      year_of_analysis = 2019,
      min_age = 20,
      geo_id_disaggregated = rep(c("a", "b"), each = 2* 100),
      geo_id_aggregated = rep("ch", each = 2 * 2 * 100))

  scen_2_yll_lifetable_geo <-
    healthiar::attribute_mod(
      output_attribute = scen_1_yll_lifetable_geo,
      exp_central = rep(c(6, 6.5), each = 2 * 100)) # Fake data just for testing purposes

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_yll_lifetable_geo,
        output_attribute_scen_2 = scen_2_yll_lifetable_geo,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(32609, 17058, 48074) # Result on 7 July 2025; no comparison study to
  )
})

### PREMATURE DEATHS ############################################################

#### DELTA ######################################################################

testthat::test_that("results correct |pathway_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_deaths_lifetable <-
    healthiar::attribute_lifetable(
      health_outcome = "deaths",
      exp_central = 8.85, # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100),
      age_group = base::rep(0:99, times = 2),
      bhd_central = c(data[["pop"]]$number_of_deaths_male,
                      data[["pop"]]$number_of_deaths_female),
      population = c(data_lifetable[["male"]]$population,
                     data_lifetable[["female"]]$population),
      year_of_analysis = 2019,
      min_age = 20)

  scen_2_deaths_lifetable <-
    healthiar::attribute_mod(
      output_attribute = scen_1_deaths_lifetable,
      exp_central = 6)

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_deaths_lifetable,
        output_attribute_scen_2 = scen_2_deaths_lifetable
        )$health_main$impact_rounded,
    expected =
      c(1914, 1012, 2793) # Result on 7 July 2025; no comparison study to
  )
})

##### ITERATION #################################################################

testthat::test_that("results correct d|pathway_compare|comp_appr_delta|exp_single|iteration_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_deaths_lifetable_geo <-
    healthiar::attribute_lifetable(
      health_outcome = "deaths",
      exp_central = rep(c(8.85, 8.0), each = 2 * 100) , # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100, times = 2),
      age_group = base::rep(0:99, times = 2*2),
      bhd_central = base::rep(
        c(data[["pop"]]$number_of_deaths_male,
          data[["pop"]]$number_of_deaths_female),
        times = 2),
      population = base::rep(
        c(data_lifetable[["male"]]$population,
          data_lifetable[["female"]]$population),
        times = 2),
      year_of_analysis = 2019,
      min_age = 20,
      geo_id_disaggregated = rep(c("a", "b"), each = 2* 100),
      geo_id_aggregated = rep("ch", each = 2 * 2 * 100))

  scen_2_deaths_lifetable_geo <-
    healthiar::attribute_mod(
      output_attribute = scen_1_deaths_lifetable_geo,
      exp_central = rep(c(6, 6.5), each = 2 * 100)) # Fake data just for testing purposes

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_deaths_lifetable_geo,
        output_attribute_scen_2 = scen_2_deaths_lifetable_geo
        )$health_main$impact_rounded,
    expected =
      c(2924, 1545, 4267) # Result on 7 July 2025; no comparison study to
  )
})

#### PIF ########################################################################

testthat::test_that("results the same |pathway_compare|comp_appr_pif|exp_single|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_deaths_lifetable <-
    healthiar::attribute_lifetable(
      health_outcome = "deaths",
      exp_central = 8.85, # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100),
      age_group = base::rep(0:99, times = 2),
      bhd_central = c(data[["pop"]]$number_of_deaths_male,
                      data[["pop"]]$number_of_deaths_female),
      population = c(data_lifetable[["male"]]$population,
                     data_lifetable[["female"]]$population),
      year_of_analysis = 2019,
      min_age = 20)

  scen_2_deaths_lifetable <-
    healthiar::attribute_mod(
      output_attribute = scen_1_deaths_lifetable,
      exp_central = 6)

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_deaths_lifetable,
        output_attribute_scen_2 = scen_2_deaths_lifetable,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(1934, 1017, 2836) # Result on 7 July 2025; no comparison study to
  )
})

##### ITERATION #################################################################

testthat::test_that("results the same |pathway_compare|comp_appr_pif|exp_single|iteration_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_deaths_lifetable_geo <-
    healthiar::attribute_lifetable(
      health_outcome = "deaths",
      exp_central = rep(c(8.85, 8.0), each = 2 * 100) , # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100, times = 2),
      age_group = base::rep(0:99, times = 2*2),
      bhd_central = base::rep(
        c(data[["pop"]]$number_of_deaths_male,
          data[["pop"]]$number_of_deaths_female),
        times = 2),
      population = base::rep(
        c(data_lifetable[["male"]]$population,
          data_lifetable[["female"]]$population),
        times = 2),
      year_of_analysis = 2019,
      min_age = 20,
      geo_id_disaggregated = rep(c("a", "b"), each = 2* 100),
      geo_id_aggregated = rep("ch", each = 2 * 2 * 100))

  scen_2_deaths_lifetable_geo <-
    healthiar::attribute_mod(
      output_attribute = scen_1_deaths_lifetable_geo,
      exp_central = rep(c(6, 6.5), each = 100 * 2)) # Fake data just for testing purposes

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_deaths_lifetable_geo,
        output_attribute_scen_2 = scen_2_deaths_lifetable_geo,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(2959, 1555, 4343) # Result on 7 July 2025; no comparison study to
  )
})

testthat::test_that("results the same Sciensano tobacco example |pathway_compare|comp_appr_pif|exp_single|iteration_FALSE|", {

  data <- readRDS(testthat::test_path("data/rr_data.RDS"))
  data <- subset(data, CAUSE == "Chronic obstructive pulmonary disease" & EXPOSURE == 'PACK_YEAR')

  output_attribute_scen_1 =
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      exp_central = 6.167882,
      cutoff_central = 0,
      erf_eq_central = approxfun(data$UNITS, data$RR, rule = 2),
      bhd_central = 1000)

  output_attribute_scen_2 =
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      exp_central = 3.167488,
      cutoff_central = 0,
      erf_eq_central = approxfun(data$UNITS, data$RR, rule = 2),
      bhd_central = 1000)

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = output_attribute_scen_1,
        output_attribute_scen_2 = output_attribute_scen_2,
        approach_comparison = "pif"
      )$health_main$impact_rounded,
    expected = 231
  )
})

testthat::test_that("results the same Sciensano tobacco example |pathway_compare|comp_appr_pif|exp_dist|iteration_FALSE|", {

  data <- readRDS(testthat::test_path("data/rr_data.RDS"))
  data <- subset(data, CAUSE == "Chronic obstructive pulmonary disease" & EXPOSURE == 'PACK_YEAR')

  output_attribute_scen_1 =
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      exp_central = c(23.68696, 0),
      cutoff_central = 0,
      prop_pop_exp = c(0.2603914, 0.7396086),
      erf_eq_central = approxfun(data$UNITS, data$RR, rule = 2),
      bhd_central = 1000)

  output_attribute_scen_2 =
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      exp_central = c(17.19273, 0),
      cutoff_central = 0,
      prop_pop_exp = c(0.1842342, 0.8157658),
      erf_eq_central = approxfun(data$UNITS, data$RR, rule = 2),
      bhd_central = 1000)

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = output_attribute_scen_1,
        output_attribute_scen_2 = output_attribute_scen_2,
        approach_comparison = "pif"
      )$health_main$impact_rounded,
    expected = 226
  )
})

testthat::test_that("results the same Sciensano tobacco example |pathway_compare|comp_appr_pif|exp_single|iteration_TRUE|", {

  data <- readRDS(testthat::test_path("data/rr_data.RDS"))
  data <- subset(data, CAUSE == "Chronic obstructive pulmonary disease" & EXPOSURE == 'PACK_YEAR')

  output_attribute_scen_1 =
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      exp_central = c(6.848995, 6.565633, 6.167882),
      cutoff_central = 0,
      erf_eq_central = approxfun(data$UNITS, data$RR, rule = 2),
      bhd_central = c(650, 1200, 1000),
      geo_id_disaggregated = c('BR', 'FL', 'WA'))

  output_attribute_scen_2 =
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      exp_central = c(3.125626, 2.948348, 3.167488),
      cutoff_central = 0,
      erf_eq_central = approxfun(data$UNITS, data$RR, rule = 2),
      bhd_central = c(650, 1200, 1000),
      geo_id_disaggregated = c('BR', 'FL', 'WA'))

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = output_attribute_scen_1,
        output_attribute_scen_2 = output_attribute_scen_2,
        approach_comparison = "pif"
      )$health_main$impact_rounded,
    expected = c(177, 325, 231)
  )
})

testthat::test_that("results the same Sciensano tobacco example |pathway_compare|comp_appr_pif|exp_dist|iteration_TRUE|", {

  data <- readRDS(testthat::test_path("data/rr_data.RDS"))
  data <- subset(data, CAUSE == "Chronic obstructive pulmonary disease" & EXPOSURE == 'PACK_YEAR')

  output_attribute_scen_1 =
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      exp_central = c(c(0, 26.79813), c(0, 25.89477), c(0, 23.68696)),
      cutoff_central = 0,
      prop_pop_exp = c(c(0.7444227, 0.2555773), c(0.7464495, 0.2535505), c(0.7396086, 0.2603914)),
      erf_eq_central = approxfun(data$UNITS, data$RR, rule = 2),
      bhd_central = rep(c(650, 1200, 1000), each = 2),
      geo_id_disaggregated = rep(c('BR', 'FL', 'WA'), each = 2))

  output_attribute_scen_2 =
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      exp_central = c(c(0, 19.22196), c(0, 17.91513), c(0, 17.19273)),
      cutoff_central = 0,
      prop_pop_exp = c(c(0.8373929, 0.1626071), c(0.8354269, 0.1645731), c(0.8157658, 0.1842342)),
      erf_eq_central = approxfun(data$UNITS, data$RR, rule = 2),
      bhd_central = rep(c(650, 1200, 1000), each = 2),
      geo_id_disaggregated = rep(c('BR', 'FL', 'WA'), each = 2))

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_scen_1 = output_attribute_scen_1,
        output_attribute_scen_2 = output_attribute_scen_2,
        approach_comparison = "pif"
      )$health_main$impact_rounded,
    expected = c(173, 317, 226)
  )
})

# ERROR OR WARNING ########
## ERROR #########
testthat::test_that("error if not the same arguments", {

  output_attribute_scen_1 =
    healthiar::attribute_health(
      exp_central = 8.85,
      exp_lower = 7.75,
      exp_upper = 9,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2010")

  output_attribute_scen_2 =
    healthiar::attribute_health(
      exp_central = 6,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2020")

  testthat::expect_error(
    object =
      healthiar::compare(
        output_attribute_scen_1 = output_attribute_scen_1,
        output_attribute_scen_2 = output_attribute_scen_2,
        approach_comparison = "delta"),
    regexp = "The two scenarios must use the same arguments.")
})

testthat::test_that("error if common arguments with different value", {

  output_attribute_scen_1 =
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2010")

  output_attribute_scen_2 =
    healthiar::attribute_health(
      exp_central = 6,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.15, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2020")

  testthat::expect_error(
    object =
      healthiar::compare(
        output_attribute_scen_1 = output_attribute_scen_1,
        output_attribute_scen_2 = output_attribute_scen_2,
        approach_comparison = "delta"),
    regexp = "rr_central must be identical in both scenarios.")
})


testthat::test_that("error pif and different bhd", {

  output_attribute_scen_1 =
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2010")

  output_attribute_scen_2 =
    healthiar::attribute_health(
      exp_central = 6,
      cutoff_central = 5,
      bhd_central = 1000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2020")

  testthat::expect_error(
    object =
      healthiar::compare(
        output_attribute_scen_1 = output_attribute_scen_1,
        output_attribute_scen_2 = output_attribute_scen_2,
        approach_comparison = "pif"),
    regexp = "For the PIF approach, bhd must be identical in both scenarios.")
})

testthat::test_that("error pif and absolute risk", {

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
      output_attribute = scen_1_singlebhd_ar,
      exp_central = c(50, 55, 60, 65, 75))

  testthat::expect_error(
    object =
      healthiar::compare(
        output_attribute_scen_1 = scen_1_singlebhd_ar,
        output_attribute_scen_2 = scen_2_singlebhd_ar,
        approach_comparison = "pif"),
    regexp = "For the PIF approach, the absolute risk approach cannot be used.")
})




## WARNING #########
