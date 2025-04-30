# RELATIVE RISK ################################################################

## DELTA #######################################################################

testthat::test_that("results correct |pathway_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

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
      )$health_main$impact_rounded,
    expected =
      c(774, 409, 1127) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct |pathway_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

  output_attribute_1 <-
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = 25000,
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
      rr_increment = 10,
      info = "PM2.5_mortality_2010")

  output_attribute_2 <-
    healthiar::attribute_mod(
      output_attribute_1 = output_attribute_1,
      ## What is different in scenario 2 compared to scenario 1
      exp_central = 6)

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = output_attribute_1,
        output_attribute_2 = output_attribute_2,
        approach_comparison = "delta"
      )$health_main$impact_rounded,
    expected =
      c(774, 409, 1127) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("zero difference when scenarios are identical |meta_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

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
      )$health_main$impact_rounded,
    expected =
      c(0, 0, 0) # Results on 16 May 2024; no comparison study
  )
})

### ITERATION ##################################################################

testthat::test_that("results correct |pathway_compare|comp_appr_delta|exp_single|iteration_TRUE|", {

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
        output_attribute_2 = scen_2_singlebhd_rr_geo
      )$health_main$impact_rounded,
    expected =
      c(1100, 582, 1603) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct |pathway_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

  scen_1_singlebhd_rr_geo_large <-
    healthiar::attribute_health(
      exp_central = as.list(runif_with_seed(1E4, 8.0, 9.0, 1)),
      exp_lower = as.list(runif_with_seed(1E4, 8.0, 9.0, 1)-0.1),
      exp_upper = as.list(runif_with_seed(1E4, 8.0, 9.0, 1)+0.1),
      cutoff_central = 5,
      bhd_central = as.list(runif_with_seed(1E4, 25000, 35000, 1)),
      bhd_lower = as.list(runif_with_seed(1E4, 25000, 35000, 1)),
      bhd_upper = as.list(runif_with_seed(1E4, 25000, 35000, 1)),
      rr_central = 1.369,
      rr_lower = 1.124,
      rr_upper = 1.664,
      rr_increment = 10,
      erf_shape = "log_linear",
      geo_id_disaggregated = 1:1E4,
      geo_id_aggregated = rep("CH", 1E4),
      info = "PM2.5_mortality_2010")

  scen_2_singlebhd_rr_geo_large <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_singlebhd_rr_geo_large,
      exp_central = as.list(runif_with_seed(1E4, 8.0, 9.0, 2)),
      exp_lower = as.list(runif_with_seed(1E4, 8.0, 9.0, 2)-0.1),
      exp_upper = as.list(runif_with_seed(1E4, 8.0, 9.0, 2)+0.1))

  testthat::expect_equal(
    object =
      comparison_singlebhd_rr_delta_geo <-
      healthiar::compare(
        output_attribute_1 = scen_1_singlebhd_rr_geo_large,
        output_attribute_2 = scen_2_singlebhd_rr_geo_large
        )$health_main$impact_rounded,
    expected =
      c(211111, 84203, 319618) # Result on 19 December 2024; no comparison study
  )
})

### YLD ########################################################################

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
      dw_central = 0.5, dw_lower = 0.1, dw_upper = 10,
      duration_central = 1, duration_lower = 0.5, duration_upper = 10)

  scen_2_singlebhd_yld <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_singlebhd_yld,
      exp_central = 6)

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_singlebhd_yld,
        output_attribute_2 = scen_2_singlebhd_yld
        )$health_main$impact_rounded,
    expected =
      c(387, 205, 564) # Result on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct yld |pathway_compare|comp_appr_delta|exp_single|iteration_FALSE|", {

  scen_1_singlebhd_yld_geo <-
    healthiar::attribute_health(
      exp_central = list(8.85, 8.0),
      cutoff_central = 5,
      bhd_central = 25000,
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
      output_attribute_1 = scen_1_singlebhd_yld_geo,
      exp_central = list(6, 6.5),
      info = "PM2.5_yld_after")

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_singlebhd_yld_geo,
        output_attribute_2 = scen_2_singlebhd_yld_geo
        )$health_main$impact_rounded,
    expected =
      c(591, 313, 861) # Result on 26 June 2024; no comparison study
  )
})

#### ITERATION #################################################################

testthat::test_that("results correct yld |pathway_compare|comp_appr_delta|exp_single|iteration_TRUE|", {

  scen_1_singlebhd_yld_geo <-
    healthiar::attribute_health(
      exp_central = list(8.85, 8.0),
      cutoff_central = 5,
      bhd_central = 25000,
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
      output_attribute_1 = scen_1_singlebhd_yld_geo,
      exp_central = list(6, 6.5),
      info = "PM2.5_yld_after")

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_singlebhd_yld_geo,
        output_attribute_2 = scen_2_singlebhd_yld_geo
        )$health_main$impact_rounded,
    expected =
      c(591, 313, 861) # Result on 26 June 2024; no comparison study
  )
})

## PIF #########################################################################

testthat::test_that("results the same |pathway_compare|comp_appr_pif|exp_single|iteration_FALSE|", {

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
      dw_central = 0.5, dw_lower = 0.1, dw_upper = 10,
      duration_central = 1, duration_lower = 0.5, duration_upper = 10)

  scen_2_singlebhd_yld <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_singlebhd_yld,
      exp_central = 6)

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_singlebhd_yld,
        output_attribute_2 = scen_2_singlebhd_yld,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(391,206,573) # Result on 16 May 2024; no comparison study
  )
})

### ITERATION ##################################################################

testthat::test_that("results the same |pathway_compare|comp_appr_pif|exp_single|iteration_TRUE|", {

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
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(1114, 586, 1634) # Results on 19 June 2024; no comparison study
  )
})

testthat::test_that("results the same |pathway_compare|comp_appr_pif|exp_single|iteration_TRUE|", {

  scen_1_singlebhd_yld_geo <-
    healthiar::attribute_health(
      exp_central = list(8.85, 8.0),
      cutoff_central = 5,
      bhd_central = 25000,
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
      output_attribute_1 = scen_1_singlebhd_yld_geo,
      exp_central = list(6, 6.5),
      info = "PM2.5_yld_after")

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_singlebhd_yld_geo,
        output_attribute_2 = scen_2_singlebhd_yld_geo,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(599, 315, 878) # Result on 20 June 2024; no comparison study
  )
})

### YLD ########################################################################

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
      dw_central = 0.5, dw_lower = 0.1, dw_upper = 10,
      duration_central = 1, duration_lower = 0.5, duration_upper = 10)

  scen_2_singlebhd_yld <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_singlebhd_yld,
      exp_central = 6)

  testthat::expect_equal(
    object =
      healthiar::compare(
        approach_comparison = "pif",
        output_attribute_1 = scen_1_singlebhd_yld,
        output_attribute_2 = scen_2_singlebhd_yld
        )$health_main$impact_rounded,
    expected =
      c(391,206,573) # Result on 16 May 2024; no comparison study
  )
})

#### ITERATION #################################################################

testthat::test_that("results the same yld |pathway_compare|comp_appr_pif|exp_single|iteration_TRUE|", {

  scen_1_singlebhd_yld_geo <-
    healthiar::attribute_health(
      exp_central = list(8.85, 8.0),
      cutoff_central = 5,
      bhd_central = 25000,
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
      output_attribute_1 = scen_1_singlebhd_yld_geo,
      exp_central = list(6, 6.5),
      info = "PM2.5_yld_after")

  testthat::expect_equal(
    object =
      healthiar::compare(
        approach_comparison = "pif",
        output_attribute_1 = scen_1_singlebhd_yld_geo,
        output_attribute_2 = scen_2_singlebhd_yld_geo
        )$health_main$impact_rounded,
    expected =
      c(599, 315, 878) # Result on 20 June 2024; no comparison study
  )
})

# ABSOLUTE RISK ################################################################

## NOTE: no PIF option in AR pathway

## DELTA #######################################################################

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
      output_attribute_1 = scen_1_singlebhd_ar,
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

### YLD ########################################################################

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
      dw_central = 0.5, dw_lower = 0.1, dw_upper = 10,
      duration_central = 1, duration_lower = 0.5, duration_upper = 10)

  scen_2_singlebhd_yld <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_singlebhd_yld,
      exp_central = 6)

  testthat::expect_equal(
    object =
      comparison_singlebhd_ar_delta <-
      healthiar::compare(
        output_attribute_1 = scen_1_singlebhd_yld,
        output_attribute_2 = scen_2_singlebhd_yld
        )$health_main$impact_rounded,
    expected =
      c(387, 205, 564) # Result on 16 May 2024; no comparison study
  )
})

### ITERATION ##################################################################

testthat::test_that("results correct |pathway_compare|comp_appr_delta|exp_dist|iteration_TRUE|", {

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
        output_attribute_2 = scen_2_singlebhd_ar_geo
        )$health_main$impact_rounded,
    expected =
      c(115869) # Results on 19 June 2024; no comparison study
  )
})

# LIFETABLE ####################################################################

## YLL #########################################################################

### DELTA ######################################################################

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
      first_age_pop = 0,
      last_age_pop = 99,
      deaths_male = data[["pop"]]$number_of_deaths_male,
      deaths_female = data[["pop"]]$number_of_deaths_female,
      population_midyear_male = data_lifetable[["male"]]$population,
      population_midyear_female = data_lifetable[["female"]]$population,
      year_of_analysis = 2019,
      min_age = 20)

  scen_2_yll_lifetable_test <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_yll_lifetable_test,
      exp_central = 6) # Fake data just for testing purposes

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_yll_lifetable_test,
        output_attribute_2 = scen_2_yll_lifetable_test
        )$health_main$impact_rounded,
    expected =
      c(21644, 11340, 31860) # Result on 20 August 2024; no comparison study to
  )
})

#### ITERATION #################################################################

testthat::test_that("results correct yll |pathway_compare|comp_appr_delta|exp_single|iteration_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_yll_lifetable_geo <-
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      exp_central = list(8.85, 8.0), # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      first_age_pop = 0,
      last_age_pop = 99,
      deaths_male = data[["pop"]]$number_of_deaths_male,
      deaths_female = data[["pop"]]$number_of_deaths_female,
      population_midyear_male = data_lifetable[["male"]]$population,
      population_midyear_female = data_lifetable[["female"]]$population,
      year_of_analysis = 2019,
      min_age = 20,
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2))

  scen_2_yll_lifetable_geo <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_yll_lifetable_geo,
      exp_central = list(6, 6.5)) # Fake data just for testing purposes

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_yll_lifetable_geo,
        output_attribute_2 = scen_2_yll_lifetable_geo
        )$health_main$impact_rounded,
    expected =
      c(33041, 17309, 48639) # Result on 20 August 2024; no comparison study to
  )
})

### PIF ########################################################################

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
      first_age_pop = 0,
      last_age_pop = 99,
      deaths_male = data[["pop"]]$number_of_deaths_male,
      deaths_female = data[["pop"]]$number_of_deaths_female,
      population_midyear_male = data_lifetable[["male"]]$population,
      population_midyear_female = data_lifetable[["female"]]$population,
      year_of_analysis = 2019,
      min_age = 20)

  scen_2_yll_lifetable <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_yll_lifetable,
      exp_central = 6) # Fake data just for testing purposes

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_yll_lifetable,
        output_attribute_2 = scen_2_yll_lifetable,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(21698, 11354, 31978) # Result on 20 August 2024; no comparison study to
  )
})

#### ITERATION #################################################################

testthat::test_that("results the same yll |pathway_compare|comp_appr_pif|exp_single|iteration_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_yll_lifetable_geo <-
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      exp_central = list(8.85, 8.0), # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      first_age_pop = 0,
      last_age_pop = 99,
      deaths_male = data[["pop"]]$number_of_deaths_male,
      deaths_female = data[["pop"]]$number_of_deaths_female,
      population_midyear_male = data_lifetable[["male"]]$population,
      population_midyear_female = data_lifetable[["female"]]$population,
      year_of_analysis = 2019,
      min_age = 20,
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2))

  scen_2_yll_lifetable_geo <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_yll_lifetable_geo,
      exp_central = list(6, 6.5)) # Fake data just for testing purposes

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_yll_lifetable_geo,
        output_attribute_2 = scen_2_yll_lifetable_geo,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(33137, 17335, 48851) # Result on 20 August 2024; no comparison study to
  )
})

## PREMATURE DEATHS ############################################################

### DELTA ######################################################################

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
      first_age_pop = 0,
      last_age_pop = 99,
      deaths_male = data[["pop"]]$number_of_deaths_male,
      deaths_female = data[["pop"]]$number_of_deaths_female,
      population_midyear_male = data_lifetable[["male"]]$population,
      population_midyear_female = data_lifetable[["female"]]$population,
      year_of_analysis = 2019,
      min_age = 20)

  scen_2_deaths_lifetable <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_deaths_lifetable,
      exp_central = 6)

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_deaths_lifetable,
        output_attribute_2 = scen_2_deaths_lifetable
        )$health_main$impact_rounded,
    expected =
      c(1915, 1013, 2795) # Result on 20 August 2024; no comparison study to
  )
})

#### ITERATION #################################################################

testthat::test_that("results correct d|pathway_compare|comp_appr_delta|exp_single|iteration_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_deaths_lifetable_geo <-
    healthiar::attribute_lifetable(
      health_outcome = "deaths",
      exp_central = list(8.85, 8.0),# Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      first_age_pop = 0,
      last_age_pop = 99,
      deaths_male = data[["pop"]]$number_of_deaths_male,
      deaths_female = data[["pop"]]$number_of_deaths_female,
      population_midyear_male = data_lifetable[["male"]]$population,
      population_midyear_female = data_lifetable[["female"]]$population,
      year_of_analysis = 2019,
      min_age = 20,
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2))

  scen_2_deaths_lifetable_geo <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_deaths_lifetable_geo,
      exp_central = list(6, 6.5)) # Fake data just for testing purposes

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_deaths_lifetable_geo,
        output_attribute_2 = scen_2_deaths_lifetable_geo
        )$health_main$impact_rounded,
    expected =
      c(2925, 1546, 4269) # Result on 20 August 2024; no comparison study to
  )
})

### PIF ########################################################################

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
      first_age_pop = 0,
      last_age_pop = 99,
      deaths_male = data[["pop"]]$number_of_deaths_male,
      deaths_female = data[["pop"]]$number_of_deaths_female,
      population_midyear_male = data_lifetable[["male"]]$population,
      population_midyear_female = data_lifetable[["female"]]$population,
      year_of_analysis = 2019,
      min_age = 20)

  scen_2_deaths_lifetable <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_deaths_lifetable,
      exp_central = 6)

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_deaths_lifetable,
        output_attribute_2 = scen_2_deaths_lifetable,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(1935, 1018, 2837) # Result on 20 August 2024; no comparison study to
  )
})

#### ITERATION #################################################################

testthat::test_that("results the same |pathway_compare|comp_appr_pif|exp_single|iteration_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_deaths_lifetable_geo <-
    healthiar::attribute_lifetable(
      health_outcome = "deaths",
      exp_central = list(8.85, 8.0),# Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      first_age_pop = 0,
      last_age_pop = 99,
      deaths_male = data[["pop"]]$number_of_deaths_male,
      deaths_female = data[["pop"]]$number_of_deaths_female,
      population_midyear_male = data_lifetable[["male"]]$population,
      population_midyear_female = data_lifetable[["female"]]$population,
      year_of_analysis = 2019,
      min_age = 20,
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = rep("ch", 2))

  scen_2_deaths_lifetable_geo <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_deaths_lifetable_geo,
      exp_central = list(6, 6.5)) # Fake data just for testing purposes

  testthat::expect_equal(
    object =
      healthiar::compare(
        output_attribute_1 = scen_1_deaths_lifetable_geo,
        output_attribute_2 = scen_2_deaths_lifetable_geo,
        approach_comparison = "pif"
        )$health_main$impact_rounded,
    expected =
      c(2961, 1556, 4346) # Result on 20 AUgust 2024; no comparison study to
  )
})
