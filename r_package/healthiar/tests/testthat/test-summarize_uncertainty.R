## NOTE 2025-04-30 AL: actually all comparisons for summarize_uncertainty are
## "fake", and should be labelled as such - but since it's the way of
## corroborating our results that we have, I classify them as
## "results correct" comparisons.

# QUANTITATIVE TEST ############################################################

## RR ###########################################################################

### SINGLE EXPOSURE #############################################################

testthat::test_that("results correct |pathway_uncertainty|exp_single|erf_rr_increment|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_copd_with_summary_uncertainty <-
    healthiar::attribute_health(
      exp_central = 8.85,
      exp_lower = data$mean_concentration - 1,
      exp_upper = data$mean_concentration + 1,
      cutoff_central = 5,
      cutoff_lower = data$cut_off_value - 1,
      cutoff_upper = data$cut_off_value + 1,
      bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
      bhd_lower = (data$incidents_per_100_000_per_year/1E5*data$population_at_risk) - 5000,
      bhd_upper = (data$incidents_per_100_000_per_year/1E5*data$population_at_risk) + 5000,
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear"
    )

  testthat::expect_equal(
    object =
      healthiar::summarize_uncertainty(
        output_attribute = bestcost_pm_copd_with_summary_uncertainty,
        n_sim = 100,
        seed = 122
        )$uncertainty_main$impact_rounded,
    expected = # Results on 2025-06-04; no comparison study
      c(1303, 528, 2246)
  )
})

#### ITERATION #################################################################
testthat::test_that("results correct |pathway_uncertainty|exp_single|erf_rr_increment|iteration_FALSE|", {

  summary_uncertainty_small_iteration <-
    healthiar::attribute_health(
      exp_central = c(8, 7.5),
      exp_lower = c(7, 6.2),
      exp_upper = c(9, 8.1),
      cutoff_central = 5,
      cutoff_lower = 4,
      cutoff_upper = 6,
      bhd_central = c(1E5, 1E5),
      bhd_lower = c(5E4, 5E4),
      bhd_upper = c(2E5, 2E5),
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      geo_id_disaggregated = c("a", "b")
    )

  testthat::expect_equal(
    object =
      healthiar::summarize_uncertainty(
        output_attribute = summary_uncertainty_small_iteration,
        n_sim = 100,
        seed = 123
      )$uncertainty_main$impact_rounded,

    expected = # Results on 2025-06-04; no comparison study
      c(2591.0, 745.0, 7400.0, 2599.0, 537.0, 6566.0)
  )
})

testthat::test_that("results correct |pathway_uncertainty|exp_single|erf_rr_increment|iteration_TRUE|", {

  bestcost_pm_copd_geo_short <-
    healthiar::attribute_health(
      exp_central = runif_with_seed(1E1, 8.0, 9.0, 1),
      exp_lower = runif_with_seed(1E1, 8.0, 9.0, 1)-0.1,
      exp_upper = runif_with_seed(1E1, 8.0, 9.0, 1)+0.1,
      cutoff_central = 5,
      bhd_central = runif_with_seed(1E1, 25000, 35000, 1),
      rr_central = 1.369,
      rr_lower = 1.124,
      rr_upper = 1.664,
      rr_increment = 10,
      erf_shape = "log_linear",
      geo_id_disaggregated = 1:1E1,
      geo_id_aggregated = c(rep("CH", 5), rep("DE", 5)),
      info = "PM2.5_copd")

  testthat::expect_equal(
    object =
      healthiar::summarize_uncertainty(
        output_attribute = bestcost_pm_copd_geo_short,
        n_sim = 100
      )$uncertainty_main$impact_rounded,

    expected = # Results on 2025-07-17; no comparison study
      c(14672, 6900, 23566, 15485, 7350, 24879)
  )
})


#### YLD ########################################################################

testthat::test_that("results correct yld |pathway_uncertainty|exp_single|erf_rr_increment|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_yld_singlebhd_with_summary_uncertainty  <-
    healthiar::attribute_health(
      exp_central = 8.85,
      exp_lower = data$mean_concentration - 1,
      exp_upper = data$mean_concentration + 1,
      cutoff_central = 5,
      cutoff_lower = data$cut_off_value - 1,
      cutoff_upper = data$cut_off_value + 1,
      bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
      bhd_lower = (data$incidents_per_100_000_per_year/1E5*data$population_at_risk) - 5000,
      bhd_upper = (data$incidents_per_100_000_per_year/1E5*data$population_at_risk) + 5000,
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      # dw_central = 0.9, dw_lower = 0.88, dw_upper = 0.93,
      dw_central = 0.5, dw_lower = 0.25, dw_upper = 0.75,
      duration_central = 1
    )

  testthat::expect_equal(

    object =
      healthiar::summarize_uncertainty(
        output_attribute = bestcost_pm_yld_singlebhd_with_summary_uncertainty,
        n_sim = 100,
        seed = 122
      )$uncertainty_main$impact_rounded,

    expected = # Results on 2025-06-04; no comparison study
      c(643, 264, 1255)
  )
})

### EXPOSURE DISTRIBUTION #######################################################

testthat::test_that("results correct |pathway_uncertainty|exp_dist|erf_rr_increment|iteration_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  bestcost_noise_ihd_expDist <-
    healthiar::attribute_health(
      exp_central = data$exposure_mean,
      prop_pop_exp = data$prop_exposed,
      cutoff_central = min(data$exposure_mean),
      bhd_central = data$gbd_daly[1],
      rr_central = 1.08,
      rr_lower = 1.08 - 0.02,
      rr_upper = 1.08 + 0.02,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = data.frame(pollutant = "road_noise", outcome = "YLD"))

  testthat::expect_equal(

    object =
      healthiar::summarize_uncertainty(
        output_attribute = bestcost_noise_ihd_expDist,
        n_sim = 100,
        seed = 122)$uncertainty_main$impact_rounded,

    expected = # Results on 2025-06-04; no comparison study
      c(1199, 890, 1430)
  )
})



### YLD #########################################################################

testthat::test_that("results correct yld |pathway_uncertainty|exp_dist|erf_ar_formula|iteration_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  bestcost_noise_ha_ar <-
    healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = data$exposure_mean,
      pop_exp = data$population_exposed_total,
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      dw_central = 0.5, dw_lower = 0.25, dw_upper = 0.75,
      duration_central = 1, duration_lower = 0.1, duration_upper = 10,
      info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance"))

  testthat::expect_equal(
    object =
      summarize_uncertainty(
        output_attribute = bestcost_noise_ha_ar,
        n_sim = 100,
        seed = 122)$uncertainty_main$impact_rounded,

    expected = # Results on 2025-06-04; no comparison study
      c(177388, 15557, 527362)
  )
})

# COMPARE ########

testthat::test_that("results correct |pathway_uncertainty_compare|exp_dist|erf_ar_formula|iteration_TRUE|", {

  rr_scenario_1 <-
    healthiar::attribute_health(
      exp_central = 8,
      exp_lower = 7,
      exp_upper = 9,
      cutoff_central = 5,
      cutoff_lower = 4,
      cutoff_upper = 6,
      bhd_central = 1E5,
      bhd_lower = 5E4,
      bhd_upper = 2E5,
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear")

  rr_scenario_2 <-
    healthiar::attribute_mod(
      output_attribute =  rr_scenario_1,
      exp_central = 7.5,
      exp_lower = 6.2,
      exp_upper = 8.1)

  rr_comparison <-
    healthiar::compare(
      output_attribute_scen_1 = rr_scenario_1,
      output_attribute_scen_2 = rr_scenario_2,
      approach_comparison = "delta")

  testthat::expect_equal(
    object =
      healthiar::summarize_uncertainty(
        output_attribute = rr_comparison,
        n_sim = 100,
        seed = 122)$uncertainty_main$impact_rounded,

    expected = # Results on 2025-06-04; no comparison study
      c(528, 148, 1173)
  )
})

## ITERATION #######

testthat::test_that("summary uncertainty comparison iteration", {

  scen_1_singlebhd_rr_geo <-
    healthiar::attribute_health(
      approach_risk = "relative_risk",
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

  comparison_iteration <-
    healthiar::compare(
      output_attribute_scen_1 = scen_1_singlebhd_rr_geo,
      output_attribute_scen_2 = scen_2_singlebhd_rr_geo)

  testthat::expect_equal(
    object =
      healthiar::summarize_uncertainty(
        output_attribute = comparison_iteration,
        n_sim = 100)$uncertainty_main$impact_rounded,
    expected = # Results on 2025-06-05; no comparison study
      c(1052.0, 634.0, 1510.0)
  )
})

# ERROR OR WARNING ########
## ERROR #########

testthat::test_that("error_if_erf_eq |pathway_uncertainty|exp_dist|erf_ar_formula|iteration_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  bestcost_noise_ha_ar_with_erf_eq <-
    healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = data$exposure_mean,
      pop_exp = data$population_exposed_total,
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      erf_eq_lower = "78.9270-3.1162*c+0.034*c^2",
      erf_eq_upper = "78.9270-3.1162*c+0.04*c^2",
      info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance"),
    )

  testthat::expect_error(
    object =
      summarize_uncertainty(
        bestcost_noise_ha_ar_with_erf_eq,
        n_sim = 1000),
    regexp = "Sorry, the summary of uncertainty for erf_eq_... is not currently supported."
  )
})

testthat::test_that("error_if_erf_eq  |pathway_uncertainty|exp_dist|erf_ar_formula|iteration_TRUE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  bestcost_noise_ha_ar_iteration_with_erf_eq <-
    healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = c(data$exposure_mean,
                      data$exposure_mean + 5,
                      data$exposure_mean + 10),
      pop_exp = c(data$population_exposed_total,
                  data$population_exposed_total + 0.1,
                  data$population_exposed_total + 0.2),
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      erf_eq_lower = "78.9270-3.1162*c+0.034*c^2",
      erf_eq_upper = "78.9270-3.1162*c+0.04*c^2",
      geo_id_disaggregated = rep(1:3, each = 5),
      geo_id_aggregated = rep("CH", 3*5),
      info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance"),
    )

  testthat::expect_error(
    object =
      summarize_uncertainty(
        bestcost_noise_ha_ar_iteration_with_erf_eq,
        n_sim = 100),
    regexp = "Sorry, the summary of uncertainty for erf_eq_... is not currently supported.")
})



testthat::test_that("error_if_uncertainty_in_exposure_distribution |pathway_uncertainty|exp_dist|erf_ar_formula|iteration_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  bestcost_noise_ha_ar_with_summary_uncertainty <-
    healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = data$exposure_mean,
      exp_lower = data$exposure_mean - 1,
      exp_upper = data$exposure_mean + 1,
      pop_exp = data$population_exposed_total,
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")
    )

  testthat::expect_error(
    object =
      summarize_uncertainty(
        output_attribute = bestcost_noise_ha_ar_with_summary_uncertainty,
        n_sim = 100,
        seed = 122)$uncertainty_main$impact_rounded,
    regexp = "Sorry, the summary of uncertainty for exp_... in exposure distributions is not currently supported."
    )
})

testthat::test_that("error_if_no_uncertainty |pathway_uncertainty|exp_single|erf_rr_increment|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_copd_with_summary_uncertainty <-
    healthiar::attribute_health(
      exp_central = 8.85,
      cutoff_central = 5,
      bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
      rr_central = 1.060,
      rr_increment = 10,
      erf_shape = "log_linear"
    )

  testthat::expect_error(
    object =
      healthiar::summarize_uncertainty(
        output_attribute = bestcost_pm_copd_with_summary_uncertainty,
        n_sim = 100
      ),
    regexp = "Please enter an assessment with uncertainty (..._lower and ..._upper) in any argument.",
    fixed = TRUE
  )
})
## WARNING #########

