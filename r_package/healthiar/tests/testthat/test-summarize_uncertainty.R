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
        res = bestcost_pm_copd_with_summary_uncertainty,
        n_sim = 100,
        seed = 123
        )$uncertainty_main |> base::as.numeric() |> base::round(),
    expected = # Results on 2025-05-08; no comparison study
      c(1324, 854, 1815)
  )
})

#### ITERATION ##################################################################

testthat::test_that("results correct |pathway_uncertainty|exp_single|erf_rr_increment|iteration_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_copd_geo_short <-
    healthiar::attribute_health(
      exp_central = as.list(runif_with_seed(1E1, 8.0, 9.0, 1)),
      exp_lower = as.list(runif_with_seed(1E1, 8.0, 9.0, 1)-0.1),
      exp_upper = as.list(runif_with_seed(1E1, 8.0, 9.0, 1)+0.1),
      cutoff_central = 5,
      bhd_central = as.list(runif_with_seed(1E1, 25000, 35000, 1)),
      rr_central = 1.369,
      rr_lower = 1.124,
      rr_upper = 1.664,
      rr_increment = 10,
      erf_shape = "log_linear",
      geo_id_disaggregated = 1:1E1,
      geo_id_aggregated = rep("CH", 1E1),
      info = "PM2.5_copd")

  testthat::expect_equal(
    object =
      healthiar::summarize_uncertainty(
        res = bestcost_pm_copd_geo_short,
        n_sim = 100
      )$uncertainty_main |> base::as.numeric() |> base::round(),
    expected = # Results on 2025-05-08; no comparison study
      c(31566, 25333, 37347)
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
        res = bestcost_pm_yld_singlebhd_with_summary_uncertainty,
        n_sim = 100,
        seed = 123
      )$uncertainty_main |> base::as.numeric() |> base::round(),

    expected = # Results on 2025-05-08; no comparison study
      c(652, 266, 1031)
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
        bestcost_noise_ihd_expDist,
        n_sim = 100)$uncertainty_main |> base::as.numeric() |> base::round(),

    expected = # Results on 2025-05-07; no comparison study
      c(1152, 997, 1290)
  )
})

#### ITERATION ##################################################################

testthat::test_that("results correct |pathway_uncertainty|exp_dist|erf_rr_increment|iteration_TRUE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  bestcost_noise_ihd_expDist_iteration <-
    healthiar::attribute_health(
      exp_central = list(data$exposure_mean,
                         data$exposure_mean + 5,
                         data$exposure_mean + 10),
      exp_lower = list(data$exposure_mean - 2,
                       data$exposure_mean + 5 - 2,
                       data$exposure_mean + 10 - 2),
      exp_upper = list(data$exposure_mean + 2,
                       data$exposure_mean + 5 + 2,
                       data$exposure_mean + 10 + 2),
      prop_pop_exp = list(data$prop_exposed,
                          data$prop_exposed,
                          data$prop_exposed),
      cutoff_central = min(data$exposure_mean),
      bhd_central = list(data$gbd_daly[1],
                         data$gbd_daly[1] + 5000,
                         data$gbd_daly[1] + 10000),
      rr_central = 1.08,
      rr_increment = 10,
      erf_shape = "log_linear",
      geo_id_disaggregated = 1:3,
      geo_id_aggregated = rep("CH", 3),
      info = data.frame(pollutant = "road_noise", outcome = "YLD"))

  testthat::expect_equal(
    object =
      healthiar::summarize_uncertainty(
        bestcost_noise_ihd_expDist_iteration,
        n_sim = 100)$uncertainty_main |> base::as.numeric() |> base::round(),

    expected = # Results on 2025-05-08; no comparison study
      c(14177, 12688, 15875)
  )
})



## AR ###########################################################################

testthat::test_that("results correct |pathway_uncertainty|exp_dist|erf_ar_formula|iteration_FALSE|", {

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

  testthat::expect_equal(
    object =
      summarize_uncertainty(
        bestcost_noise_ha_ar_with_summary_uncertainty,
        n_sim = 100)$uncertainty_main |> base::as.numeric() |> base::round(),

    expected = # Results on 2025-02-11; no comparison study
      c(174770, 170157, 179547)
  )
})



### ITERATION ###################################################################

testthat::test_that("results correct |pathway_uncertainty|exp_dist|erf_ar_formula|iteration_TRUE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  bestcost_noise_ha_ar_iteration <-
    healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = list(data$exposure_mean,
                         data$exposure_mean + 5,
                         data$exposure_mean + 10),
      exp_lower = list(data$exposure_mean -2,
                       data$exposure_mean + 5 - 2,
                       data$exposure_mean + 10 - 2),
      exp_upper = list(data$exposure_mean + 2,
                       data$exposure_mean + 5 + 2,
                       data$exposure_mean + 10 + 2),
      pop_exp = list(data$population_exposed_total,
                     data$population_exposed_total + 0.1,
                     data$population_exposed_total + 0.2),
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      geo_id_disaggregated = 1:3,
      geo_id_aggregated = rep("CH", 3),
      info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance"),
    )

  testthat::expect_equal(
    object =
      summarize_uncertainty(
        bestcost_noise_ha_ar_iteration,
        n_sim = 100)$uncertainty_main |> base::as.numeric() |> base::round(),

    expected = # Results on 2025-04-04; no comparison study
    c(728049, 708545, 751784)
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
        bestcost_noise_ha_ar,
        n_sim = 100)$uncertainty_main |> base::as.numeric() |> base::round(),

    expected = # Results on 2025-02-11; no comparison study
      c(25126, 276, 142539)
  )
})

# ERROR OR WARNING ########
## ERROR #########

testthat::test_that("results correct with erf_eq uncertainty |pathway_uncertainty|exp_dist|erf_ar_formula|iteration_FALSE|", {

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
        n_sim = 1000)
  )
})

testthat::test_that("results correct with erf_eq uncertainty |pathway_uncertainty|exp_dist|erf_ar_formula|iteration_TRUE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  bestcost_noise_ha_ar_iteration_with_erf_eq <-
    healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = list(data$exposure_mean,
                         data$exposure_mean + 5,
                         data$exposure_mean + 10),
      pop_exp = list(data$population_exposed_total,
                     data$population_exposed_total + 0.1,
                     data$population_exposed_total + 0.2),
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      erf_eq_lower = "78.9270-3.1162*c+0.034*c^2",
      erf_eq_upper = "78.9270-3.1162*c+0.04*c^2",
      geo_id_disaggregated = 1:3,
      geo_id_aggregated = rep("CH", 3),
      info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance"),
    )

  testthat::expect_error(
    object =
      summarize_uncertainty(
        bestcost_noise_ha_ar_iteration_with_erf_eq,
        n_sim = 100))
})



## WARNING #########

