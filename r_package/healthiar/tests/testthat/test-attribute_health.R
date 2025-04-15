# RR ###########################################################################

## SINGLE EXPOSURE #############################################################
# testthat::test_that("result correct rr with single exposure value and only rr_central", {
# testthat::test_that("result correct rr with single exp (pw_exp_single pw_cutoff_TRUE pw_varuncer_FALSE pw_erf_log_lin pw_iteration_FALSE pw_multiexp_FALSE)", {
# testthat::test_that("result correct ID|erf_log_lin|exp_single|cutoff_TRUE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE", {
testthat::test_that("result correct pathway_rr|erf_log_lin|exp_single|cutoff_TRUE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE|", {

    data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

    testthat::expect_equal(
      object =
        healthiar::attribute_health(
          exp_central = data$mean_concentration,
          cutoff_central = data$cut_off_value,
          bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
          rr_central = data$relative_risk,
          rr_increment = 10,
          erf_shape = "log_linear",
          info = paste0(data$pollutant,"_", data$evaluation_name)
        )$health_main$impact_rounded,
      expected = # airqplus_pm_copd
        data |>
        dplyr::select(estimated_number_of_attributable_cases_central)|>
        base::as.numeric()
    )
})

# testthat::test_that("result correct rr with single exposure and variable uncertainty", {
# testthat::test_that("result correct rr with single exposure and variable uncertainty (pw_erf_log_lin pw_exp_single pw_cutoff_TRUE pw_varuncer_TRUE pw_iteration_FALSE pw_multiexp_FALSE )", {
testthat::test_that("result correct pathway_rr|erf_log_lin|exp_single|cutoff_TRUE|varuncer_TRUE|iteration_FALSE|multiexp_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        exp_lower = 8,
        exp_upper = 9,
        cutoff_central = data$cut_off_value,
        cutoff_lower = data$cut_off_value - 1,
        cutoff_upper = data$cut_off_value + 1,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        bhd_lower = 25000,
        bhd_upper = 35000,
        rr_central = data$relative_risk,
        rr_lower = data$relative_risk_lower,
        rr_upper = data$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = paste0(data$pollutant,"_", data$evaluation_name)
      )$health_main$impact_rounded,
    expected = # airqplus_pm_copd
      data |>
      dplyr::select(estimated_number_of_attributable_cases_central,
                    estimated_number_of_attributable_cases_lower,
                    estimated_number_of_attributable_cases_upper)|>
      base::as.numeric()
  )

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        exp_lower = data$mean_concentration - 1,
        exp_upper = data$mean_concentration + 1,
        cutoff_central = data$cut_off_value,
        cutoff_lower = data$cut_off_value - 1,
        cutoff_upper = data$cut_off_value + 1,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        bhd_lower = data$incidents_per_100_000_per_year/1E5*data$population_at_risk - 1,
        bhd_upper = data$incidents_per_100_000_per_year/1E5*data$population_at_risk + 1,
        rr_central = data$relative_risk,
        rr_lower = data$relative_risk_lower,
        rr_upper = data$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = paste0(data$pollutant,"_", data$evaluation_name)
      )$health_detailed$impact_raw$impact_rounded,
    expected = # Results on 2025-01-17; no comparison study
      c(3502, 4344, 2633, 3502, 4344, 2633, 3502, 4345, 2633, 1353, 1695, 1007, 1353, 1695, 1007, 1353, 1695, 1007, 5474, 6729, 4154, 5474, 6728, 4153, 5474, 6729, 4154, 2633, 3502, 1736, 2633, 3502, 1736, 2633, 3502, 1736, 1007, 1353, 658, 1007, 1353, 658, 1007, 1353, 658, 4154, 5474, 2764, 4153, 5474, 2764, 4154, 5474, 2764, 4344, 5161, 3502, 4344, 5161, 3502, 4345, 5161, 3502, 1695, 2032, 1353, 1695, 2032, 1353, 1695, 2032, 1353, 6729, 7921, 5474, 6728, 7921, 5474, 6729, 7921, 5474)
  )
})

testthat::test_that("detailed result the same fake_rr|erf_log_lin|exp_single|cutoff_TRUE|varuncer_TRUE|iteration_FALSE|multiexp_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        exp_lower = data$mean_concentration - 1,
        exp_upper = data$mean_concentration + 1,
        cutoff_central = data$cut_off_value,
        cutoff_lower = data$cut_off_value - 1,
        cutoff_upper = data$cut_off_value + 1,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        bhd_lower = data$incidents_per_100_000_per_year/1E5*data$population_at_risk - 1,
        bhd_upper = data$incidents_per_100_000_per_year/1E5*data$population_at_risk + 1,
        rr_central = data$relative_risk,
        rr_lower = data$relative_risk_lower,
        rr_upper = data$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = paste0(data$pollutant,"_", data$evaluation_name)
      )$health_detailed$impact_raw$impact_rounded,
    expected = # Results on 2025-01-17; no comparison study
      c(3502, 4344, 2633, 3502, 4344, 2633, 3502, 4345, 2633, 1353, 1695, 1007, 1353, 1695, 1007, 1353, 1695, 1007, 5474, 6729, 4154, 5474, 6728, 4153, 5474, 6729, 4154, 2633, 3502, 1736, 2633, 3502, 1736, 2633, 3502, 1736, 1007, 1353, 658, 1007, 1353, 658, 1007, 1353, 658, 4154, 5474, 2764, 4153, 5474, 2764, 4154, 5474, 2764, 4344, 5161, 3502, 4344, 5161, 3502, 4345, 5161, 3502, 1695, 2032, 1353, 1695, 2032, 1353, 1695, 2032, 1353, 6729, 7921, 5474, 6728, 7921, 5474, 6729, 7921, 5474)
  )
})

testthat::test_that("no error rr_no_error|erf_log_lin|exp_single|cutoff_TRUE|varuncer_TRUE|iteration_FALSE|multiexp_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_no_error(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        exp_lower = 8,
        exp_upper = 9,
        cutoff_central = data$cut_off_value,
        cutoff_lower = data$cut_off_value - 1,
        cutoff_upper = data$cut_off_value + 1,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        bhd_lower = 25000,
        bhd_upper = 35000,
        rr_central = data$relative_risk,
        rr_lower = data$relative_risk_lower,
        rr_upper = data$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = paste0(data$pollutant,"_", data$evaluation_name)
        )
    )
})

testthat::test_that("number of rows in detailed results correct rr|erf_log_lin|exp_single|cutoff_TRUE|varuncer_TRUE|iteration_FALSE|multiexp_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        exp_lower = 8,
        exp_upper = 9,
        cutoff_central = data$cut_off_value,
        cutoff_lower = data$cut_off_value - 1,
        cutoff_upper = data$cut_off_value + 1,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        bhd_lower = 25000,
        bhd_upper = 35000,
        rr_central = data$relative_risk,
        rr_lower = data$relative_risk_lower,
        rr_upper = data$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = paste0(data$pollutant,"_", data$evaluation_name)
        )$health_detailed$impact_raw |> base::nrow(),
    expected =
      3^4 # CI's in 4 input variables
      )

})



## single exposure & user-defined points for relative risk using splinefun
testthat::test_that("results the same rr single exposure value and user-defined ERF (using stats::approxfun) and cutoff equals 5", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        cutoff_central = data$cut_off_value,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        erf_eq_central = # GBD2019 ERF for PM and lower respiratory infections
          stats::splinefun(
            x = c(600,500,400,300,200,150,130,110,90,70,50,30,25,20,15,10,5,0),
            y = c(2.189,2.143,2.098,2.052,1.909,1.751,1.68,1.607,1.533,1.453,1.357,1.238,1.204,1.168,1.129,1.089,1.046,	1),
            method = "natural"),
        info = paste0(data$pollutant,"_", data$evaluation_name)
        )$health_main$impact_rounded,
    expected =
      c(1057) # Results on 10 October 2024 (with cutoff = 5 = data$cut_off_value); no comparison study
  )
})

testthat::test_that("results the same rr single exposure value and user-defined ERF (using stats::splinefun) and cutoff equals 0", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        cutoff_central = 0,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        erf_eq_central = # GBD2019 ERF for PM and lower respiratory infections
          stats::splinefun(
            x = c(600,500,400,300,200,150,130,110,90,70,50,30,25,20,15,10,5,0),
            y = c(2.189,2.143,2.098,2.052,1.909,1.751,1.68,1.607,1.533,1.453,1.357,1.238,1.204,1.168,1.129,1.089,1.046,	1),
            method = "natural"),
        info = paste0(data$pollutant,"_", data$evaluation_name))$health_main$impact_rounded,
    expected =
      c(2263) # Results on 10 October 2024 (with cutoff = 0); no comparison study
  )
})

## single exposure & user-defined points for relative risk using approxfun
testthat::test_that("results the same rr single exposure value and user-defined ERF (using stats::approxfun) and cutoff equals 0", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        cutoff_central = data$cut_off_value,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        erf_eq_central = # GBD2019 ERF for PM and lower respiratory infections
          stats::approxfun(
            x = c(600,500,400,300,200,150,130,110,90,70,50,30,25,20,15,10,5,0),
            y = c(2.189,2.143,2.098,2.052,1.909,1.751,1.68,1.607,1.533,1.453,1.357,1.238,1.204,1.168,1.129,1.089,1.046,	1),
            method = "linear"),
        info = paste0(data$pollutant,"_", data$evaluation_name)
        )$health_main$impact_rounded,
    expected =
      c(1052) # Results on 10 October 2024 (with cutoff = 5); no comparison study
  )
})

### ITERATION ##################################################################

testthat::test_that("results the same rr iteration with exposure distribution and uncertainties in rr and exp", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = as.list(runif_with_seed(1E4, 8.0, 9.0, 1)),
        exp_lower = as.list(runif_with_seed(1E4, 8.0, 9.0, 1)-0.1),
        exp_upper = as.list(runif_with_seed(1E4, 8.0, 9.0, 1)+0.1),
        cutoff_central = 5,
        bhd_central = as.list(runif_with_seed(1E4, 25000, 35000, 1)),
        bhd_lower = as.list(runif_with_seed(1E4, 25000, 35000, 1) - 1000),
        bhd_upper = as.list(runif_with_seed(1E4, 25000, 35000, 1) + 1000),
        rr_central = 1.369,
        rr_lower = 1.124,
        rr_upper = 1.664,
        rr_increment = 10,
        erf_shape = "log_linear",
        population = as.list(rep(1E6, 1E4)),
        geo_id_disaggregated = 1:1E4,
        geo_id_aggregated = rep("CH", 1E4),
        info = "PM2.5_copd")$health_main$impact_rounded,
    expected =
      c(31460722, 12120764, 49312859) # Results on 5 November 2024; no comparison study
  )
})

### YLD ########################################################################

testthat::test_that("results the same rr single exposure value and prevalence-based YLD (duration_central=1)", {

  testthat::expect_equal(
    object =
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
        duration_central = 1, duration_lower = 0.5, duration_upper = 10
        )$health_main$impact_rounded,
    expected =
      c(525, 277, 768) # Result on 16 May 2024; no comparison study
  )
})

testthat::test_that("results the same rr single exposure value and incidence-based YLD (duration_central > 1)", {

  testthat::expect_equal(
    object =
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
        duration_central = 5, duration_lower = 2, duration_upper = 10
        )$health_main$impact_rounded,
    expected =
      c(2627, 1386, 3839) # # Result on 2025-01-28; no comparison study
  )
})

## EXPOSURE DISTRIBUTION #######################################################

testthat::test_that("results correct pathway_rr|erf_log_linear|exp_dist|cutoff_TRUE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  ## With prop_pop_exp
  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$exposure_mean,
        prop_pop_exp = data$prop_exposed,
        cutoff_central = min(data$exposure_mean),
        bhd_central = data$gbd_daly[1],
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = data.frame(pollutant = "road_noise", outcome = "YLD")
        )$health_main$impact_rounded,
    expected =
      data_raw |>
      dplyr::filter(exposure_category %in% "Total exposed")|>
      dplyr::select(daly)|>
      dplyr::pull() |>
      base::round()
    )

  ## With pop_exp
  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$exposure_mean,
        pop_exp = data$population_exposed_total,
        cutoff_central = min(data$exposure_mean),
        bhd_central = data$gbd_daly[1],
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = data.frame(pollutant = "road_noise", outcome = "YLD")
        )$health_main$impact_rounded,
    expected =
      data_raw |>
      dplyr::filter(exposure_category %in% "Total exposed")|>
      dplyr::select(daly)|>
      dplyr::pull() |>
      round()
  )
})

testthat::test_that("results the same rr exposure distribution without cutoff", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$exposure_mean,
        prop_pop_exp = data$prop_exposed,
        bhd_central = data$gbd_daly[1],
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = data.frame(pollutant = "road_noise", outcome = "YLD")
        )$health_main$impact_rounded,
    expected =
      29358 # Results on 2025-01-20; no comparison study
  )
})

### ITERATION ##################################################################
testthat::test_that("results the same rr exposure distribution without cutoff", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = list(runif_with_seed(5,8,10,1),
                           runif_with_seed(5,8,10,2),
                           runif_with_seed(5,8,10,3)),
        cutoff_central = 5,
        pop_exp = list(runif_with_seed(5,1E2,1E3,1),
                       runif_with_seed(5,1E2,1E3,2),
                       runif_with_seed(5,1E2,1E3,3)),
        bhd_central = list(runif_with_seed(3,1E4,1E5,1)),
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        geo_id_disaggregated = 1:3,
        geo_id_aggregated = rep("ch", 3)
        )$health_detailed$impact_raw$impact_rounded,
    expected =
      round(c(1066.970, 1421.845, 1908.409)) # Results on 2025-04-14; no comparison study
  )
})

### USER-DEFINED ERF POINT-PAIRS ###############################################

testthat::test_that("results the same user-defined erf (mrbrt) with splinefun, exposure distribution, uncertainties erf & cutoff", {

  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))
  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$exposure_mean,
        prop_pop_exp = data$prop_exposed,
        cutoff_central = min(data$exposure_mean),
        bhd_central = data$gbd_daly[1],
        rr_central = 1.08,
        rr_increment = 10,
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural"),
        erf_eq_lower = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean,
          method = "natural"),
        erf_eq_upper = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean + 0.01,
          method = "natural"),
        info = data.frame(pollutant = "road_noise", outcome = "YLD")
        )$health_main$impact_rounded,
    expected =
      c(1637, 1637, 2450) # Results on 2025-01-20 ; no comparison study
  )
})

testthat::test_that("results the same user-defined erf (mrbrt) with splinefun, exposure distribution, uncertainties erf & no cutoff", {

  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))
  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$exposure_mean,
        prop_pop_exp = data$prop_exposed,
        bhd_central = data$gbd_daly[1],
        rr_central = 1.08,
        rr_increment = 10,
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural"),
        erf_eq_lower = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean,
          method = "natural"),
        erf_eq_upper = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean + 0.01,
          method = "natural"),
        info = data.frame(pollutant = "road_noise", outcome = "YLD")
        )$health_main$impact_rounded,
    expected =
      c(32502, 32502, 32828) # Results on 2025-01-20 ; no comparison study
  )
})

# testthat::test_that("results correct user-defined erf (mrbrt) with splinefun", {
# testthat::test_that("results correct user-defined erf (mrbrt) with splinefun ( pw_exp_single pw_erf_point_pairs pw_cutoff_TRUE pw_varuncer_FALSE pw_iteration_FALSE pw_multiexp_FALSE )", {
testthat::test_that("results the same fake_rr|erf_point_pairs|exp_dist|cutoff_FALSE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE|", {

  data_pop <- base::readRDS(testthat::test_path("data", "pop_data_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        exp_central = data_pop$Concentration,
        prop_pop_exp = data_pop$Viken,
        cutoff_central = 5,
        bhd_central = 4500,
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural")
      )$health_main$impact_rounded,
    expected =
      c(32) # Results on 19 Dec 2024; no comparison study
  )
})

testthat::test_that("results the same fake_rr user-defined erf (mrbrt) with splinefun with no cutoff", {

  data_pop <- base::readRDS(testthat::test_path("data", "pop_data_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        exp_central = data_pop$Concentration,
        prop_pop_exp = data_pop$Viken,
        bhd_central = 4500,
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural")
      )$health_main$impact_rounded,
    expected =
      c(249) # Results on 2025-01-17; no comparison study
  )
})

testthat::test_that("results the same fake_rr|erf_point_pairs|exp_dist|cutoff_TRUE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE|", {

  data_pop <- base::readRDS(testthat::test_path("data", "pop_data_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        exp_central = data_pop$Concentration,
        prop_pop_exp = data_pop$Viken,
        cutoff_central = 0,
        bhd_central = 4500,
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural")
      )$health_main$impact_rounded,
    expected =
      c(249) # Results on 2025-01-17 ; no comparison study
  )
})

# TODO BUG 2025-04-02: WHEN CALLING THE FUNCTION WITH CUTOFF AND BHD CI'S THEN HEALTH MAIN HAS MORE THAN 3 ROWS ####
testthat::test_that("results the same fake_rr|erf_point_pairs|exp_dist|cutoff_TRUE|varuncer_TRUE|iteration_FALSE|multiexp_FALSE|", {

  data_pop <- base::readRDS(testthat::test_path("data", "pop_data_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        exp_central = data_pop$Concentration,
        prop_pop_exp = data_pop$Viken,
        cutoff_central = 5,
        # cutoff_lower = 5 - 1,
        # cutoff_upper = 5 + 1,
        bhd_central = 4500,
        # bhd_lower = 4500 - 1000,
        # bhd_upper = 4500 + 1000,
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural"),
        erf_eq_lower = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean,
          method = "natural"),
        erf_eq_upper = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean + 0.01,
          method = "natural")
      )$health_main$impact_rounded,
    expected =
      c(32,32,76) # Results on 19 Dec 2024 ; no comparison study
  )
})

# TODO BUG 2025-04-02: WHEN CALLING THE FUNCTION WITH CUTOFF AND BHD CI'S THEN HEALTH MAIN HAS MORE THAN 3 ROWS ####
testthat::test_that("results the same fake_rr|erf_point_pairs|exp_dist|cutoff_FALSE|varuncer_TRUE|iteration_FALSE|multiexp_FALSE|", {

  data_pop <- base::readRDS(testthat::test_path("data", "pop_data_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        exp_central = data_pop$Concentration,
        # cutoff_lower = 5 - 1,
        # cutoff_upper = 5 + 1,
        prop_pop_exp = data_pop$Viken,
        bhd_central = 4500,
        # bhd_lower = 4500 - 1000,
        # bhd_upper = 4500 + 1000,
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural"),
        erf_eq_lower = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean,
          method = "natural"),
        erf_eq_upper = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean + 0.01,
          method = "natural")
      )$health_main$impact_rounded,
    expected =
      c(249,249,289) # Results on 15 Jan 2024 ; no comparison study
  )
})


# AR ###########################################################################

testthat::test_that("results correct pathway_ar|erf_formula|exp_dist|cutoff_FALSE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE|", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))
  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = data$exposure_mean,
        pop_exp = data$population_exposed_total,
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")
        )$health_main$impact_rounded,
    expected =
      data_raw |>
      dplyr::filter(exposure_category %in% "Total exposed")|>
      dplyr::select(number)|>
      dplyr::pull() |>
      round()
  )
})

## ITERATION ###################################################################

testthat::test_that("no error ar iteration", {

  testthat::expect_no_error(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = list(runif_with_seed(5,8,10,1),
                           runif_with_seed(5,8,10,2),
                           runif_with_seed(5,8,10,3)),
        pop_exp = list(
          runif_with_seed(1,5E3,1E4,1) * runif_with_seed(5,0,1,1), # total pop * proportion pop exposed
          runif_with_seed(1,5E3,1E4,2) * runif_with_seed(5,0,1,2),
          runif_with_seed(1,5E3,1E4,3) * runif_with_seed(5,0,1,3)
        ),
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        geo_id_disaggregated = 1:3,
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance"))
)
})

testthat::test_that("detailed results the same ar iteration no variable uncertainties", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = list(runif_with_seed(5,8,10,1),
                           runif_with_seed(5,8,10,2),
                           runif_with_seed(5,8,10,3)),
        pop_exp = list(
          runif_with_seed(1,5E3,1E4,1) * runif_with_seed(5,0,1,1), # total pop * proportion pop exposed
          runif_with_seed(1,5E3,1E4,2) * runif_with_seed(5,0,1,2),
          runif_with_seed(1,5E3,1E4,3) * runif_with_seed(5,0,1,3)
          ),
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        geo_id_disaggregated = 1:3,
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")
        )$health_detailed$impact_raw$impact |> round(),
    expected =
      c(921, 1278, 1932, 2967, 704, 605, 2191, 1810, 551, 2877, 543, 2458, 1219, 1043, 1869) # Results on 2025-02-05; no comparison study
  )
})

testthat::test_that("detailed results the same ar iteration with variable uncertainties", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = list(runif_with_seed(5,8,10,1),
                           runif_with_seed(5,8,10,2),
                           runif_with_seed(5,8,10,3)),
        exp_lower = list(runif_with_seed(5,8,10,1) - 5,
                         runif_with_seed(5,8,10,2) - 5,
                         runif_with_seed(5,8,10,3) - 5),
        exp_upper = list(runif_with_seed(5,8,10,1) + 5,
                         runif_with_seed(5,8,10,2) + 5,
                         runif_with_seed(5,8,10,3) + 5),
        pop_exp = list(
          runif_with_seed(1,5E3,1E4,1) * runif_with_seed(5,0,1,1), # total pop * proportion pop exposed
          runif_with_seed(1,5E3,1E4,2) * runif_with_seed(5,0,1,2),
          runif_with_seed(1,5E3,1E4,3) * runif_with_seed(5,0,1,3)
        ),
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        geo_id_disaggregated = 1:3,
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")
        )$health_detailed$impact_raw$impact |> round(),
    expected = # Results on 2025-01-20; no comparison study
      c(921, 1148, 723, 1278, 1595, 1002, 1932, 2414, 1511, 2967, 3719, 2314, 704, 877, 553, 605, 754, 475, 2191, 2741, 1712, 1810, 2262, 1416, 551, 686, 433, 2877, 3607, 2243, 543, 676, 426, 2458, 3078, 1919, 1219, 1521, 956, 1043, 1301, 818, 1869, 2336, 1462)
  )
})

## YLD #########################################################################

testthat::test_that("results the same rr yld with variable uncertainties", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = 8.85,
        exp_lower = 8.85,
        cutoff_central = 5,
        bhd_central = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        dw_central = 0.5, dw_lower = 0.1, dw_upper = 10,
        duration_central = 1, duration_lower = 0.5, duration_upper = 10
        )$health_main$impact_rounded,
    expected =
      c(525, 277, 768) # Result on 16 May 2024; no comparison study
  )
})

testthat::test_that("detailed results the same rr yld with variable uncertainties", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = 8.85,
        exp_lower = 8.85 - 1,
        exp_upper = 8.85 + 1,
        cutoff_central = 5,
        cutoff_lower = 5 - 1,
        cutoff_upper = 5 + 1,
        bhd_central = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        dw_central = 0.5, dw_lower = 0.1, dw_upper = 10,
        duration_central = 1, duration_lower = 0.5, duration_upper = 10
        )$health_detailed$impact_raw$impact |> round(), # 2025-04-02 Round at the end to obtain rounded results
    expected = # Result on 2025-01-20; no comparison study
      c(525, 263, 5254, 105, 53, 1051, 10509, 5254, 105086, 658, 329, 6583, 132, 66, 1317, 13165, 6583, 131651, 391, 196, 3911, 78, 39, 782, 7822, 3911, 78223, 277, 139, 2773, 55, 28, 555, 5546, 2773, 55459, 348, 174, 3483, 70, 35, 697, 6966, 3483, 69662, 206, 103, 2059, 41, 21, 412, 4117, 2059, 41174, 768, 384, 7679, 154, 77, 1536, 15357, 7679, 153572, 959, 480, 9595, 192, 96, 1919, 19189, 9595, 191894, 573, 287, 5731, 115, 57, 1146, 11461, 5731, 114615, 391, 196, 3911, 78, 39, 782, 7822, 3911, 78223, 525, 263, 5254, 105, 53, 1051, 10509, 5254, 105086, 255, 128, 2553, 51, 26, 511, 5106, 2553, 51059, 206, 103, 2059, 41, 21, 412, 4117, 2059, 41174, 277, 139, 2773, 55, 28, 555, 5546, 2773, 55459, 134, 67, 1340, 27, 13, 268, 2680, 1340, 26805, 573, 287, 5731, 115, 57, 1146, 11461, 5731, 114615, 768, 384, 7679, 154, 77, 1536, 15357, 7679, 153572, 375, 188, 3750, 75, 38, 750, 7501, 3750, 75010, 658, 329, 6583, 132, 66, 1317, 13165, 6583, 131651, 790, 395, 7896, 158, 79, 1579, 15792, 7896, 157921, 525, 263, 5254, 105, 53, 1051, 10509, 5254, 105086, 348, 174, 3483, 70, 35, 697, 6966, 3483, 69662, 419, 209, 4189, 84, 42, 838, 8378, 4189, 83782, 277, 139, 2773, 55, 28, 555, 5546, 2773, 55459, 959, 480, 9595, 192, 96, 1919, 19189, 9595, 191894, 1148, 574, 11479, 230, 115, 2296, 22959, 11479, 229589, 768, 384, 7679, 154, 77, 1536, 15357, 7679, 153572))
})

testthat::test_that("results correct ar yld with uncertainties in dw and duration", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))
  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  niph_noise_ha_input <-
    niph_noise_ha_excel |>
    dplyr::filter(!is.na(niph_noise_ha_excel$exposure_mean))

  testthat::expect_equal(
    object = healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = data$exposure_mean,
      # population = sum(data$population_exposed_total),
      # prop_pop_exp = data$population_exposed_total/sum(data$population_exposed_total),
      pop_exp = data$population_exposed_total,
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      dw_central = 0.5, dw_lower = 0.1, dw_upper = 10,
      duration_central = 1, duration_lower = 0.1, duration_upper = 10,
      info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")
      )$health_main$impact_rounded,
    expected = data_raw |>
      dplyr::filter(exposure_category %in% "Total exposed")|>
      dplyr::select(number)|>
      dplyr::pull() |>
      round() / 2 # With dw_central = 0.5 & duration_central = 1 the expected results are half of those we would obtain without dw & duration arguments
  )
})
