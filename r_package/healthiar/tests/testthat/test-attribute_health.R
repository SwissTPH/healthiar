testthat::test_that("result correct rr with single exposure and rr CIs", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        cutoff_central = data$cut_off_value,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        rr_central = data$relative_risk,
        rr_lower = data$relative_risk_lower,
        rr_upper = data$relative_risk_upper,
        erf_increment = 10,
        erf_shape = "log_linear",
        info = paste0(data$pollutant,"_", data$evaluation_name)
      ) |>
      helper_extract_main_results(),
    expected =
      data |>
      dplyr::select(estimated_number_of_attributable_cases_central,
                    estimated_number_of_attributable_cases_lower,
                    estimated_number_of_attributable_cases_upper)|>
      base::as.numeric()
    )
})

testthat::test_that("result correct rr with single exposure value and only rr_central", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        cutoff_central = data$cut_off_value,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        rr_central = data$relative_risk,
        erf_increment = 10,
        erf_shape = "log_linear",
        info = paste0(data$pollutant,"_", data$evaluation_name)
        ) |>
      helper_extract_main_results(),
    expected =
      data |>
      dplyr::select(estimated_number_of_attributable_cases_central)|>
      base::as.numeric()
    )
})


testthat::test_that("no error rr single exposure value with with uncertainties in 4 input variables", {

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
        erf_increment = 10,
        erf_shape = "log_linear",
        info = paste0(data$pollutant,"_", data$evaluation_name)
        )
    )
})

testthat::test_that("main result correct rr single exposure value with uncertainties in 4 input variables", {

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
        erf_increment = 10,
        erf_shape = "log_linear",
        info = paste0(data$pollutant,"_", data$evaluation_name)
        ) |>
      helper_extract_main_results(),
  expected =
    data |>
    dplyr::select(estimated_number_of_attributable_cases_central,
                  estimated_number_of_attributable_cases_lower,
                  estimated_number_of_attributable_cases_upper)|>
    base::as.numeric()
  )
})

testthat::test_that("number of rows in detailed results correct rr single exposure value with uncertainties in 4 input variables", {

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
        erf_increment = 10,
        erf_shape = "log_linear",
        info = paste0(data$pollutant,"_", data$evaluation_name)
        ) |>
      purrr::pluck("health_detailed") |>
      purrr::pluck("raw") |>
      base::nrow(),
    expected =
      3^4 # CI's in 4 input variables
      )

})

testthat::test_that("results correct user-defined erf (mrbrt) with splinefun and uncertainties erf & with cutoff of 5", {

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
            method = "natural"),
        erf_eq_lower = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean,
          method = "natural"),
        erf_eq_upper = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean + 0.01,
          method = "natural")
      ) |>
      helper_extract_main_results(),
    expected =
      c(32,32,76) # Results on 19 Dec 2024
  )
})

testthat::test_that("results correct user-defined erf (mrbrt) with splinefun and uncertainties erf & no cutoff", {

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
            method = "natural"),
        erf_eq_lower = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean,
          method = "natural"),
        erf_eq_upper = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean + 0.01,
          method = "natural")
      ) |>
      helper_extract_main_results(),
    expected =
      c(249,249,289) # Results on 15 Jan 2024
  )
})

testthat::test_that("results correct rr iteration with exposure distribution and uncertainties in rr and exp", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = as.list(runif_with_seed(1E4, 8.0, 9.0, 1)),
        exp_lower = as.list(runif_with_seed(1E4, 8.0, 9.0, 1)-0.1),
        exp_upper = as.list(runif_with_seed(1E4, 8.0, 9.0, 1)+0.1),
        cutoff_central = 5,
        bhd_central = as.list(runif_with_seed(1E4, 25000, 35000, 1)),
        rr_central = 1.369,
        rr_lower = 1.124,
        rr_upper = 1.664,
        erf_increment = 10,
        erf_shape = "log_linear",
        population = as.list(rep(1E6, 1E4)),
        geo_id_raw = 1:1E4,
        geo_id_aggregated = rep("CH", 1E4),
        info = "PM2.5_copd") |>
      helper_extract_main_results(),
    expected =
      c(31460722, 12120764, 49312859) # Results on 5 November 2024
  )
})

testthat::test_that("results correct rr single exposure value and user-defined ERF (using stats::approxfun) and cutoff equals 5", {

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
        info = paste0(data$pollutant,"_", data$evaluation_name)) |>
      helper_extract_main_results(),
    expected =
      c(1057) # Results on 10 October 2024 (with cutoff = 5 = data$cut_off_value); no study to compare bestcost results
  )
})

testthat::test_that("results correct rr single exposure value and user-defined ERF (using stats::splinefun) and cutoff equals 0", {

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
        info = paste0(data$pollutant,"_", data$evaluation_name)) |>
      helper_extract_main_results(),
    expected =
      c(2263) # Results on 10 October 2024 (with cutoff = 0); no study to compare bestcost results
  )
})

testthat::test_that("results correct rr single exposure value and user-defined ERF (using stats::approxfun) and cutoff equals 0", {

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
        info = paste0(data$pollutant,"_", data$evaluation_name)) |>
      helper_extract_main_results(),
    expected =
      c(1052) # Results on 10 October 2024 (with cutoff = 5); no study to compare bestcost results
  )
})

testthat::test_that("results correct rr exposure distribution", {

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
        erf_increment = 10,
        erf_shape = "log_linear",
        info = data.frame(pollutant = "road_noise", outcome = "YLD")) |>
      helper_extract_main_results(),
    expected =
      data_raw |>
      dplyr::filter(exposure_category %in% "Total exposed")|>
      dplyr::select(daly)|>
      dplyr::pull() |>
      round()
    )
})

testthat::test_that("results correct ar", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))
  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = data$exposure_mean,
        population = sum(data$population_exposed_total),
        prop_pop_exp = data$population_exposed_total/sum(data$population_exposed_total),
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")) |>
      helper_extract_main_results(),
    expected =
      data_raw |>
      dplyr::filter(exposure_category %in% "Total exposed")|>
      dplyr::select(number)|>
      dplyr::pull() |>
      round()
  )
})

testthat::test_that("no error ar iteration", {

  testthat::expect_no_error(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = list(runif_with_seed(5,8,10,1),
                           runif_with_seed(5,8,10,2),
                           runif_with_seed(5,8,10,3)),
        population = list(runif_with_seed(1,5E3,1E4,1),
                          runif_with_seed(1,5E3,1E4,2),
                          runif_with_seed(1,5E3,1E4,3)),
        prop_pop_exp = list(runif_with_seed(5,0,1,1),
                            runif_with_seed(5,0,1,2),
                            runif_with_seed(5,0,1,3)),
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        geo_id_raw = 1:3,
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance"))
)
})

testthat::test_that("results correct rr yld", {

  testthat::expect_equal(
    object =
      healthiar::attribute_yld(
        exp_central = 8.85,
        cutoff_central = 5,
        bhd_central = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        erf_increment = 10,
        erf_shape = "log_linear",
        dw_central = 0.5, dw_lower = 0.1, dw_upper = 10,
        duration_central = 1, duration_lower = 0.5, duration_upper = 10) |>
      helper_extract_main_results(),
    expected =
      c(525, 277, 768) # Result on 16 May 2024; no study to compare bestcost results
  )
})

testthat::test_that("results correct rr multiple exposure additive approach", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_multiexposure = "additive",
        exp_central = c("pm2.5" = 8.1, "no2" =  10.9),
        cutoff_central =  setNames(c(0, 0), c("pm2.5", "no2")),
        bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
        rr_central = setNames(c(1.063, 1.031), c("pm2.5", "no2")),
        erf_increment = setNames(c(10, 10), c("pm2.5", "no2")),
        erf_shape = "log_linear") |>
      helper_extract_main_results(),
    expected =
      c(0.081 * 1000) # Unsure whether numbers from a study...; Results on 16 Jan 2025
  )
})

testthat::test_that("results correct rr multiple exposure multiplicative approach", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_multiexposure = "multiplicative",
        exp_central = c("pm2.5" = 8.1, "no2" =  10.9),
        cutoff_central =  setNames(c(0, 0), c("pm2.5", "no2")),
        bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
        rr_central = setNames(c(1.063, 1.031), c("pm2.5", "no2")),
        erf_increment = setNames(c(10, 10), c("pm2.5", "no2")),
        erf_shape = "log_linear") |>
      helper_extract_main_results(),
    expected =
      c(0.079 * 1000) # Unsure whether numbers from a study...; Results on 16 Jan 2025
  )
})

testthat::test_that("results correct rr multiple exposure combined approach", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_multiexposure = "combined",
        exp_central = c("pm2.5" = 8.1, "no2" =  10.9),
        cutoff_central =  setNames(c(0, 0), c("pm2.5", "no2")),
        bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
        rr_central = setNames(c(1.063, 1.031), c("pm2.5", "no2")),
        erf_increment = setNames(c(10, 10), c("pm2.5", "no2")),
        erf_shape = "log_linear") |>
      helper_extract_main_results(),
    expected =
      c(0.079 * 1000) # Unsure whether numbers from a study...; Results on 16 Jan 2025
  )
})

