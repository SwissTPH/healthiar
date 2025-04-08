# ADDITIVE APPROACH ############################################################

testthat::test_that("results correct rr multiple exposure additive approach no variable uncertainties", {

  bestcost_pm_mortality <-
    healthiar::attribute_health(
      exp_central = 8.1,
      cutoff_central =  0,
      bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
      rr_central = 1.063,
      rr_increment = 10,
      erf_shape = "log_linear")

  bestcost_no2_mortality <-
    healthiar::attribute_mod(
      output_attribute_1 = bestcost_pm_mortality,
      exp_central = 10.9,
      rr_central = 1.031)

  testthat::expect_equal(
    object =
      healthiar::get_multiexposure(
        output_attribute_1 = bestcost_pm_mortality,
        output_attribute_2 = bestcost_no2_mortality,
        exposure_name_1 = "pm2.5",
        exposure_name_2 = "no2",
        approach = "additive") |>
      helper_extract_main_results(),
    expected =
      c(0.081 * 1000) # Results on 2025-01-16; Results from BEST-COST T1.4 report (RIVM)
  )
})

testthat::test_that("results correct rr multiple exposure additive approach with variable uncertainties", {

  bestcost_pm_mortality <-
    healthiar::attribute_health(
      exp_central = 8.1,
      exp_lower = 7, # Fake lower and upper bound in exp and rr
      exp_upper = 9,
      cutoff_central =  0,
      bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
      rr_central = 1.063,
      rr_lower = 1.05,
      rr_upper = 1.07,
      rr_increment = 10,
      erf_shape = "log_linear")

  bestcost_no2_mortality <-
    healthiar::attribute_mod(
      output_attribute_1 = bestcost_pm_mortality,
      exp_central = 10.9,
      exp_lower = 9,
      exp_upper = 12,
      rr_lower = 1.02,
      rr_upper = 1.04,
      rr_central = 1.031)

  testthat::expect_equal(
    object =
      healthiar::get_multiexposure(
        output_attribute_1 = bestcost_pm_mortality,
        output_attribute_2 = bestcost_no2_mortality,
        exposure_name_1 = "pm2.5",
        exposure_name_2 = "no2",
        approach = "additive") |>
      helper_extract_main_results(),
    expected =
      c(0.081, 0.06, 0.095) * 1000 # Results on 2025-01-16; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
  )
})

testthat::test_that("detailed results the same rr multiple exposure additive approach with variable uncertainties", {

  bestcost_pm_mortality <- healthiar::attribute_health(
    exp_central = 8.1,
    exp_lower = 8.1 - 1,
    exp_upper = 8.1 + 1,
    cutoff_central = 0,
    bhd_central = 1000,
    rr_central = 1.063,
    rr_lower = 1.063 - 0.005,
    rr_upper = 1.063 + 0.005,
    rr_increment = 10,
    erf_shape = "log_linear"
  )

  bestcost_no2_mortality <- healthiar::attribute_mod(
    output_attribute_1 = bestcost_pm_mortality,
    exp_central = 10.9,
    exp_lower = 10.9 - 1,
    exp_upper = 10.9 + 1,
    rr_central = 1.031,
    rr_lower = 1.031 - 0.005,
    rr_upper = 1.031 + 0.005
  )

  testthat::expect_equal(
    object =
      healthiar::get_multiexposure(
        output_attribute_1 = bestcost_pm_mortality,
        output_attribute_2 = bestcost_no2_mortality,
        exposure_name_1 = "pm2.5",
        exposure_name_2 = "no2",
        approach = "additive"
      ) |>
      helper_extract_detailed_results() |>
      round(),
    expected = # Results on 2025-01-20; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
      c(48, 45, 52, 42, 39, 46, 54, 50, 58, 33, 28, 38, 30, 25, 34, 36, 30, 41) # NEW order
      # c(33, 30, 36, 28, 25, 30, 38, 34, 41, 48, 42, 54, 45, 39, 50, 52, 46, 58) # OLD order (from multiexposure with attribute_health call
  )
})

# MULTIPLICATIVE APPROACH ######################################################

# DELETE UNCERTAINTY ARGUMENTS ONCE FINISHED ####
testthat::test_that("results correct rr multiple exposure multiplicative approach", {

  bestcost_pm_mortality <-
    healthiar::attribute_health(
      exp_central = 8.1,
      cutoff_central =  0,
      bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
      rr_central = 1.063,
      rr_increment = 10,
      erf_shape = "log_linear")

  bestcost_no2_mortality <-
    healthiar::attribute_mod(
      output_attribute_1 = bestcost_pm_mortality,
      exp_central = 10.9,
      rr_central = 1.031,
    )

  testthat::expect_equal(
    object =
      healthiar::get_multiexposure(
        output_attribute_1 = bestcost_pm_mortality,
        output_attribute_2 = bestcost_no2_mortality,
        exposure_name_1 = "pm2.5",
        exposure_name_2 = "no2",
        approach = "multiplicative") |>
      helper_extract_main_results(),
    expected =
      c(0.079) * 1000 # Results on 2025-01-16; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
  )
})

testthat::test_that("results correct rr multiple exposure multiplicative approach no cutoff", {

  bestcost_pm_mortality <-
    healthiar::attribute_health(
      exp_central = 8.1,
      exp_lower = 7, # Fake lower and upper bound in exp and rr
      exp_upper = 9,
      bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
      rr_central = 1.063,
      rr_lower = 1.05,
      rr_upper = 1.07,
      rr_increment = 10,
      erf_shape = "log_linear")

  bestcost_no2_mortality <-
    healthiar::attribute_mod(
      output_attribute_1 = bestcost_pm_mortality,
      exp_central = 10.9,
      exp_lower = 9,
      exp_upper = 12,
      rr_lower = 1.02,
      rr_upper = 1.04,
      rr_central = 1.031)

  testthat::expect_equal(
    object =
      healthiar::get_multiexposure(
        output_attribute_1 = bestcost_pm_mortality,
        output_attribute_2 = bestcost_no2_mortality,
        exposure_name_1 = "pm2.5",
        exposure_name_2 = "no2",
        approach = "multiplicative") |>
      helper_extract_main_results(),
    expected =
      c(0.079, 0.059, 0.093) * 1000 # Results on 2025-01-16; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
  )
})

# COMBINED APPROACH ############################################################

testthat::test_that("results correct rr multiple exposure combined approach", {

  bestcost_pm_mortality <-
    healthiar::attribute_health(
      exp_central = 8.1,
      exp_lower = 7, # Fake lower and upper bound in exp and rr
      exp_upper = 9,
      cutoff_central =  0,
      bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
      rr_central = 1.063,
      rr_lower = 1.05,
      rr_upper = 1.07,
      rr_increment = 10,
      erf_shape = "log_linear")

  bestcost_no2_mortality <-
    healthiar::attribute_mod(
      output_attribute_1 = bestcost_pm_mortality,
      exp_central = 10.9,
      exp_lower = 9,
      exp_upper = 12,
      rr_lower = 1.02,
      rr_upper = 1.04,
      rr_central = 1.031)

  testthat::expect_equal(
    object =
      healthiar::get_multiexposure(
        output_attribute_1 = bestcost_pm_mortality,
        output_attribute_2 = bestcost_no2_mortality,
        exposure_name_1 = "pm2.5",
        exposure_name_2 = "no2",
        approach = "combined") |>
      helper_extract_main_results(),
    expected =
      c(0.079, 0.059, 0.093) * 1000 # Results on 2025-01-16; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
  )
})

testthat::test_that("results correct rr multiple exposure combined approach without cutoff", {

  bestcost_pm_mortality <-
    healthiar::attribute_health(
      exp_central = 8.1,
      exp_lower = 7, # Fake lower and upper bound in exp and rr
      exp_upper = 9,

      bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
      rr_central = 1.063,
      rr_lower = 1.05,
      rr_upper = 1.07,
      rr_increment = 10,
      erf_shape = "log_linear")

  bestcost_no2_mortality <-
    healthiar::attribute_mod(
      output_attribute_1 = bestcost_pm_mortality,
      exp_central = 10.9,
      exp_lower = 9,
      exp_upper = 12,
      rr_lower = 1.02,
      rr_upper = 1.04,
      rr_central = 1.031)

  testthat::expect_equal(
    object =
      healthiar::get_multiexposure(
        output_attribute_1 = bestcost_pm_mortality,
        output_attribute_2 = bestcost_no2_mortality,
        exposure_name_1 = "pm2.5",
        exposure_name_2 = "no2",
        approach = "combined") |>
      helper_extract_main_results(),
    expected =
      c(0.079, 0.059, 0.093) * 1000 # Results on 2025-01-16; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
  )
})

testthat::test_that("detailed results correct rr multiple exposure combined approach", {

  bestcost_pm_mortality <-
    healthiar::attribute_health(
      exp_central = 8.1,
      exp_lower = 7, # Fake lower and upper bound in exp and rr
      exp_upper = 9,
      cutoff_central =  0,
      bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
      rr_central = 1.063,
      rr_lower = 1.05,
      rr_upper = 1.07,
      rr_increment = 10,
      erf_shape = "log_linear")

  bestcost_no2_mortality <-
    healthiar::attribute_mod(
      output_attribute_1 = bestcost_pm_mortality,
      exp_central = 10.9,
      exp_lower = 9,
      exp_upper = 12,
      rr_lower = 1.02,
      rr_upper = 1.04,
      rr_central = 1.031)

  testthat::expect_equal(
    object =
      healthiar::get_multiexposure(
        output_attribute_1 = bestcost_pm_mortality,
        output_attribute_2 = bestcost_no2_mortality,
        exposure_name_1 = "pm2.5",
        exposure_name_2 = "no2",
        approach = "combined") |>
      helper_extract_detailed_results() |>
      round(),
    expected =
      c(0.051, 0.059, 0.065, 0.068, 0.079, 0.079, 0.088, 0.093, 0.102) * 1000 # Results on 2025-01-16; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
  )
})
