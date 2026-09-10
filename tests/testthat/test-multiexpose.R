# QUANTITATIVE TEST ############################################################
## ADDITIVE APPROACH ############################################################

testthat::test_that("results the same |pathway_multiexposure|approach_multiexposure_additive|", {

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
      output_attribute = bestcost_pm_mortality,
      exp_central = 10.9,
      rr_central = 1.031)

  testthat::expect_equal(
    object =
      healthiar::multiexpose(
        output_attribute_exp_1 = bestcost_pm_mortality,
        output_attribute_exp_2 = bestcost_no2_mortality,
        exp_name_1 = "pm2.5",
        exp_name_2 = "no2",
        approach_multiexposure = "additive"
        )$health_main$impact_rounded,
    expected =
      c(0.081 * 1000) # Results on 2025-01-16; Results from BEST-COST T1.4 report (RIVM)
  )
})

testthat::test_that("results the same |fake_multiexposure|approach_additive|", {

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
      output_attribute = bestcost_pm_mortality,
      exp_central = 10.9,
      exp_lower = 9,
      exp_upper = 12,
      rr_lower = 1.02,
      rr_upper = 1.04,
      rr_central = 1.031)

  testthat::expect_equal(
    object =
      healthiar::multiexpose(
        output_attribute_exp_1 = bestcost_pm_mortality,
        output_attribute_exp_2 = bestcost_no2_mortality,
        exp_name_1 = "pm2.5",
        exp_name_2 = "no2",
        approach_multiexposure = "additive"
        )$health_main$impact_rounded,
    expected =
      c(0.081, 0.06, 0.095) * 1000 # Results on 2025-01-16; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
  )
})

testthat::test_that("results the same |fake_multiexposure|approach_multiexposure_additive|", {

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
    output_attribute = bestcost_pm_mortality,
    exp_central = 10.9,
    exp_lower = 10.9 - 1,
    exp_upper = 10.9 + 1,
    rr_central = 1.031,
    rr_lower = 1.031 - 0.005,
    rr_upper = 1.031 + 0.005
  )

  testthat::expect_equal(
    object =
      healthiar::multiexpose(
        output_attribute_exp_1 = bestcost_pm_mortality,
        output_attribute_exp_2 = bestcost_no2_mortality,
        exp_name_1 = "pm2.5",
        exp_name_2 = "no2",
        approach_multiexposure = "additive"
      )$health_detailed$results_raw$impact |> base::round(),
    expected = # Results on 2025-01-20; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
      c(48, 45, 52, 42, 39, 46, 54, 50, 58, 33, 28, 38, 30, 25, 34, 36, 30, 41) # NEW order
      # c(33, 30, 36, 28, 25, 30, 38, 34, 41, 48, 42, 54, 45, 39, 50, 52, 46, 58) # OLD order (from multiexposure with attribute_health call
  )
})

## MULTIPLICATIVE APPROACH ######################################################

testthat::test_that("results the same |pathway_multiexposure|approach_multiexposure_multiplicative|", {

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
      output_attribute = bestcost_pm_mortality,
      exp_central = 10.9,
      rr_central = 1.031,
    )

  testthat::expect_equal(
    object =
      healthiar::multiexpose(
        output_attribute_exp_1 = bestcost_pm_mortality,
        output_attribute_exp_2 = bestcost_no2_mortality,
        exp_name_1 = "pm2.5",
        exp_name_2 = "no2",
        approach_multiexposure = "multiplicative"
        )$health_main$impact_rounded,
    expected =
      c(0.079) * 1000 # Results on 2025-01-16; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
  )
})

testthat::test_that("results the same |fake_multiexposure|approach_multiexposure_multiplicative|", {

  bestcost_pm_mortality <-
    healthiar::attribute_health(
      exp_central = 8.1,
      exp_lower = 7, # Fake lower and upper bound in exp and rr
      exp_upper = 9,
      cutoff_central = 0,
      bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
      rr_central = 1.063,
      rr_lower = 1.05,
      rr_upper = 1.07,
      rr_increment = 10,
      erf_shape = "log_linear")

  bestcost_no2_mortality <-
    healthiar::attribute_mod(
      output_attribute = bestcost_pm_mortality,
      exp_central = 10.9,
      exp_lower = 9,
      exp_upper = 12,
      cutoff_central = 0,
      rr_lower = 1.02,
      rr_upper = 1.04,
      rr_central = 1.031)

  testthat::expect_equal(
    object =
      healthiar::multiexpose(
        output_attribute_exp_1 = bestcost_pm_mortality,
        output_attribute_exp_2 = bestcost_no2_mortality,
        exp_name_1 = "pm2.5",
        exp_name_2 = "no2",
        approach_multiexposure = "multiplicative"
        )$health_main |> dplyr::arrange(erf_ci) |> dplyr::select(impact_rounded) |> base::unlist() |> base::as.numeric(),
    expected =
      c(0.079, 0.059, 0.093) * 1000 # Results on 2025-01-16; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
  )
})

## COMBINED APPROACH ############################################################

testthat::test_that("results the same |pathway_multiexposure|approach_multiexposure_combined|", {

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
      output_attribute = bestcost_pm_mortality,
      exp_central = 10.9,
      exp_lower = 9,
      exp_upper = 12,
      rr_lower = 1.02,
      rr_upper = 1.04,
      rr_central = 1.031)

  testthat::expect_equal(
    object =
      healthiar::multiexpose(
        output_attribute_exp_1 = bestcost_pm_mortality,
        output_attribute_exp_2 = bestcost_no2_mortality,
        exp_name_1 = "pm2.5",
        exp_name_2 = "no2",
        approach_multiexposure = "combined"
        )$health_main |> dplyr::arrange(erf_ci) |> dplyr::select(impact_rounded) |> base::unlist() |> base::as.numeric(),
    expected =
      c(0.079, 0.059, 0.093) * 1000 # Results on 2025-01-16; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
  )
})

testthat::test_that("results the same |fake_multiexposure|approach_multiexposure_combined|", {

  bestcost_pm_mortality <-
    healthiar::attribute_health(
      exp_central = 8.1,
      exp_lower = 7, # Fake lower and upper bound in exp and rr
      exp_upper = 9,
      cutoff_central = 0,
      bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
      rr_central = 1.063,
      rr_lower = 1.05,
      rr_upper = 1.07,
      rr_increment = 10,
      erf_shape = "log_linear")

  bestcost_no2_mortality <-
    healthiar::attribute_mod(
      output_attribute = bestcost_pm_mortality,
      exp_central = 10.9,
      exp_lower = 9,
      exp_upper = 12,
      cutoff_central = 0,
      rr_lower = 1.02,
      rr_upper = 1.04,
      rr_central = 1.031)

  testthat::expect_equal(
    object =
      healthiar::multiexpose(
        output_attribute_exp_1 = bestcost_pm_mortality,
        output_attribute_exp_2 = bestcost_no2_mortality,
        exp_name_1 = "pm2.5",
        exp_name_2 = "no2",
        approach_multiexposure = "combined"
        )$health_main |> dplyr::arrange(erf_ci) |> dplyr::select(impact_rounded) |> base::unlist() |> base::as.numeric(),
    expected =
      c(0.079, 0.059, 0.093) * 1000 # Results on 2025-01-16; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
  )
})

testthat::test_that("results the same |fake_multiexposure|approach_multiexposure_combined|", {

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
      output_attribute = bestcost_pm_mortality,
      exp_central = 10.9,
      exp_lower = 9,
      exp_upper = 12,
      rr_lower = 1.02,
      rr_upper = 1.04,
      rr_central = 1.031)

  testthat::expect_equal(
    object =
      healthiar::multiexpose(
        output_attribute_exp_1 = bestcost_pm_mortality,
        output_attribute_exp_2 = bestcost_no2_mortality,
        exp_name_1 = "pm2.5",
        exp_name_2 = "no2",
        approach_multiexposure = "combined")$health_detailed$results_raw$impact |> base::round(),
    expected =
      c(0.079, 0.059, 0.093, 0.068, 0.051, 0.079, 0.088, 0.065, 0.102) * 1000 # Results on 2025-01-16; Results from BEST-COST task 1.4 report (NIVM), but lower and upper bounds are fake
  )
})

# ERROR OR WARNING ########
## ERROR #########

testthat::test_that("error if the two assessments assume a different life table approach", {

  # The life table projects one cohort, so it needs one single value of
  # approach_exposure. Two assessments with different values used to reach
  # get_impact_with_lifetable() and abort there with
  # "the condition has length > 1"
  data <- base::readRDS(testthat::test_path("testdata", "lifetable_male_ekv_2010.rds"))

  attribute_with_approach_exposure <- function(approach_exposure){
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      exp_central = 10,
      cutoff_central = 0,
      rr_central = 1.045,
      rr_increment = 10,
      erf_shape = "log_linear",
      age_group = data$age,
      sex = base::rep("male", 106),
      population = data$population_male,
      bhd_central = base::as.numeric(data$deaths_natural_male),
      year_of_analysis = 2010,
      min_age = 20,
      approach_exposure = approach_exposure)
  }

  testthat::expect_error(
    object =
      healthiar::multiexpose(
        output_attribute_exp_1 = attribute_with_approach_exposure("single_year"),
        output_attribute_exp_2 = attribute_with_approach_exposure("constant"),
        exp_name_1 = "pm2.5",
        exp_name_2 = "no2",
        approach_multiexposure = "additive"),
    regexp = "needs one single value of approach_exposure")
})

testthat::test_that("error if exposure distribution and approach_multiexposure is not additive", {

  # The exposure categories of two exposures are not paired: category 1 of
  # pm2.5 has nothing to do with category 1 of no2. Before, the multiplicative
  # approach silently multiplied the relative risks of every category of both
  # exposures together
  attribute_one_exposure <- function(exp_central, rr_central){
    healthiar::attribute_health(
      exp_central = exp_central,
      prop_pop_exp = c(0.5, 0.5),
      cutoff_central = 0,
      rr_central = rr_central,
      rr_increment = 10,
      erf_shape = "log_linear",
      bhd_central = 1000)
  }

  testthat::expect_error(
    object =
      healthiar::multiexpose(
        output_attribute_exp_1 = attribute_one_exposure(c(10, 20), 1.10),
        output_attribute_exp_2 = attribute_one_exposure(c(20, 40), 1.05),
        exp_name_1 = "pm2.5",
        exp_name_2 = "no2",
        approach_multiexposure = "multiplicative"),
    regexp = "cannot merge exposure distributions")
})

## WARNING #########


## INFO PER EXPOSURE ###########################################################

testthat::test_that("results the same |multiexpose|info_per_exposure|", {

  # Each exposure can come from an attribute_health() call with its own info,
  # e.g. the name of the pollutant. That info identifies the exposures that are
  # being merged, just like exp_name, so it must not keep them apart:
  # otherwise the relative risks are not multiplied and the impacts are counted
  # once per exposure instead of once in total
  attribute_one_exposure <- function(info, exp_central, rr_central){
    healthiar::attribute_health(
      exp_central = exp_central,
      cutoff_central = 0,
      rr_central = rr_central,
      rr_increment = 10,
      erf_shape = "log_linear",
      bhd_central = 1000,
      info = info)
  }

  output_multiexpose <-
    healthiar::multiexpose(
      output_attribute_exp_1 = attribute_one_exposure("pm2.5", 10, 1.10),
      output_attribute_exp_2 = attribute_one_exposure("no2", 20, 1.05),
      exp_name_1 = "pm2.5",
      exp_name_2 = "no2",
      approach_multiexposure = "multiplicative")

  testthat::expect_equal(
    object = base::unique(output_multiexpose$health_detailed$results_raw$rr_at_exp),
    # 1.10 * 1.05^(20/10), i.e. the two relative risks multiplied
    expected = 1.10 * 1.05^2)
})


## SEVERAL GEO UNITS ###########################################################

testthat::test_that("results the same |multiexpose|approach_multiexposure_multiplicative|several_geo_units|", {

  # The relative risks must be multiplied across the exposures within one geo
  # unit, never across geo units (or sexes, age groups and info subgroups).
  # Grouping only by the _ci columns multiplied every row of the assessment
  # together, which gave both geo units the same (too high) relative risk
  attribute_one_exposure <- function(exp_central, rr_central){
    healthiar::attribute_health(
      exp_central = exp_central,
      geo_id_micro = c("a", "b"),
      cutoff_central = 0,
      rr_central = rr_central,
      rr_increment = 10,
      erf_shape = "log_linear",
      bhd_central = c(1000, 1000))
  }

  output_multiexpose <-
    healthiar::multiexpose(
      output_attribute_exp_1 = attribute_one_exposure(c(10, 20), 1.10),
      output_attribute_exp_2 = attribute_one_exposure(c(20, 40), 1.05),
      exp_name_1 = "pm2.5",
      exp_name_2 = "no2",
      approach_multiexposure = "multiplicative")

  testthat::expect_equal(
    object =
      output_multiexpose$health_main |>
      dplyr::arrange(geo_id_micro) |>
      dplyr::pull(rr_at_exp),
    # geo unit a: 1.10^(10/10) * 1.05^(20/10); geo unit b: 1.10^(20/10) * 1.05^(40/10)
    expected = c(1.10 * 1.05^2, 1.10^2 * 1.05^4))
})
