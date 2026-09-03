## NOTE 2025-04-30 AL: actually all comparisons for summarize_uncertainty are
## "fake", and should be labelled as such - but since it's the way of
## corroborating our results that we have, I classify them as
## "results the same" comparisons.

# QUANTITATIVE TEST ############################################################

## RR ###########################################################################

### SINGLE EXPOSURE #############################################################

testthat::test_that("results the same |pathway_uncertainty|exp_single|erf_rr_increment|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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
    expected = # Results on 2025-10-29; no comparison study
      c(1318, 639, 2239)
  )
})



testthat::test_that("results the same |pathway_uncertainty|exp_single|erf_ar_function|iteration_FALSE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("testdata", "noise_road_lden_stavanger_bergen.rds"))

  data$GEO_ID <- factor(data$GEO_ID, levels = unique(data$GEO_ID))

  data <- data.frame(
    GEO_ID      = levels(data$GEO_ID),
    exp_central = tapply(data$average_cat, data$GEO_ID, mean),
    pop_exp     = tapply(data$ANTALL_PER,  data$GEO_ID, sum),
    population  = tapply(data$totpop,      data$GEO_ID, unique)
  )

  data <- data["Bergen",]

  erf_df <- data.frame(
    dB = seq(30, 85, by = 0.5)
  )

  # Compute AR using the quadratic formula
  erf_df$AR <- 78.9270 - 3.1162 * erf_df$dB + 0.0342 * erf_df$dB^2

  # Create a function using spline interpolation over the data
  spline_fun <- splinefun(
    x = erf_df$dB,
    y = erf_df$AR,
    method = "natural"
  )


  ## healthiar FUNCTION CALL
  results_noise_ha <-   healthiar::attribute_health(
    approach_risk = "absolute_risk",
    exp_central = data$exp_central,
    exp_lower = data$exp_central-5,
    exp_upper = data$exp_central+5,
    population = data$totpop,
    pop_exp = data$pop_exp,
    geo_id_micro =  data$GEO_ID,
    geo_id_macro = "Norway",
    erf_eq_central = spline_fun,
    dw_central = 0.02,
    duration_central = 1,

    info = data.frame(pollutant = "road_noise",

                      outcome = "highly_annoyance")

  )


  results_noise_ha_summarised <- healthiar::summarize_uncertainty(results_noise_ha, n_sim = 10, seed = 123)

  # Assuming SD of 47 and 70, and normal distribution
  expected_impacts <-c(#283, 191,375  , 2 try: 398, 261 , 535
    350.0, 288.0, 507.0)

  ## COMPARE ONLY THE IMPACT_ROUNDED VECTOR
  testthat::expect_equal(
    object   = results_noise_ha_summarised$uncertainty_detailed$uncertainty_by_geo_id_micro$impact_rounded,
    expected = expected_impacts
  )



})

## ASSESSOR:
## Liliana Vázquez, NIPH
## ASSESSMENT DETAILS:
## Bergen highly annoyance
## INPUT DATA DETAILS:
## Add here input data details: defined own function with spline interpolation, summarised the categories
## to the average and checked the +and- 5dB on the exposure.
## Assumed also a SD from the results_noise_ha object


testthat::test_that("results the same |pathway_uncertainty|exp_single|erf_ar_formula|iteration_FALSE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("testdata", "noise_road_lden_stavanger_bergen.rds"))

  data$GEO_ID <- factor(data$GEO_ID, levels = unique(data$GEO_ID))

  data <- data.frame(
    GEO_ID      = levels(data$GEO_ID),
    exp_central = tapply(data$average_cat, data$GEO_ID, mean),
    pop_exp     = tapply(data$ANTALL_PER,  data$GEO_ID, sum),
    population  = tapply(data$totpop,      data$GEO_ID, unique)
  )

  data <-  data["Bergen",]

  ## healthiar FUNCTION CALL
  results_noise_ha <-   healthiar::attribute_health(
    approach_risk = "absolute_risk",
    exp_central = data$exp_central,
    exp_lower = data$exp_central-5,
    exp_upper = data$exp_central+5,
    population = data$totpop,
    pop_exp = data$pop_exp,
    geo_id_micro =  data$GEO_ID,
    geo_id_macro = "Norway",
    erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
    dw_central = 0.02,
    duration_central = 1,

    info = data.frame(pollutant = "road_noise",

                      outcome = "highly_annoyance")

  )


  results_noise_ha_summarised <- healthiar::summarize_uncertainty(results_noise_ha, n_sim = 100,seed = 123)

  # Assuming SD of 47 and 70, and normal distribution
  # from results_noise_ha
  expected_impacts <- c(#283, 209,385 ,
    349.0, 247.0, 506.0)

  ## COMPARE ONLY THE IMPACT_ROUNDED VECTOR
  testthat::expect_equal(
    object   = results_noise_ha_summarised$uncertainty_detailed$uncertainty_by_geo_id_micro$impact_rounded,
    expected = expected_impacts
  )

})

## ASSESSOR:
## Liliana Vázquez, NIPH
## ASSESSMENT DETAILS:
## Bergen highly annoyance
## INPUT DATA DETAILS:
## Add here input data details: defined own function with spline interpolation, summarised the categories
## to the average and checked the +and- 5dB on the exposure.
## Assumed also a SD from the results_noise_ha object

#### ITERATION #################################################################
testthat::test_that("results the same |pathway_uncertainty|exp_single|erf_rr_increment|iteration_True|", {

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
      geo_id_micro = c("a", "b")
    )

  testthat::expect_equal(
    object =
      healthiar::summarize_uncertainty(
        output_attribute = summary_uncertainty_small_iteration,
        n_sim = 100,
        seed = 123
      )$uncertainty_main$impact_rounded,

    expected =
      # Results on 2026-09-03; no comparison study.
      # Only the estimates of geo unit "a" change compared to the previous
      # expectation c(2853, 936, 6531, 2943, 875, 7232), because the bhd
      # interval entered above puts 0.45% of the normal distribution below
      # zero and those values are now truncated instead of mirrored
      c(2908, 979, 6531, 2943, 875, 7232)
  )
})

testthat::test_that("results the same |pathway_uncertainty|exp_single|erf_rr_increment|iteration_TRUE|", {

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
      geo_id_micro = 1:1E1,
      geo_id_macro = c(rep("CH", 5), rep("DE", 5)),
      info = "PM2.5_copd")

  testthat::expect_equal(
    object =
      healthiar::summarize_uncertainty(
        output_attribute = bestcost_pm_copd_geo_short,
        n_sim = 100,
        # Seed entered explicitly. Without it the expectation below only held
        # because the runif_with_seed() calls above happened to leave the RNG
        # in a fixed state
        seed = 123
      )$uncertainty_main$impact_rounded,

    expected = # Results on 2026-09-03; no comparison study
      c(15497, 8007, 24682, 16588, 8486, 25928)
  )
})


testthat::test_that("results the same |pathway_uncertainty|exp_single|erf_ar_function|iteration_TRUE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("testdata", "noise_road_lden_stavanger_bergen.rds"))

  data$GEO_ID <- factor(data$GEO_ID, levels = unique(data$GEO_ID))

  data <- data.frame(
    GEO_ID      = levels(data$GEO_ID),
    exp_central = tapply(data$average_cat, data$GEO_ID, mean),
    pop_exp     = tapply(data$ANTALL_PER,  data$GEO_ID, sum),
    population  = tapply(data$totpop,      data$GEO_ID, unique)
  )



  erf_df <- data.frame(
    dB = seq(30, 85, by = 0.5)
  )

  # Compute AR using the quadratic formula
  erf_df$AR <- 78.9270 - 3.1162 * erf_df$dB + 0.0342 * erf_df$dB^2

  # Create a function using spline interpolation over the data
  spline_fun <- splinefun(
    x = erf_df$dB,
    y = erf_df$AR,
    method = "natural"
  )


  ## healthiar FUNCTION CALL
  results_noise_ha <-   healthiar::attribute_health(
    approach_risk = "absolute_risk",
    exp_central = data$exp_central,
    exp_lower = data$exp_central-5,
    exp_upper = data$exp_central+5,
    population = data$totpop,
    pop_exp = data$pop_exp,
    geo_id_micro =  data$GEO_ID,
    geo_id_macro = "Norway",
    erf_eq_central = spline_fun,
    dw_central = 0.02,
    duration_central = 1,

    info = data.frame(pollutant = "road_noise",

                      outcome = "highly_annoyance")

  )


  results_noise_ha_summarised <- healthiar::summarize_uncertainty(results_noise_ha, n_sim = 10, seed = 123)

  # Assuming SD of 47 and 70, and normal distribution
  expected_impacts <-c(291.0, 228.0, 453.0, 357.0, 280.0, 479.0)

  ## COMPARE ONLY THE IMPACT_ROUNDED VECTOR
  testthat::expect_equal(
    object   = results_noise_ha_summarised$uncertainty_detailed$uncertainty_by_geo_id_micro$impact_rounded,
    expected = expected_impacts
  )

})

## ASSESSOR:
## Liliana Vázquez, NIPH
## ASSESSMENT DETAILS:
## Stavanger and Bergen highly annoyance
## INPUT DATA DETAILS:
## Add here input data details: defined own function with spline interpolation, summarised the categories
## to the average and checked the +and- 5dB on the exposure.
## Assumed also a SD from the results_noise_ha object



testthat::test_that("results the same |pathway_uncertainty|exp_single|erf_ar_formula|iteration_TRUE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("testdata", "noise_road_lden_stavanger_bergen.rds"))

  data$GEO_ID <- factor(data$GEO_ID, levels = unique(data$GEO_ID))

  data <- data.frame(
    GEO_ID      = levels(data$GEO_ID),
    exp_central = tapply(data$average_cat, data$GEO_ID, mean),
    pop_exp     = tapply(data$ANTALL_PER,  data$GEO_ID, sum),
    population  = tapply(data$totpop,      data$GEO_ID, unique)
  )


  ## healthiar FUNCTION CALL
  results_noise_ha <-   healthiar::attribute_health(
    approach_risk = "absolute_risk",
    exp_central = data$exp_central,
    exp_lower = data$exp_central-5,
    exp_upper = data$exp_central+5,
    population = data$totpop,
    pop_exp = data$pop_exp,
    geo_id_micro =  data$GEO_ID,
    geo_id_macro = "Norway",
    erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
    dw_central = 0.02,
    duration_central = 1,

    info = data.frame(pollutant = "road_noise",

                      outcome = "highly_annoyance")

  )


  results_noise_ha_summarised <- healthiar::summarize_uncertainty(results_noise_ha, n_sim = 100,seed = 123)

  # Assuming SD of 47 and 70, and normal distribution
  expected_impacts <-c(287.0, 201.0, 418.0, 378.0, 276.0, 536.0)

  ## COMPARE ONLY THE IMPACT_ROUNDED VECTOR
  testthat::expect_equal(
    object   = results_noise_ha_summarised$uncertainty_detailed$uncertainty_by_geo_id_micro$impact_rounded,
    expected = expected_impacts
  )

})

## ASSESSOR:
## Liliana Vázquez, NIPH
## ASSESSMENT DETAILS:
## Bergen highly annoyance
## INPUT DATA DETAILS:
## Add here input data details: defined own function with spline interpolation, summarised the categories
## to the average and checked the +and- 5dB on the exposure.
## Assumed also a SD from the results_noise_ha object



#### YLD ########################################################################

testthat::test_that("results the same |pathway_uncertainty|exp_single|erf_rr_increment|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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

    expected = # Results on 2025-10-29; no comparison study
      c(706, 230, 1309)
  )
})

### EXPOSURE DISTRIBUTION #######################################################

testthat::test_that("results the same |pathway_uncertainty|exp_dist|erf_rr_increment|iteration_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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

    expected = # Results on 2025-10-29; no comparison study
      c(1146, 910, 1478)
  )
})



### YLD #########################################################################

testthat::test_that("results the same |pathway_uncertainty|exp_dist|erf_ar_formula|iteration_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ha.rds"))
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

    expected =
      # Results on 2026-09-03; no comparison study.
      # The duration interval entered above (0.1, 1, 10) is strongly
      # asymmetric, so 35% of the normal distribution falls below zero.
      # The lower estimate changes a lot compared to the previous expectation
      # c(171674, 2430, 614420), because those values are now truncated
      # instead of being mirrored onto the positive side
      c(175252, 7412, 670185)
  )
})

### TRUNCATION AT ZERO #########################################################

testthat::test_that("results correct |pathway_uncertainty|exp_single|erf_ar_formula|iteration_FALSE|", {

  # The duration confidence interval entered below is so wide and asymmetric
  # that about 35% of the corresponding normal distribution falls below zero.
  # As duration cannot be negative, the distribution is truncated at zero.
  # The simulated values must therefore follow the normal distribution
  # truncated at zero and not the mirrored one obtained with abs() before,
  # which returns a median of about 1.84 instead of 2.13.

  results_duration <-
    healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = 65,
      pop_exp = 1E5,
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      dw_central = 0.02,
      duration_central = 1,
      duration_lower = 0.1,
      duration_upper = 10)

  simulated_duration <-
    healthiar::summarize_uncertainty(
      output_attribute = results_duration,
      n_sim = 1E4,
      seed = 123)$uncertainty_detailed$impact_by_sim$duration

  # Median of the normal distribution truncated at zero
  sd_duration <- (10 - 0.1) / (2 * stats::qnorm(0.975))
  prob_at_zero <- stats::pnorm(q = 0, mean = 1, sd = sd_duration)

  testthat::expect_equal(
    object = stats::median(simulated_duration),
    expected =
      stats::qnorm(p = prob_at_zero + 0.5 * (1 - prob_at_zero),
                   mean = 1,
                   sd = sd_duration),
    tolerance = 0.02)
})

# COMPARE ########

testthat::test_that("results the same |pathway_uncertainty_compare|exp_dist|erf_ar_formula|iteration_TRUE|", {

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

    expected = # Results on 2025-10-29; no comparison study
      c(545.0, 171, 1107)
  )
})

## PIF #######

testthat::test_that("results the same |pathway_uncertainty_compare_pif|exp_single|erf_rr_increment|iteration_FALSE|", {

  # Before, this call stopped with the error message of the data validation
  # saying that no uncertainty was entered, because compare() stored the input
  # table of the pif approach in a different way than the one of the delta
  # approach and the comparison was therefore not identified as such

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
      output_attribute = rr_scenario_1,
      exp_central = 7.5,
      exp_lower = 6.2,
      exp_upper = 8.1)

  rr_comparison_pif <-
    healthiar::compare(
      output_attribute_scen_1 = rr_scenario_1,
      output_attribute_scen_2 = rr_scenario_2,
      approach_comparison = "pif")

  testthat::expect_equal(
    object =
      healthiar::summarize_uncertainty(
        output_attribute = rr_comparison_pif,
        n_sim = 100,
        seed = 122)$uncertainty_main$impact_rounded,

    expected =
      # Results on 2026-09-03; no comparison study.
      # The central estimate is close to the one obtained without
      # Monte Carlo simulation, i.e. c(556, 291, 820)
      c(561, 175, 1156)
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
      geo_id_micro = c("a", "b"),
      geo_id_macro = rep("ch", 2))

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
        n_sim = 100,
        # Seed entered explicitly. Without it this test inherited the RNG state
        # left by the preceding tests, so it was not reproducible standalone
        seed = 123)$uncertainty_main$impact_rounded,
    expected = # Results on 2026-09-03; no comparison study
      c(1111, 737, 1584)
  )
})

## SHARED SIMULATED VALUES ACROSS SCENARIOS #######

testthat::test_that("results correct |pathway_uncertainty_compare|exp_single|erf_rr_increment|iteration_FALSE|", {

  # The variables that are common to both scenarios (here rr and bhd) must take
  # the same simulated value in both scenarios of each simulation.
  # Therefore, comparing a scenario with an identical one must result in a
  # delta of exactly zero, also when the user enters no seed.
  # Otherwise the comparison would show a variability that does not exist,
  # because it is the very same variable in both scenarios.

  scen_1 <-
    healthiar::attribute_health(
      erf_shape = "log_linear",
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      exp_central = 8,
      exp_lower = 7,
      exp_upper = 9,
      cutoff_central = 5,
      bhd_central = 1E5,
      bhd_lower = 5E4,
      bhd_upper = 2E5)

  scen_2_identical <-
    healthiar::attribute_mod(
      output_attribute = scen_1,
      # Same values as in scenario 1
      exp_central = 8,
      exp_lower = 7,
      exp_upper = 9)

  comparison_of_identical_scenarios <-
    healthiar::compare(
      output_attribute_scen_1 = scen_1,
      output_attribute_scen_2 = scen_2_identical,
      approach_comparison = "delta")

  testthat::expect_equal(
    object =
      healthiar::summarize_uncertainty(
        output_attribute = comparison_of_identical_scenarios,
        # No seed on purpose (see comment above)
        n_sim = 100)$uncertainty_main$impact,
    expected = c(0, 0, 0)
  )
})

# ERROR OR WARNING ########
## ERROR #########

testthat::test_that("error_if_erf_eq |pathway_uncertainty|exp_dist|erf_ar_formula|iteration_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ha.rds"))
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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ha.rds"))
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
      geo_id_micro = rep(1:3, each = 5),
      geo_id_macro = rep("CH", 3*5),
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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ha.rds"))
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

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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


testthat::test_that("error_if_erf_eq_rr_function |pathway_uncertainty|exp_dist|erf_rr_function|iteration_FALSE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("testdata", "ozone_copd_mort_2016.rds"))
  erf<-splinefun(data$x, data$y, method="natural")
  erf_l<-splinefun(data$x, data$y_l, method="natural")
  erf_u<-splinefun(data$x, data$y_u, method="natural")


  bestcost_pm_copd_with_summary_uncertainty <-
    healthiar::attribute_health(
      erf_eq_central = erf,
      erf_eq_lower = erf_l,
      erf_eq_upper = erf_u,
      prop_pop_exp = 1,
      exp_central = 84.1, # exposure distribution for ozone
      exp_lower = NULL,
      exp_upper = NULL,
      cutoff_central = 0,
      cutoff_lower = NULL,
      cutoff_upper = NULL,
      bhd_central =  29908, #COPD mortality in Germany 2016
      bhd_lower = NULL,
      bhd_upper = NULL,
    )

  testthat::expect_error(
    object =
      healthiar::summarize_uncertainty(
        output_attribute = bestcost_pm_copd_with_summary_uncertainty,
        n_sim = 100,
        seed = 122
      )$uncertainty_main$impact_rounded,
    regexp = "Sorry, the summary of uncertainty for erf_eq_... is not currently supported."
  )
})
## ASSESSOR: Susanne Breitner-Busch, LMU Munich
## ASSESSMENT DETAILS: https://www.umweltbundesamt.de/publikationen/quantifizierung-der-krankheitslast-verursacht-durch#:~:text=Beschrieben%20werden%20die%20gesundheitlichen%20Effekte%20in%20der%20deutschen,f%C3%BCr%20die%20Jahre%202007%20-%202016%20quantifiziert%20wurden.
## INPUT DATA DETAILS: Modelled ozone exposure, real COPD mortality data from Germany, 2016


## WARNING #########

