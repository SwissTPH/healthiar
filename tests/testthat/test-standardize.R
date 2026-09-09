# QUANTITATIVE TEST ############################################################

## ONLY ATTRIBUTE ############################################################

### ONE GEO UNIT ############################################################



testthat::test_that("results the same |pathway_standardize|single_geo|", {

  bestcost_pm_mortality <-
    healthiar::attribute_health(
      age_group = c("below_40", "above_40"),
      exp_central = c(8.1, 10.9),
      cutoff_central =  0,
      bhd_central = c(1000, 4000),
      rr_central = 1.063,
      rr_increment = 10,
      erf_shape = "log_linear",
      population = c(1E5, 5E5))

  bestcost_pm_mortality_below_40 <-
    healthiar::attribute_health(
      exp_central = 8.1,
      cutoff_central =  0,
      bhd_central = 1000,
      rr_central = 1.063,
      rr_increment = 10,
      erf_shape = "log_linear",
      population = 1E5)

  bestcost_pm_mortality_40_plus <-
    healthiar::attribute_mod(
      output_attribute = bestcost_pm_mortality_below_40,
      bhd_central = 4000,
      exp_central = 10.9,
      population = 5E5)

  #### WITH REF_PROP_POP ############################################################

  testthat::expect_equal(
    object =
      healthiar::standardize(
        output_attribute = bestcost_pm_mortality,
        age_group = c("below_40", "above_40"),
        ref_prop_pop = c(0.5, 0.5))$health_main$impact_per_100k_inhab,

    expected =  base::sum(
      bestcost_pm_mortality_below_40$health_main$impact_per_100k_inhab * 0.5,
      bestcost_pm_mortality_40_plus$health_main$impact_per_100k_inhab * 0.5)
      # No study behind.
      # Fake numbers to check consistency of result overtime. Results on 2025-01-16
  )

  #### WITHOUT REF_PROP_POP ############################################################

  testthat::expect_equal(
    object =
      healthiar::standardize(
        output_attribute = bestcost_pm_mortality,
        age_group = c("below_40", "above_40")
        #,
        #ref_prop_pop = c(0.5, 0.5) # Deactivating ref_prop_pop
      )$health_main$impact_per_100k_inhab,

    expected =  base::sum(
      bestcost_pm_mortality_below_40$health_main$impact_per_100k_inhab * 0.1666667,
      bestcost_pm_mortality_40_plus$health_main$impact_per_100k_inhab * 0.8333333)
    # No study behind.
    # Fake numbers to check consistency of result overtime. Results on 2025-01-16
  )

})



### MULTIPLE GEO UNITS ############################################################


testthat::test_that("results the same |pathway_standardize|multi_geo|", {

  bestcost_pm_mortality_multigeo <-
    healthiar::attribute_health(
      geo_id_micro = c("a", "a", "b", "b"),
      age_group = c("below_40", "above_40", "below_40", "above_40"),
      exp_central = c(8.1, 10.9, 7.1, 9.9),
      cutoff_central =  0,
      bhd_central = c(1000, 4000, 2000, 8000),
      rr_central = 1.063,
      rr_increment = 10,
      erf_shape = "log_linear",
      population = c(1E5, 5E5, 2E5, 1E6))

  bestcost_pm_mortality_below_40_multigeo <-
    healthiar::attribute_health(
      exp_central = c(8.1, 7.1),
      cutoff_central =  0,
      bhd_central = c(1000, 2000),
      rr_central = 1.063,
      rr_increment = 10,
      erf_shape = "log_linear",
      population = c(1E5, 2E5),
      geo_id_micro = c("a", "b"))

  bestcost_pm_mortality_40_plus_multigeo <-
    healthiar::attribute_mod(
      output_attribute = bestcost_pm_mortality_below_40_multigeo,
      bhd_central = c(4000, 8000),
      exp_central = c(10.9, 9.9),
      population = c(5E5, 1E6))

  #### WITHOUT REF_PROP_POP ############################################################

  testthat::expect_equal(
    object =
      healthiar::standardize(
        output_attribute = bestcost_pm_mortality_multigeo,
        age_group = c("below_40", "above_40"),
        ref_prop_pop = c(0.5, 0.5))$health_main$impact_per_100k_inhab,

    expected =
      bestcost_pm_mortality_below_40_multigeo$health_main$impact_per_100k_inhab * 0.5 +
      bestcost_pm_mortality_40_plus_multigeo$health_main$impact_per_100k_inhab * 0.5
    # No study behind.
    # Fake numbers to check consistency of result overtime. Results on 2025-01-16
  )

  #### WITHOUT REF_PROP_POP ############################################################
  testthat::expect_equal(
    object =
      healthiar::standardize(
        output_attribute = bestcost_pm_mortality_multigeo,
        age_group = c("below_40", "above_40"),
        #ref_prop_pop = c(0.5, 0.5)
      )$health_main$impact_per_100k_inhab,

    expected =
      bestcost_pm_mortality_below_40_multigeo$health_main$impact_per_100k_inhab * 0.1666667 +
      bestcost_pm_mortality_40_plus_multigeo$health_main$impact_per_100k_inhab * 0.8333333
    # No study behind.
    # Fake numbers to check consistency of result overtime. Results on 2026-01-16
  )
})




## IN COMBINATION WITH COMPARE() ############################################################

testthat::test_that("results the same |pathway_standardize|single_geo|", {

  mortality_1 <-
    healthiar::attribute_health(
      age_group = c("below_40", "above_40"),
      exp_central = c(8.1, 10.9),
      cutoff_central =  0,
      bhd_central = c(1000, 4000),
      rr_central = 1.063,
      rr_increment = 10,
      erf_shape = "log_linear",
      population = c(1E5, 5E5))

  std_mortality_1 <-
    healthiar::standardize(
      output_attribute = mortality_1,
      age_group = c("below_40", "above_40"),
      ref_prop_pop = c(0.5, 0.5))

  mortality_2 <-
    healthiar::attribute_health(
      age_group = c("below_40", "above_40"),
      exp_central = c(6.1, 7.9),
      cutoff_central =  0,
      bhd_central = c(1000, 4000),
      rr_central = 1.063,
      rr_increment = 10,
      erf_shape = "log_linear",
      population = c(1E5, 5E5))

  std_mortality_2 <-
    healthiar::standardize(
      output_attribute = mortality_2,
      age_group = c("below_40", "above_40"),
      ref_prop_pop = c(0.5, 0.5))

  comparison <- healthiar::compare(std_mortality_1, std_mortality_2)

  testthat::expect_equal(
    object =
      comparison$health_main$impact,

    expected =  80.9237143
    # No study behind.
    # Fake numbers to check consistency of result overtime. Results on 2026-01-16
  )

})




# ERROR OR WARNING ########
## ERROR #########


## NOT SUMMED #################################################################

testthat::test_that("results the same |pathway_standardize|main_results_by|two_subgroups_in_one_call|", {

  # Two exposure-outcome pairs kept apart with main_results_by must give the same
  # age-standardized impacts as two separate assessments, i.e. standardize()
  # must be applied once per subgroup instead of pooling them
  in_one_call <-
    healthiar::attribute_health(
      info = base::data.frame(pair = base::rep(c("copd", "asthma"), each = 2)),
      main_results_by = "pair",
      age_group = base::rep(c("below_40", "above_40"), times = 2),
      exp_central = c(8.1, 10.9, 22.1, 24.5),
      cutoff_central = 0,
      bhd_central = c(1000, 4000, 800, 3000),
      rr_central = c(1.063, 1.063, 1.041, 1.041),
      rr_increment = 10,
      erf_shape = "log_linear",
      population = c(1E5, 5E5, 1E5, 5E5)) |>
    healthiar::standardize(
      age_group = c("below_40", "above_40"),
      ref_prop_pop = c(0.5, 0.5))

  in_separate_calls <-
    purrr::map_dbl(
      .x = 1:2,
      .f = ~ healthiar::attribute_health(
        age_group = c("below_40", "above_40"),
        exp_central = base::list(c(8.1, 10.9), c(22.1, 24.5))[[.x]],
        cutoff_central = 0,
        bhd_central = base::list(c(1000, 4000), c(800, 3000))[[.x]],
        rr_central = c(1.063, 1.041)[.x],
        rr_increment = 10,
        erf_shape = "log_linear",
        population = c(1E5, 5E5)) |>
        healthiar::standardize(
          age_group = c("below_40", "above_40"),
          ref_prop_pop = c(0.5, 0.5)) |>
        purrr::pluck("health_main", "impact_per_100k_inhab"))

  testthat::expect_equal(
    object = in_one_call$health_main$impact_per_100k_inhab,
    expected = in_separate_calls)
})
