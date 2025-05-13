# QUANTITATIVE TEST ############################################################
## USING IMPACT FROM OUTPUT OF ATTRIBUTE ############################################################

### ONE GEO UNIT ############################################################

testthat::test_that("results correct |pathway_standardize|single_geo|", {

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
      output_attribute_1 = bestcost_pm_mortality_below_40,
      bhd_central = 4000,
      exp_central = 10.9,
      population = 5E5)

  population_below_40 <- bestcost_pm_mortality_below_40$health_main$population
  population_40_plus <- bestcost_pm_mortality_40_plus$health_main$population
  population_total <- population_below_40 + population_40_plus

  testthat::expect_equal(
    object =
      healthiar::standardize(
        output_healthiar = list(bestcost_pm_mortality_below_40,
                                bestcost_pm_mortality_40_plus),
        age_groups = c("below_40", "40_plus"))$health_main$impact_per_100k_inhab,

    expected =  base::sum(
      bestcost_pm_mortality_below_40$health_main$impact_per_100k_inhab * population_below_40 / population_total,
      bestcost_pm_mortality_40_plus$health_main$impact_per_100k_inhab * population_40_plus / population_total
    )
      # No study behind.
      # Fake numbers to check consistency of result overtime. Results on 2025-01-16
  )
})

## USING IMPACT FROM USER ############################################################

# ERROR OR WARNING ########
## ERROR #########
