# QUANTITATIVE TEST ############################################################
## USING ATTRIBUTE ############################################################

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
        listed_output_healthiar = list(bestcost_pm_mortality_below_40,
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

### MULTIPLE GEO UNITS ############################################################
testthat::test_that("results correct |pathway_standardize|multi_geo|", {

  bestcost_pm_mortality_below_40_multigeo <-
    healthiar::attribute_health(
      exp_central = list(8.1, 7.1),
      cutoff_central =  0,
      bhd_central = list(1000, 2000),
      rr_central = 1.063,
      rr_increment = 10,
      erf_shape = "log_linear",
      population = list(1E5, 2E5),
      geo_id_disaggregated = c("a", "b"))

  bestcost_pm_mortality_40_plus_multigeo <-
    healthiar::attribute_mod(
      output_attribute_1 = bestcost_pm_mortality_below_40_multigeo,
      bhd_central = list(4000, 8000),
      exp_central = list(10.9, 9.9),
      population = list(5E5, 1E6))

  population_below_40_multigeo <- bestcost_pm_mortality_below_40_multigeo$health_main$population
  population_40_plus_multigeo <- bestcost_pm_mortality_40_plus_multigeo$health_main$population
  population_total_multigeo <- population_below_40_multigeo + population_40_plus_multigeo

  testthat::expect_equal(
    object =
      healthiar::standardize(
        listed_output_healthiar = list(bestcost_pm_mortality_below_40_multigeo,
                                bestcost_pm_mortality_40_plus_multigeo),
        age_groups = c("below_40", "40_plus"))$health_main$impact_per_100k_inhab,

    expected =
      bestcost_pm_mortality_below_40_multigeo$health_main$impact_per_100k_inhab * population_below_40_multigeo / population_total_multigeo +
      bestcost_pm_mortality_40_plus_multigeo$health_main$impact_per_100k_inhab * population_40_plus_multigeo / population_total_multigeo
    # No study behind.
    # Fake numbers to check consistency of result overtime. Results on 2025-01-16
  )
})


### ONE GEO UNIT ############################################################
#
# testthat::test_that("results correct |pathway_standardize|single_geo|", {
#
#   bestcost_pm_mortality_below_40_before <-
#     healthiar::attribute_health(
#       exp_central = 8.1,
#       cutoff_central =  0,
#       bhd_central = 1000,
#       rr_central = 1.063,
#       rr_increment = 10,
#       erf_shape = "log_linear",
#       population = 1E5)
#
#   bestcost_pm_mortality_40_plus_before <-
#     healthiar::attribute_mod(
#       output_attribute_1 = bestcost_pm_mortality_below_40_before,
#       bhd_central = 4000,
#       exp_central = 10.9,
#       population = 5E5)
#
#
#   bestcost_pm_mortality_below_40_after <-
#     healthiar::attribute_mod(
#       output_attribute_1 = bestcost_pm_mortality_below_40_before,
#       exp_central = 7.1,
#       bhd_central = 2000,
#       population = 2E5)
#
#   bestcost_pm_mortality_40_plus_after <-
#     healthiar::attribute_mod(
#       output_attribute_1 = bestcost_pm_mortality_40_plus_before,
#       bhd_central = 8000,
#       exp_central = 9.9,
#       population = 1E6)
#
#   bestcost_pm_mortality_below_40_compared <-
#     healthiar::compare(
#       output_attribute_1 =
#         healthiar::standardize(
#           list(bestcost_pm_mortality_below_40_before,
#                bestcost_pm_mortality_40_plus_before),
#           age_groups = c("below_40", "40_plus")),
#       output_attribute_2 =
#         healthiar::standardize(
#           list(bestcost_pm_mortality_below_40_after,
#                bestcost_pm_mortality_40_plus_after),
#           age_groups = c("below_40", "40_plus")
#         )
#       )
#
#
#   testthat::expect_equal(
#     object =
#       bestcost_pm_mortality_below_40_compared$health_main$impact_per_100k_inhab,
#
#     expected =  (46.20257 - 50.99689)    )
#     # No study behind.
#     # Fake numbers to check consistency of result overtime. Results on 2025-01-16
#
# })


## USING COMPARE ############################################################

# ERROR OR WARNING ########
## ERROR #########
