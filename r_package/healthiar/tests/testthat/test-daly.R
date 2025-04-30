# YLL from lifetable ###########################################################

testthat::test_that("results correct |pathway_daly|yll_from_lifetable_TRUE|output_1_type_attribute|output_2_type_attribute|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  bestcost_pm_yld  <-
    healthiar::attribute_health(
      exp_central = 8.85,
      prop_pop_exp = 1,
      cutoff_central = 5,
      bhd_central = 1E3,
      rr_central = 1.1,
      rr_lower = 1.05,
      rr_upper = 1.19,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = "pm2.5_yld",
      duration_central = 100,
      population = sum(data_lifetable[["male"]]$population,
                       data_lifetable[["female"]]$population),
      dw_central = 1)

  bestcost_pm_yll <-
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      approach_exposure = "single_year",
      exp_central = 8.85,
      prop_pop_exp = 1,
      cutoff_central = 5,
      rr_central =  1.118,
      rr_lower = 1.06,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      first_age_pop = 0,
      last_age_pop = 99,
      deaths_male = data[["pop"]]$number_of_deaths_male,
      deaths_female = data[["pop"]]$number_of_deaths_female,
      population_midyear_male = data_lifetable[["male"]]$population,
      population_midyear_female = data_lifetable[["female"]]$population,
      year_of_analysis = 2019,
      min_age = 20,
      info = "pm2.5_yll")

  testthat::expect_equal(
    object =
      healthiar::daly(
        output_attribute_yll = bestcost_pm_yll,
        output_attribute_yld = bestcost_pm_yld
        )$health_main$impact_rounded,
    expected =
      c(32878, 17189, 49596) # Result from 2025-04-04; no comparison study
  )
})

testthat::test_that("results correct daly from two compare outputs (once 2 delta comparisons and once 2 pif comparisons)", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  bestcost_pm_yld  <-
    healthiar::attribute_health(
      exp_central = 8.85,
      prop_pop_exp = 1,
      cutoff_central = 5,
      bhd_central = 1E3,
      rr_central = 1.1,
      rr_lower = 1.05,
      rr_upper = 1.19,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = "pm2.5_yld",
      duration_central = 100,
      population = sum(data_lifetable[["male"]]$population,
                       data_lifetable[["female"]]$population),
      dw_central = 1)

  bestcost_pm_yll <-
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      approach_exposure = "single_year",
      exp_central = 8.85,
      prop_pop_exp = 1,
      cutoff_central = 5,
      rr_central =  1.118,
      rr_lower = 1.06,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      first_age_pop = 0,
      last_age_pop = 99,
      deaths_male = data[["pop"]]$number_of_deaths_male,
      deaths_female = data[["pop"]]$number_of_deaths_female,
      population_midyear_male = data_lifetable[["male"]]$population,
      population_midyear_female = data_lifetable[["female"]]$population,
      year_of_analysis = 2019,
      min_age = 20,
      info = "pm2.5_yll")

  ## Define scenarios
  scen_1_yll <-
    bestcost_pm_yll

  scen_2_yll <-
    healthiar::attribute_mod(
      output_attribute_1 = bestcost_pm_yll,
      exp_central = 6)

  scen_1_yld <-
    bestcost_pm_yld

  scen_2_yld <-
    healthiar::attribute_mod(
      output_attribute_1 = bestcost_pm_yld,
      exp_central = 6)

  ## Delta comparison
  testthat::expect_equal(
    object =
      healthiar::daly(
        output_attribute_yll =
          healthiar::compare(
            approach_comparison = "delta",
            output_attribute_1 = scen_1_yll,
            output_attribute_2 = scen_2_yll),
        output_attribute_yld =
          healthiar::compare(
            approach_comparison = "delta",
            output_attribute_1 = scen_1_yld,
            output_attribute_2 = scen_2_yld)
        )$health_main$impact_rounded,
    expected =
      c(24299, 12714, 36613) # Result on 21 March 2025; no comparison study
  )

  ## PIF comparison
  testthat::expect_equal(
    object =
      healthiar::daly(
        output_attribute_yll =
          healthiar::compare(
            approach_comparison = "pif",
            output_attribute_1 = scen_1_yll,
            output_attribute_2 = scen_2_yll),
        output_attribute_yld =
          healthiar::compare(
            approach_comparison = "pif",
            output_attribute_1 = scen_1_yld,
            output_attribute_2 = scen_2_yld)
        )$health_main$impact_rounded,
    expected =
      c(24378, 12735, 36815) # Result on 21 March 2025; no comparison study
  )

})

testthat::test_that("results correct daly from two compare outputs (once 2 delta comparisons and once 2 pif comparisons)", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  bestcost_pm_yld  <-
    healthiar::attribute_health(
      exp_central = 8.85,
      prop_pop_exp = 1,
      cutoff_central = 5,
      bhd_central = 1E3,
      rr_central = 1.1,
      rr_lower = 1.05,
      rr_upper = 1.19,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = "pm2.5_yld",
      duration_central = 100,
      population = sum(data_lifetable[["male"]]$population,
                       data_lifetable[["female"]]$population),
      dw_central = 1)

  bestcost_pm_yll <-
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      approach_exposure = "single_year",
      exp_central = 8.85,
      prop_pop_exp = 1,
      cutoff_central = 5,
      rr_central =  1.118,
      rr_lower = 1.06,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      first_age_pop = 0,
      last_age_pop = 99,
      deaths_male = data[["pop"]]$number_of_deaths_male,
      deaths_female = data[["pop"]]$number_of_deaths_female,
      population_midyear_male = data_lifetable[["male"]]$population,
      population_midyear_female = data_lifetable[["female"]]$population,
      year_of_analysis = 2019,
      min_age = 20,
      info = "pm2.5_yll")

  ## Define scenarios
  scen_1_yll_geo <-
    healthiar::attribute_mod(
      output_attribute_1 = bestcost_pm_yll,
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = c("ch", "ch"),
      exp_central = list(8.5, 8))

  scen_2_yll_geo <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_yll_geo,
      exp_central = list(6, 6.5))

  scen_1_yld_geo <-
    healthiar::attribute_mod(
      output_attribute_1 = bestcost_pm_yld,
      exp_central = list(8.5, 8),
      geo_id_disaggregated = c("a", "b"),
      geo_id_aggregated = c("ch", "ch"))

  scen_2_yld_geo <-
    healthiar::attribute_mod(
      output_attribute_1 = scen_1_yld_geo,
      exp_central = list(6, 6.5))

  ## Delta comparison
  testthat::expect_equal(
    object =
      healthiar::daly(
        output_attribute_yll =
          healthiar::compare(
            approach_comparison = "delta",
            output_attribute_1 = scen_1_yll_geo,
            output_attribute_2 = scen_2_yll_geo),
        output_attribute_yld =
          healthiar::compare(
            approach_comparison = "delta",
            output_attribute_1 = scen_1_yld_geo,
            output_attribute_2 = scen_2_yld_geo)
        )$health_main$impact_rounded,
    expected =
      c(34123, 17849, 51437) # Result on 21 March 2025; no comparison study
  )

  ## PIF comparison
  testthat::expect_equal(
    object =
      healthiar::daly(
        output_attribute_yll =
          healthiar::compare(
            approach_comparison = "pif",
            output_attribute_1 = scen_1_yll_geo,
            output_attribute_2 = scen_2_yll_geo),
        output_attribute_yld =
          healthiar::compare(
            approach_comparison = "pif",
            output_attribute_1 = scen_1_yld_geo,
            output_attribute_2 = scen_2_yld_geo)
        )$health_main$impact_rounded,
    expected =
      c(34255, 17885, 51773) # Result on 21 March 2025; no comparison study
  )

})
