# QUANTITATIVE TEST ############################################################
## YLL from lifetable ###########################################################

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
      exp_central = 8.85, # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100),
      age_group = base::rep(0:99, times = 2),
      bhd_central = c(data[["pop"]]$number_of_deaths_male,
                      data[["pop"]]$number_of_deaths_female),
      population = c(data_lifetable[["male"]]$population,
                     data_lifetable[["female"]]$population),
      year_of_analysis = 2019,
      min_age = 20)

  testthat::expect_equal(
    object =
      healthiar::daly(
        output_attribute_yll = bestcost_pm_yll,
        output_attribute_yld = bestcost_pm_yld
        )$health_main$impact_rounded,
    expected =
      c(32413, 16944, 48915) # Result from 2025-04-04; no comparison study
  )
})

testthat::test_that("results correct using 2 delta comparisons as inputs|pathway_daly|yll_from_lifetable_TRUE|output_1_type_compare|output_2_type_compare|", {

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
      exp_central = 8.85, # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100),
      age_group = base::rep(0:99, times = 2),
      bhd_central = c(data[["pop"]]$number_of_deaths_male,
                      data[["pop"]]$number_of_deaths_female),
      population = c(data_lifetable[["male"]]$population,
                     data_lifetable[["female"]]$population),
      year_of_analysis = 2019,
      min_age = 20)

  ## Define scenarios
  scen_1_yll <-
    bestcost_pm_yll

  scen_2_yll <-
    healthiar::attribute_mod(
      output_attribute = bestcost_pm_yll,
      exp_central = 6)

  scen_1_yld <-
    bestcost_pm_yld

  scen_2_yld <-
    healthiar::attribute_mod(
      output_attribute = bestcost_pm_yld,
      exp_central = 6)

  ## Delta comparison
  testthat::expect_equal(
    object =
      healthiar::daly(
        output_attribute_yll =
          healthiar::compare(
            approach_comparison = "delta",
            output_attribute_scen_1 = scen_1_yll,
            output_attribute_scen_2 = scen_2_yll),
        output_attribute_yld =
          healthiar::compare(
            approach_comparison = "delta",
            output_attribute_scen_1 = scen_1_yld,
            output_attribute_scen_2 = scen_2_yld)
      )$health_main$impact_rounded,
    expected =
      c(23956, 12533, 36112) # Result on 7 July 2025; no comparison study
  )
})

testthat::test_that("results correct using 2 pif comparisons as inputs |pathway_daly|yll_from_lifetable_TRUE|output_1_type_compare|output_2_type_compare|", {

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
      exp_central = 8.85, # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100),
      age_group = base::rep(0:99, times = 2),
      bhd_central = c(data[["pop"]]$number_of_deaths_male,
                      data[["pop"]]$number_of_deaths_female),
      population = c(data_lifetable[["male"]]$population,
                     data_lifetable[["female"]]$population),
      year_of_analysis = 2019,
      min_age = 20)

  ## Define scenarios
  scen_1_yll <-
    bestcost_pm_yll

  scen_2_yll <-
    healthiar::attribute_mod(
      output_attribute = bestcost_pm_yll,
      exp_central = 6)

  scen_1_yld <-
    bestcost_pm_yld

  scen_2_yld <-
    healthiar::attribute_mod(
      output_attribute = bestcost_pm_yld,
      exp_central = 6)

  ## PIF comparison
  testthat::expect_equal(
    object =
      healthiar::daly(
        output_attribute_yll =
          healthiar::compare(
            approach_comparison = "pif",
            output_attribute_scen_1 = scen_1_yll,
            output_attribute_scen_2 = scen_2_yll),
        output_attribute_yld =
          healthiar::compare(
            approach_comparison = "pif",
            output_attribute_scen_1 = scen_1_yld,
            output_attribute_scen_2 = scen_2_yld)
        )$health_main$impact_rounded,
    expected =
      c(24032, 12554, 36308) # Result on 7 July 2025; no comparison study
  )

})

testthat::test_that("results correct using 2 delta iteration comparisons as inputs |pathway_daly|yll_from_lifetable_TRUE|output_1_type_compare|output_2_type_compare|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_yld_geo <-
    healthiar::attribute_health(
      exp_central = c(8.5, 8),
      prop_pop_exp = 1,
      cutoff_central = 5,
      bhd_central = c(1E3, 1E3),
      rr_central = 1.1,
      rr_lower = 1.05,
      rr_upper = 1.19,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = "pm2.5_yld",
      duration_central = 100,
      population = rep(sum(data_lifetable[["male"]]$population,
                       data_lifetable[["female"]]$population),
                       2),
      dw_central = 1,
      geo_id_micro = c("a", "b"),
      geo_id_macro = c("ch", "ch"))

  scen_2_yld_geo <-
    healthiar::attribute_mod(
      output_attribute = scen_1_yld_geo,
      exp_central = c(6, 6.5))


  scen_1_yll_geo <-
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      exp_central = rep(c(8.5, 8.0), each = 2 * 100) , # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100, times = 2),
      age_group = base::rep(0:99, times = 2*2),
      bhd_central = base::rep(
        c(data[["pop"]]$number_of_deaths_male,
          data[["pop"]]$number_of_deaths_female),
        times = 2),
      population = base::rep(
        c(data_lifetable[["male"]]$population,
          data_lifetable[["female"]]$population),
        times = 2),
      year_of_analysis = 2019,
      min_age = 20,
      geo_id_micro = rep(c("a", "b"), each = 2* 100),
      geo_id_macro = rep("ch", each = 2 * 2 * 100))

  scen_2_yll_geo <-
    healthiar::attribute_mod(
      output_attribute = scen_1_yll_geo,
      exp_central = rep(c(6, 6.5), each = 2 * 100))

  ## Delta comparison
  testthat::expect_equal(
    object =
      healthiar::daly(
        output_attribute_yll =
          healthiar::compare(
            approach_comparison = "delta",
            output_attribute_scen_1 = scen_1_yll_geo,
            output_attribute_scen_2 = scen_2_yll_geo),
        output_attribute_yld =
          healthiar::compare(
            approach_comparison = "delta",
            output_attribute_scen_1 = scen_1_yld_geo,
            output_attribute_scen_2 = scen_2_yld_geo)
      )$health_main$impact_rounded,
    expected =
      c(33641, 17595, 50731) # Result on 7 July 2025; no comparison study
  )

})

testthat::test_that("results correct using 2 pif iteration comparisons as inputs |pathway_daly|yll_from_lifetable_TRUE|output_1_type_compare|output_2_type_compare|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  scen_1_yld_geo <-
    healthiar::attribute_health(
      exp_central = c(8.5, 8),
      prop_pop_exp = 1,
      cutoff_central = 5,
      bhd_central = c(1E3, 1E3),
      rr_central = 1.1,
      rr_lower = 1.05,
      rr_upper = 1.19,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = "pm2.5_yld",
      duration_central = 100,
      population = rep(sum(data_lifetable[["male"]]$population,
                           data_lifetable[["female"]]$population),
                       2),
      dw_central = 1,
      geo_id_micro = c("a", "b"),
      geo_id_macro = c("ch", "ch"))

  scen_2_yld_geo <-
    healthiar::attribute_mod(
      output_attribute = scen_1_yld_geo,
      exp_central = c(6, 6.5))


  scen_1_yll_geo <-
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      exp_central = rep(c(8.5, 8.0), each = 2 * 100) , # Fake data just for testing purposes
      prop_pop_exp = 1, # Fake data just for testing purposes
      cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
      rr_central = 1.118,
      rr_lower = 1.060,
      rr_upper = 1.179,
      rr_increment = 10,
      erf_shape = "log_linear",
      approach_exposure = "single_year",
      approach_newborns = "without_newborns",
      sex = base::rep(c("male", "female"), each = 100, times = 2),
      age_group = base::rep(0:99, times = 2*2),
      bhd_central = base::rep(
        c(data[["pop"]]$number_of_deaths_male,
          data[["pop"]]$number_of_deaths_female),
        times = 2),
      population = base::rep(
        c(data_lifetable[["male"]]$population,
          data_lifetable[["female"]]$population),
        times = 2),
      year_of_analysis = 2019,
      min_age = 20,
      geo_id_micro = rep(c("a", "b"), each = 2* 100),
      geo_id_macro = rep("ch", each = 2 * 2 * 100))

  scen_2_yll_geo <-
    healthiar::attribute_mod(
      output_attribute = scen_1_yll_geo,
      exp_central = rep(c(6, 6.5), each = 100 * 2))


  ## PIF comparison
  testthat::expect_equal(
    object =
      healthiar::daly(
        output_attribute_yll =
          healthiar::compare(
            approach_comparison = "pif",
            output_attribute_scen_1 = scen_1_yll_geo,
            output_attribute_scen_2 = scen_2_yll_geo),
        output_attribute_yld =
          healthiar::compare(
            approach_comparison = "pif",
            output_attribute_scen_1 = scen_1_yld_geo,
            output_attribute_scen_2 = scen_2_yld_geo)
        )$health_main$impact_rounded,
    expected =
      c(33769, 17630, 51058) # Result on 7 July 2025; no comparison study
  )

})

# ERROR OR WARNING ########
## ERROR #########

## WARNING #########
