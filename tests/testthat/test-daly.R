# QUANTITATIVE TEST ###########################################################
## YLL from lifetable #########################################################

testthat::test_that("results the same |pathway_daly|yll_from_lifetable_TRUE|output_1_type_attribute|output_2_type_attribute|", {

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("testdata", "lifetable_with_population.rds"))

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

testthat::test_that("results the same using 2 comparisons as inputs|pathway_daly|yll_from_lifetable_TRUE|output_1_type_compare|output_2_type_compare|", {

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("testdata", "lifetable_with_population.rds"))

  bestcost_yld_scen_1  <-
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

  bestcost_yll_scen_1 <-
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
  bestcost_yld_scen_2 <-
    healthiar::attribute_mod(
      output_attribute =  bestcost_yld_scen_1,
      exp_central = 6)

  bestcost_yll_scen_2 <-
    healthiar::attribute_mod(
      output_attribute =  bestcost_yll_scen_1,
      exp_central = 6)


  comparison_yld_pif <- healthiar::compare(
    bestcost_yld_scen_1,
    bestcost_yld_scen_2,
    approach_comparison = "pif"
  )

  comparison_yll_pif <- healthiar::compare(
    bestcost_yll_scen_1,
    bestcost_yll_scen_2,
    approach_comparison = "pif"
  )

  comparison_yld_delta <- healthiar::compare(
    bestcost_yld_scen_1,
    bestcost_yld_scen_2,
    approach_comparison = "delta"
  )

  comparison_yll_delta <- healthiar::compare(
    bestcost_yll_scen_1,
    bestcost_yll_scen_2,
    approach_comparison = "delta"
  )


  ## PIF comparison
  testthat::expect_equal(
    object =
      healthiar::daly(
        output_attribute_yll = comparison_yll_pif,
        output_attribute_yld = comparison_yld_pif)$health_main$impact_rounded,
    expected =
      c(24032, 12554, 36308) # Result on 7 July 2025; no comparison study
  )

  ## Delta comparison
  testthat::expect_equal(
    object =
      healthiar::daly(
        output_attribute_yll = comparison_yll_delta,
        output_attribute_yld = comparison_yld_delta)$health_main$impact_rounded,
    expected =
      c(23956, 12533, 36112) # Result on 7 July 2025; no comparison study
  )
})


testthat::test_that("results the same using 2 pif comparisons as inputs |pathway_daly|yll_from_lifetable_TRUE|output_1_type_compare|output_2_type_compare|", {

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("testdata", "lifetable_with_population.rds"))

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

### ITERATION #################################################################

testthat::test_that("results the same using 2 delta iteration comparisons as inputs |pathway_daly|yll_from_lifetable_TRUE|output_1_type_compare|output_2_type_compare|", {

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("testdata", "lifetable_with_population.rds"))

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

testthat::test_that("results the same using 2 pif iteration comparisons as inputs |pathway_daly|yll_from_lifetable_TRUE|output_1_type_compare|output_2_type_compare|", {

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("testdata", "lifetable_with_population.rds"))

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

## IMPACT RATE ################################################################

testthat::test_that("results the same |pathway_daly|impact_per_100k_inhab|", {

  # The impact rate must be the DALY rate of the geo unit, i.e. impact divided
  # by population. Before, daly() called it impact_per_100k, which get_output()
  # does not recognise as a rate: it was therefore added up like an impact (the
  # rates of the geo units were summed) while the column that does carry the
  # expected name kept the rate of the years of life lost only
  attribute_one_outcome <- function(dw = NULL, duration = NULL, population = NULL){
    healthiar::attribute_health(
      exp_central = 10,
      cutoff_central = 5,
      bhd_central = c(1000, 1200),
      geo_id_micro = c("a", "b"),
      geo_id_macro = c("ch", "ch"),
      population = population,
      rr_central = 1.05,
      rr_increment = 10,
      erf_shape = "log_linear",
      dw_central = dw,
      duration_central = duration)
  }

  # Population entered in both assessments, which is the usual case. The
  # full_join() in daly() renames it to population_yll and population_yld, so
  # without restoring it there was no impact rate in the results at all
  daly_population_in_both <-
    healthiar::daly(
      output_attribute_yll = attribute_one_outcome(population = c(1E6, 1E6)),
      output_attribute_yld = attribute_one_outcome(dw = 0.1, duration = 2,
                                                   population = c(1E6, 1E6)))

  results_by_geo_id_macro <-
    daly_population_in_both$health_detailed$results_by_geo_id_macro

  testthat::expect_equal(
    object = results_by_geo_id_macro$impact_per_100k_inhab,
    expected =
      (results_by_geo_id_macro$impact / results_by_geo_id_macro$population) * 1E5)

  # Population only in the assessment of the years of life lost, as in the
  # examples of daly(). The rate of that assessment must not survive under the
  # name of the DALY rate
  daly_population_in_yll <-
    healthiar::daly(
      output_attribute_yll = attribute_one_outcome(population = c(1E6, 1E6)),
      output_attribute_yld = attribute_one_outcome(dw = 0.1, duration = 2))

  results_raw <- daly_population_in_yll$health_detailed$results_raw

  testthat::expect_equal(
    object = results_raw$impact_per_100k_inhab,
    expected = (results_raw$impact / results_raw$population) * 1E5)
})

# ERROR OR WARNING ########
## ERROR #########

testthat::test_that("error if the two assessments of daly() are not comparable", {

  # The two assessments must refer to the same population and exposure,
  # because their results are joined by those columns. The check compared the
  # assessment of the years lived with disability with itself, and on top of
  # that it looked up column names of the results tables in the output list,
  # where they do not exist. It was therefore always TRUE and never fired
  attribute_one_outcome <- function(exp_central, ...){
    base::do.call(
      healthiar::attribute_health,
      c(base::list(exp_central = exp_central,
                   cutoff_central = 5,
                   bhd_central = 1000,
                   geo_id_micro = "a",
                   erf_shape = "log_linear",
                   rr_central = 1.05,
                   rr_increment = 10),
        base::list(...)))
  }

  # Same exposure in both: the assessments are comparable
  testthat::expect_no_error(
    healthiar::daly(
      output_attribute_yll = attribute_one_outcome(10),
      output_attribute_yld = attribute_one_outcome(10, dw_central = 0.1,
                                                   duration_central = 2)))

  # Different exposure: the message must name the column that differs (exp)
  testthat::expect_error(
    object =
      healthiar::daly(
        output_attribute_yll = attribute_one_outcome(10),
        output_attribute_yld = attribute_one_outcome(12, dw_central = 0.1,
                                                     duration_central = 2)),
    regexp = "must be identical in the assessment of years of life lost")
})

## WARNING #########
