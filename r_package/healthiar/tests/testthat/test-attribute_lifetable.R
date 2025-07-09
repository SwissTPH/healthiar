# QUANTITATIVE TEST ############################################################

## YLL from lifetable ###########################################################

## SINGLE YEAR EXPOSURE & NO NEWBORNS ##########################################
testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        approach_exposure = "single_year",
        exp_central = data_mort$exp[2], #exp CH 2019
        prop_pop_exp = 1,
        cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
        rr_central = data_mort[2,"rr_central"],
        rr_lower = data_mort[2,"rr_lower"],
        rr_upper = data_mort[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        age_group = c(data_lifetable[["male"]]$age,
                      data_lifetable[["female"]]$age),
        sex = base::rep(c("male", "female"), each = 100),
        population = c(data_lifetable[["male"]]$population,
                       data_lifetable[["female"]]$population),
        bhd_central = c(data[["pop"]]$number_of_deaths_male,
                        data[["pop"]]$number_of_deaths_female),
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
      )$health_main$impact,
    expected =
      c(29274.89, 15328.16,	43118.30), # AirQ+ results from "Lifetable_CH_2019_PM_single_year_AP_no_newborns_default.csv"
    tolerance = 0.49 # I.e. less than 1 YLL
  )
})

# Deactivated: Old code before age_group and sex
# testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {
#
#   data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
#   data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
#   data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))
#
#   testthat::expect_equal(
#     object =
#       healthiar::attribute_lifetable(
#         health_outcome = "yll",
#         approach_exposure = "single_year",
#         exp_central = data_mort$exp[2], #exp CH 2019
#         prop_pop_exp = 1,
#         cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
#         rr_central = data_mort[2,"rr_central"],
#         rr_lower = data_mort[2,"rr_lower"],
#         rr_upper =data_mort[2,"rr_upper"],
#         rr_increment = 10,
#         erf_shape = "log_linear",
#         first_age_pop = 0,
#         last_age_pop = 99,
#         population_midyear_male = data_lifetable[["male"]]$population,
#         population_midyear_female = data_lifetable[["female"]]$population,
#         deaths_male = data[["pop"]]$number_of_deaths_male,
#         deaths_female = data[["pop"]]$number_of_deaths_female,
#         year_of_analysis = 2019,
#         info = data_mort$pollutant[2],
#         min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
#         )$health_main$impact,
#     expected =
#       c(29274.89, 15328.16,	43118.30), # AirQ+ results from "Lifetable_CH_2019_PM_single_year_AP_no_newborns_default.csv"
#     tolerance = 0.49 # I.e. less than 1 YLL
#   )
# })

testthat::test_that("results the same |fake_lifetable|exp_single|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        exp_central = base::rep(c(8, 9, 10), each = 100*2), # Fake data just for testing purposes
        prop_pop_exp = base::rep(c(0.2, 0.3, 0.5), each = 100*2), # Fake data just for testing purposes
        cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
        rr_central = data_mort[2,"rr_central"],
        rr_lower = data_mort[2,"rr_lower"],
        rr_upper = data_mort[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        age_group = base::rep(
          c(data_lifetable[["male"]]$age,
            data_lifetable[["female"]]$age),
          times = 3),
        sex = base::rep(
          c("male", "female"),
          each = 100,
          times = 3),
        population = base::rep(
          c(data_lifetable[["male"]]$population,
            data_lifetable[["female"]]$population),
          times = 3),
        bhd_central = base::rep(
          c(data[["pop"]]$number_of_deaths_male,
            data[["pop"]]$number_of_deaths_female),
          times = 3),
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
        )$health_main$impact_rounded,
    expected =
      c(32185, 16849, 47413) # Result on 09 July 2025 (AirQ+ approach); no comparison study to
  )
})


# Deactivated: Old code before age_group and sex
# testthat::test_that("results the same |fake_lifetable|exp_single|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {
#
#   data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
#   data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
#   data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))
#
#   testthat::expect_equal(
#     object =
#       healthiar::attribute_lifetable(
#         health_outcome = "yll",
#         exp_central = c(8, 9, 10), # Fake data just for testing purposes
#         prop_pop_exp = c(0.2, 0.3, 0.5), # Fake data just for testing purposes
#         cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
#         rr_central = data_mort[2,"rr_central"],
#         rr_lower = data_mort[2,"rr_lower"],
#         rr_upper = data_mort[2,"rr_upper"],
#         rr_increment = 10,
#         erf_shape = "log_linear",
#         first_age_pop = 0,
#         last_age_pop = 99,
#         deaths_male = data[["pop"]]$number_of_deaths_male,
#         deaths_female = data[["pop"]]$number_of_deaths_female,
#         population_midyear_male = data_lifetable[["male"]]$population,
#         population_midyear_female = data_lifetable[["female"]]$population,
#         year_of_analysis = 2019,
#         info = data_mort$pollutant[2],
#         min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
#       )$health_main$impact_rounded,
#     expected =
#       round(c(32704.07, 17121.94, 48173.38)) # Result on 20 August 2024 (AirQ+ approach); no comparison study to
#   )
# })

### CONSTANT EXPOSURE & NO NEWBORNS #############################################
testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_constant|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        approach_exposure = "constant",
        approach_newborns = "without_newborns",
        exp_central = data[["input"]]$mean_concentration,
        cutoff_central = data[["input"]]$cut_off_value,
        rr_central = data[["input"]]$relative_risk,
        rr_lower = data[["input"]]$relative_risk_lower,
        rr_upper = data[["input"]]$relative_risk_upper,
        rr_increment = 10,
        erf_shape = base::gsub("-", "_", data[["input"]]$calculation_method),
        age_group = base::rep(data[["pop"]][["age_from..."]], times = 2),
        sex = base::rep(c("male", "female"), each = 100),
        population = c(data[["pop"]]$midyear_population_male,
                       data[["pop"]]$midyear_population_female),
        bhd_central = c(data[["pop"]]$number_of_deaths_male,
                        data[["pop"]]$number_of_deaths_female),
        year_of_analysis =  data[["input"]]$start_year,
        min_age = data[["input"]]$apply_rr_from_age
      )$health_main$impact,
    expected =
      c(2738323.2, 1432078.6, 4037910.4)

  )
})


# testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_constant|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {
#
#   data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
#
#   testthat::expect_equal(
#     object =
#       healthiar::attribute_lifetable(
#         health_outcome = "yll",
#         approach_exposure = "constant",
#         approach_newborns = "without_newborns",
#         exp_central = data[["input"]]$mean_concentration,
#         cutoff_central = data[["input"]]$cut_off_value,
#         rr_central = data[["input"]]$relative_risk,
#         rr_lower = data[["input"]]$relative_risk_lower,
#         rr_upper = data[["input"]]$relative_risk_upper,
#         rr_increment = 10,
#         erf_shape = base::gsub("-", "_", data[["input"]]$calculation_method),
#         first_age_pop = dplyr::first(data[["pop"]]$age_from...),
#         last_age_pop = dplyr::last(data[["pop"]]$age_from...),
#         population_midyear_male = data[["pop"]]$midyear_population_male,
#         population_midyear_female = data[["pop"]]$midyear_population_female,
#         deaths_male = data[["pop"]]$number_of_deaths_male,
#         deaths_female = data[["pop"]]$number_of_deaths_female,
#         year_of_analysis =  data[["input"]]$start_year,
#         min_age = data[["input"]]$apply_rr_from_age
#       )$health_main$impact,
#   expected =
#       # c(2776839.17,	1452410.83,	4094163.08) # AirQ+ results from "Lifetable_CH_2019_PM_constant_AP_no_newborns_default.csv" when female deaths for age 8 was still 0 (not OK, should be 1 or more)
#     c(2776836.1, 1452409.2, 4094158.6) # ~ AirQ+ results but slightly adjusted on 2025-07-02, because the female deaths for age 8 was set at 1 in "airqplus_pm_deaths_yll.rds" data set, which is used for this example  (before it was 0, which is not OK, because the calculated survival prob is then 1 which is not realistic)
#   )
# })

### CONSTANT EXPOSURE & WITH NEWBORNS ###########################################

testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_constant|newborns_TRUE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  testthat::expect_equal(
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      approach_exposure = "constant",
      approach_newborns = "with_newborns",
      exp_central = data[["input"]]$mean_concentration,
      cutoff_central = data[["input"]]$cut_off_value,
      rr_central = data[["input"]]$relative_risk,
      rr_lower = data[["input"]]$relative_risk_lower,
      rr_upper = data[["input"]]$relative_risk_upper,
      rr_increment = 10,
      erf_shape = base::gsub("-", "_", data[["input"]]$calculation_method),
      age_group = base::rep(data[["pop"]][["age_from..."]], times = 2),
      sex = base::rep(c("male", "female"), each = 100),
      population = c(data[["pop"]]$midyear_population_male,
                     data[["pop"]]$midyear_population_female),
      bhd_central = c(data[["pop"]]$number_of_deaths_male,
                      data[["pop"]]$number_of_deaths_female),
      year_of_analysis =  data[["input"]]$start_year,
      min_age = data[["input"]]$apply_rr_from_age
    )$health_main$impact,
    expected =
      # c(3248408.53,	1700230.04,	4786195.41) # AirQ+ results from "Lifetable_CH_2019_PM_constant_AP_with_newborns_default.csv"
      c(3248400.0, 1700225.6, 4786182.9) ## ~ AirQ+ results but slightly adjusted on 2025-07-02, because the female deaths for age 8 was set at 1 in "airqplus_pm_deaths_yll.rds" data set, which is used for this example  (before it was 0, which is not OK, because the calculated survival prob is then 1 which is not realistic)
  )
})


# testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_constant|newborns_TRUE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {
#
#   data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
#
#   testthat::expect_equal(
#     object =
#       healthiar::attribute_lifetable(
#         health_outcome = "yll",
#         approach_exposure = "constant",
#         approach_newborns = "with_newborns",
#         exp_central = data[["input"]]$mean_concentration,
#         cutoff_central = data[["input"]]$cut_off_value,
#         rr_central = data[["input"]]$relative_risk,
#         rr_lower = data[["input"]]$relative_risk_lower,
#         rr_upper = data[["input"]]$relative_risk_upper,
#         rr_increment = 10,
#         erf_shape = base::gsub("-", "_", data[["input"]]$calculation_method),
#         first_age_pop = dplyr::first(data[["pop"]]$age_from...),
#         last_age_pop = dplyr::last(data[["pop"]]$age_from...),
#         population_midyear_male = data[["pop"]]$midyear_population_male,
#         population_midyear_female = data[["pop"]]$midyear_population_female,
#         deaths_male = data[["pop"]]$number_of_deaths_male,
#         deaths_female = data[["pop"]]$number_of_deaths_female,
#         year_of_analysis =  data[["input"]]$start_year,
#         min_age = data[["input"]]$apply_rr_from_age
#       )$health_main$impact,
#     expected =
#       # c(3248408.53,	1700230.04,	4786195.41) # AirQ+ results from "Lifetable_CH_2019_PM_constant_AP_with_newborns_default.csv"
#       c(3248400.0, 1700225.6, 4786182.9) ## ~ AirQ+ results but slightly adjusted on 2025-07-02, because the female deaths for age 8 was set at 1 in "airqplus_pm_deaths_yll.rds" data set, which is used for this example  (before it was 0, which is not OK, because the calculated survival prob is then 1 which is not realistic)
#   )
# })


## PREMATURE DEATHS #############################################################

### SINGLE YEAR EXPOSURE & WITH NEWBORNS ###########################################

testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_single_year|newborns_TRUE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "deaths",
        approach_exposure = "single_year",
        approach_newborns = "with_newborns",
        exp_central = data[["input"]]$mean_concentration,
        cutoff_central = data[["input"]]$cut_off_value,
        rr_central = data[["input"]]$relative_risk,
        rr_lower = data[["input"]]$relative_risk_lower,
        rr_upper = data[["input"]]$relative_risk_upper,
        rr_increment = 10,
        erf_shape = base::gsub("-", "_", data[["input"]]$calculation_method),
        first_age_pop = dplyr::first(data[["pop"]]$age_from...),
        last_age_pop = dplyr::last(data[["pop"]]$age_from...),
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        population_midyear_male = data[["pop"]]$midyear_population_male,
        population_midyear_female = data[["pop"]]$midyear_population_female,
        year_of_analysis =  data[["input"]]$start_year,
        min_age = data[["input"]]$apply_rr_from_age
        )$health_main$impact_rounded,
    expected =
      c(2601, 1371, 3804) # Results on 2025-04-15; Rounded impacts from "airqplus_deaths_yll_lifetable_adults.xlsx" (the YLL impacts were multiplied by 2 to obtain the total premature deaths deaths)
  )
})

### CONSTANT EXPOSURE & NO NEWBORNS ###########################################

testthat::test_that("results the same |pathway_lifetable|exp_dist|exp_time_constant|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "deaths",
        exp_central = c(8, 9, 10), # Fake data just for testing purposes
        prop_pop_exp = c(0.2, 0.3, 0.5), # Fake data just for testing purposes
        cutoff_central = data_mort$cutoff[2],   # PM2.5=5, NO2=10, i.e. WHO AQG 2021
        rr_central = data[["input"]]$relative_risk,
        rr_lower = data[["input"]]$relative_risk_lower,
        rr_upper = data[["input"]]$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 99,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = 20
        )$health_main$impact_rounded,
    expected =
      c(2900, 1531, 4239) # Result on 20 August 2024; no comparison study
  )
})

# ERROR OR WARNING ########
## ERROR #########
testthat::test_that("error if length of age range higher than deaths", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_error(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "deaths",
        exp_central = c(8, 9, 10), # Fake data just for testing purposes
        prop_pop_exp = c(0.2, 0.3, 0.5), # Fake data just for testing purposes
        cutoff_central = data_mort$cutoff[2],   # PM2.5=5, NO2=10, i.e. WHO AQG 2021
        rr_central = data[["input"]]$relative_risk,
        rr_lower = data[["input"]]$relative_risk_lower,
        rr_upper = data[["input"]]$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 150,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = 20),
    regexp = "The length of age range (sequence of first_age_pop and last_age_pop) must be compatible with the age-dependent variables.",
    fixed = TRUE
  )
})

testthat::test_that("error if length geo arguments are different", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_error(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "deaths",
        exp_central = rep(c(8.85, 8.0), each = 100),# Fake data just for testing purposes
        prop_pop_exp = 1, # Fake data just for testing purposes
        cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
        rr_central = 1.118,
        rr_lower = 1.060,
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
        geo_id_disaggregated = rep(c("a", "b"), each = 100),
        geo_id_aggregated = rep("ch", 2 * 100)),
    regexp = "The length of age range (sequence of first_age_pop and last_age_pop) must be compatible with the age-dependent variables.",
    fixed = TRUE
  )
})

testthat::test_that("error if deaths_... argument contains 0", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  data[["pop"]]$number_of_deaths_male[47] <- 0 # 47 chosen randomly

  ## argument deaths_male contains 0
  testthat::expect_error(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        approach_exposure = "single_year",
        exp_central = data_mort$exp[2], #exp CH 2019
        prop_pop_exp = 1,
        cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
        rr_central = data_mort[2,"rr_central"],
        rr_lower = data_mort[2,"rr_lower"],
        rr_upper =data_mort[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 99,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
      )$health_main$impact
  )

  data[["pop"]]$number_of_deaths_female[84] <- 0 # 84 chosen randomly

  ## argument deaths_female contains 0
  testthat::expect_error(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        approach_exposure = "single_year",
        exp_central = data_mort$exp[2], #exp CH 2019
        prop_pop_exp = 1,
        cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
        rr_central = data_mort[2,"rr_central"],
        rr_lower = data_mort[2,"rr_lower"],
        rr_upper =data_mort[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 99,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
      )$health_main$impact
  )
})

testthat::test_that("error if population_... argument contains 0", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  data_lifetable[["male"]]$population[47] <- 0 # 47 chosen randomly

  ## argument population_midyear_male contains 0
  testthat::expect_error(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        approach_exposure = "single_year",
        exp_central = data_mort$exp[2], #exp CH 2019
        prop_pop_exp = 1,
        cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
        rr_central = data_mort[2,"rr_central"],
        rr_lower = data_mort[2,"rr_lower"],
        rr_upper =data_mort[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 99,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
      )$health_main$impact
  )

  data_lifetable[["female"]]$population[84] <- 0 # 84 chosen randomly

  ## argument population_midyear_female contains 0
  testthat::expect_error(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        approach_exposure = "single_year",
        exp_central = data_mort$exp[2], #exp CH 2019
        prop_pop_exp = 1,
        cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
        rr_central = data_mort[2,"rr_central"],
        rr_lower = data_mort[2,"rr_lower"],
        rr_upper =data_mort[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 99,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
      )$health_main$impact
  )
})


## WARNING #########

