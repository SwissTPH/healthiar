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
      # c(29274.89, 15328.16,	43118.30), # AirQ+ results from "Lifetable_CH_2019_PM_single_year_AP_no_newborns_default.csv"
      c(28810.0511, 15083.5908, 42437.0574) # Result on 09 July 2025
  )
})

testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        approach_exposure = "single_year",
        exp_central = 4, # If exposure lower than cutoff, impact should be 0
        cutoff_central = 5, # WHO AQG 2021
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
      c(0, 0, 0) # Result on 09 July 2025
  )
})


testthat::test_that("results the same |fake_lifetable|exp_dist|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

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
        )$health_main$impact,
    expected =
      c(32185.20309, 16848.80840, 47413.39984) # Result on 09 July 2025 (AirQ+ approach); no comparison study to
  )
})


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
    expected = c(2738323.2, 1432078.6, 4037910.4) # Results on 2025-07-09

  )
})


### CONSTANT EXPOSURE & WITH NEWBORNS ###########################################

testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_constant|newborns_TRUE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  testthat::expect_equal(
    object = healthiar::attribute_lifetable(
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
    expected =c(3207650.6, 1678688.5, 4726739.3) # Results on 2025-07-09
  )
})


## PREMATURE DEATHS #############################################################

### SINGLE YEAR EXPOSURE & WITH NEWBORNS ###########################################

testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_single_year|newborns_TRUE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  testthat::expect_equal(
    object = healthiar::attribute_lifetable(
      health_outcome = "deaths",
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
      # c(2601, 1371, 3804) # Results on 2025-04-15;Rounded impacts from "airqplus_deaths_yll_lifetable_adults.xlsx" (the YLL impacts were multiplied by 2 to obtain the total premature deaths deaths)
      c(2599.365941, 1370.612959, 3801.987144) # Results on 2025-07-09;
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
      )$health_main$impact,
    expected =
      c(2898.8254341, 1529.6159982, 4236.9347910) # Result on 20 August 2024; no comparison study
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
          times = 20), # Should be 3
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
      ),
    regexp = "Not clear what is the maximal length of your arguments: 600, 4000. Check: age_group, sex, exp_central."
  )
})


testthat::test_that("error if bhd argument contains 0", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  data[["pop"]]$number_of_deaths_male[47] <- 0 # 47 chosen randomly

  ## argument deaths_male contains 0
  testthat::expect_error(
    object = healthiar::attribute_lifetable(
      health_outcome = "deaths",
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
      min_age = data[["input"]]$apply_rr_from_age),

    regexp = "All values of bhd_central must be 1 or higher."
  )

})

testthat::test_that("error if population argument contains 0", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  data[["pop"]]$midyear_population_male[47] <- 0 # 47 chosen randomly

  ## argument population contains 0
  ## argument bhd contains 0
  testthat::expect_error(
    object = healthiar::attribute_lifetable(
      health_outcome = "deaths",
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
      min_age = data[["input"]]$apply_rr_from_age),

    regexp = "All values of population must be 1 or higher."
  )
})

testthat::test_that("error if exposuer lower than 0 | lifetable", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_error(
    object = healthiar::attribute_lifetable(
      health_outcome = "deaths",
      approach_exposure = "constant",
      approach_newborns = "with_newborns",
      exp_central = - 4,
      cutoff_central = 5,
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
      min_age = data[["input"]]$apply_rr_from_age),

    regexp = "exp_central cannot be lower than 0."
  )
})


## WARNING #########

