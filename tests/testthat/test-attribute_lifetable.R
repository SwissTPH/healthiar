# QUANTITATIVE TEST ############################################################

## YLL FROM LIFE TABLE ###########################################################

### SINGLE YEAR EXPOSURE & NO NEWBORNS ##########################################

#### ONE GEO UNIT ##############################################################

testthat::test_that("results the same |pathway_lifetable|exp_single|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

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

  # The same but using fraction_lived
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
        fraction_lived = 0.5,  # The only new input data compared to test above
        info = data_mort$pollutant[2],
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
      )$health_main$impact,
    expected =
      # c(29274.89, 15328.16,	43118.30), # AirQ+ results from "Lifetable_CH_2019_PM_single_year_AP_no_newborns_default.csv"
      c(28810.0511, 15083.5908, 42437.0574) # Result on 13 Aug 2026 
      # Same result as above because fraction_lived = 0.5 is default value
  )

  # The same but using a different fraction_lived
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
        fraction_lived = 0.4,  # The only new input data compared to test above
        info = data_mort$pollutant[2],
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
      )$health_main$impact,
    expected =
      # c(29274.89, 15328.16,	43118.30), # AirQ+ results from "Lifetable_CH_2019_PM_single_year_AP_no_newborns_default.csv"
      c(28853.312, 15106.332, 42500.549) # Result on 13 Aug 2026 
      # Same result as above because fraction_lived = 0.5 is default value
  )

  # Check the same but for impact_per_100k_inhab

  pop <- sum(c(data_lifetable[["male"]]$population,
               data_lifetable[["female"]]$population))

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
      )$health_main$impact_per_100k_inhab,
    expected =
      c(28810.0511, 15083.5908, 42437.0574)/pop*1E5 # Result on 28 Oct 2025
  )
 # Check the same but for impact_per_100k_inhab AND detailed results
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
      )$health_detailed$results_by_sex$impact_per_100k_inhab,
    expected = c(
      c(15143.22958, 7925.86465, 22312.77159) / sum(data_lifetable[["male"]]$population),
      c(13666.82149, 7157.72614, 20124.28583) / sum(data_lifetable[["female"]]$population)) * 1E5  # Result on 28 Oct 2025
  )  
})

testthat::test_that("results the same |pathway_lifetable|exp_single|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_TRUE|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  # Check that decimals between 0 and 1 are allowed in bhd and population
  population <- 
    c(data_lifetable[["male"]]$population,
      data_lifetable[["female"]]$population)
  population[100] <- 0.5 

  bhd_central <- 
    c(data[["pop"]]$number_of_deaths_male,
      data[["pop"]]$number_of_deaths_female)
  bhd_central[100] <- 0.3  
    

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
        population = population,
        bhd_central = bhd_central,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
      )$health_main$impact,
    expected =
      c(28814.1063, 15085.5567, 42443.4722) # Result on 06 August 2026
  )
})



testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_TRUE|iteration_FALSE|", {

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
        time_horizon = 80,
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
      )$health_main$impact,
    expected = c(13417.327414, 7069.442606, 19639.461930) # Result on 11 Sept 2025
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


testthat::test_that("results the same |pathway_lifetable|exp_single|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_TRUE|time_horizon_FALSE|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  # Only min_age and max_age change across the calls below,
  # so they are the only arguments of this function
  attribute_lifetable_with_age_limits <-
    function(health_outcome, min_age, max_age = NULL){
      healthiar::attribute_lifetable(
        health_outcome = health_outcome,
        approach_exposure = "single_year",
        exp_central = data_mort$exp[2], #exp CH 2019
        prop_pop_exp = 1,
        cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
        rr_central = data_mort[2,"rr_central"],
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
        min_age = min_age,
        max_age = max_age)$health_main$impact
    }

  # Impacts with an upper age limit
  testthat::expect_equal(
    object = attribute_lifetable_with_age_limits("yll", min_age = 20, max_age = 60),
    expected = 6986.725237) # Result on 01 Sept 2026

  testthat::expect_equal(
    object = attribute_lifetable_with_age_limits("deaths", min_age = 20, max_age = 60),
    expected = 214.912935) # Result on 01 Sept 2026

  # The higher the max_age, the higher the impact
  # (more age groups affected by the exposure)
  testthat::expect_equal(
    object = attribute_lifetable_with_age_limits("deaths", min_age = 20, max_age = 70),
    expected = 512.62539) # Result on 01 Sept 2026

  # A max_age equal to the last age group must deliver the same result
  # as not entering max_age at all (default)
  testthat::expect_equal(
    object = attribute_lifetable_with_age_limits("yll", min_age = 20, max_age = 99),
    expected = attribute_lifetable_with_age_limits("yll", min_age = 20))

  # If max_age is lower than min_age, no age group is affected by the exposure
  testthat::expect_equal(
    object = attribute_lifetable_with_age_limits("yll", min_age = 20, max_age = 10),
    expected = 0)

  # Premature deaths in the year of analysis are calculated for each age group
  # separately (no projection involved), so the impacts of two consecutive
  # age ranges must add up to the impact of the whole age range.
  # This checks that max_age cuts the age range where it should
  testthat::expect_equal(
    object = attribute_lifetable_with_age_limits("deaths", min_age = 20, max_age = 99),
    expected =
      attribute_lifetable_with_age_limits("deaths", min_age = 20, max_age = 59) +
      attribute_lifetable_with_age_limits("deaths", min_age = 60, max_age = 99))
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

##### ONE SEX ###########################
testthat::test_that("results the same |fake_lifetable|exp_dist|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {
  # EKV2010 data
  data <- base::readRDS(testthat::test_path("data", "lifetable_male_ekv_2010.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        exp_central = 10,
        cutoff_central = 0,
        rr_central = 1.045,
        rr_increment = 10,
        erf_shape = "log_linear",
        age_group = data$age,
        sex = base::rep(c("male"), each = 106),
        population = data$population_male,
        bhd_central = as.numeric(data$deaths_natural_male),
        year_of_analysis = 2010,
        min_age = 20
      )$health_main$impact,
    expected = 14973.76248)
  # The result of the EKV2010 was 14029 but they applied a slightly different method
  # e.g. entering probability of dying as input data.

})

#### MULTIPLE GEO UNITS ######

testthat::test_that("results the same |pathway_lifetable|exp_single|exp_time_single_year|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        exp_central = rep(c(8.85, 8.0), each = 2 * 100) , # Fake data just for testing purposes
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
        geo_id_macro = rep("ch", each = 2 * 2 * 100))$health_main$impact,
    expected = c(51282.460, 26843.268, 75555.482) # Results on 2026-01-15

  )
})



### CONSTANT EXPOSURE & NO NEWBORNS #############################################

#### ONE GEO UNIT #####################################
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

#### MULTIPLE GEO UNITS ######

testthat::test_that("results the same |pathway_lifetable|exp_single|exp_time_constant|newborns_FALSE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        approach_exposure = "constant",
        approach_newborns = "without_newborns",
        exp_central = rep(c(8.85, 8.0), each = 2 * 100) , # Fake data just for testing purposes
        prop_pop_exp = 1, # Fake data just for testing purposes
        cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
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
        geo_id_macro = rep("ch", each = 2 * 2 * 100))$health_main$impact,
    expected = c(4873223.7, 2548295.7, 7186872.3) # Results on 2025-07-09

  )
})

### CONSTANT EXPOSURE & WITH NEWBORNS ###########################################

#### ONE GEO UNIT #############################################################
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

#### MULTIPLE GEO UNITS ######

testthat::test_that("results the same |pathway_lifetable|exp_single|exp_time_constant|newborns_TRUE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_lifetable(
        health_outcome = "yll",
        approach_exposure = "constant",
        approach_newborns = "with_newborns",
        exp_central = rep(c(8.85, 8.0), each = 2 * 100) , # Fake data just for testing purposes
        prop_pop_exp = 1, # Fake data just for testing purposes
        cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
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
        geo_id_macro = rep("ch", each = 2 * 2 * 100))$health_main$impact,
    expected = c(5709248.8, 2987339.5, 8414599.0) # Results on 2025-07-09

  )
})

## PREMATURE DEATHS #############################################################

### SINGLE YEAR EXPOSURE & WITH NEWBORNS ###########################################

testthat::test_that("results correct |pathway_lifetable|exp_single|exp_time_single_year|newborns_TRUE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  testthat::expect_equal(
    object = healthiar::attribute_lifetable(
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
      c(2599.365941, 1370.612959, 3801.987144) # Results on 2026-08-10;
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

### CONSTANT EXPOSURE & WITH NEWBORNS ###########################################
testthat::test_that("results the same |pathway_lifetable|exp_single|exp_time_constant|newborns_TRUE|min_age_TRUE|max_age_FALSE|time_horizon_FALSE|iteration_FALSE|", {

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
      c(59902.2923, 30852.311, 89698.1514) # Results on 2026-08-11;
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

  data[["pop"]]$number_of_deaths_male[47] <- -1 # 47 chosen randomly

  ## argument deaths_male contains negative numbers
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

    regexp = "The values in the following arguments must be 0 or higher: bhd_central."
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

    regexp = "The values in the following arguments must be higher than 0: population."
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

    regexp = "The values in the following arguments must not be lower than 0: exp_central."
  )
})

testthat::test_that("error if age_group is not a 1-year consecutive sequence", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  attribute_lifetable_with_age_group <- function(age_group){
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      approach_exposure = "single_year",
      exp_central = 8.85,
      prop_pop_exp = 1,
      cutoff_central = 5,
      rr_central = 1.118,
      rr_increment = 10,
      erf_shape = "log_linear",
      age_group = base::rep(age_group, times = 2),
      sex = base::rep(c("male", "female"), each = base::length(age_group)),
      population = base::rep(10000, 2 * base::length(age_group)),
      bhd_central = base::rep(50, 2 * base::length(age_group)),
      year_of_analysis = 2019,
      min_age = 20)
  }

  regexp_age_group <-
    "age_group must be a consecutive sequence of integer values where the difference between elements is 1."

  # 5-year age groups
  testthat::expect_error(
    object = attribute_lifetable_with_age_group(base::seq(0, 95, 5)),
    regexp = regexp_age_group,
    fixed = TRUE)

  # Gap in the sequence
  testthat::expect_error(
    object = attribute_lifetable_with_age_group(c(0:49, 60:99)),
    regexp = regexp_age_group,
    fixed = TRUE)

  # Not integers
  testthat::expect_error(
    object = attribute_lifetable_with_age_group(base::seq(0, 49.5, 0.5)),
    regexp = regexp_age_group,
    fixed = TRUE)

  # 1-year age groups must NOT raise the error
  # (the age groups are repeated for each sex, so the check must be done
  # on the distinct values and not on the vector as entered)
  testthat::expect_no_error(
    object = attribute_lifetable_with_age_group(0:99))
})


## WARNING #########
testthat::test_that("warning if any bhd = 0", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  # Check that 0 in bhd is allowed allowed with warning
  population <- 
    c(data_lifetable[["male"]]$population,
      data_lifetable[["female"]]$population)
  population[99] <- 0.5 

  bhd_central <- 
    c(data[["pop"]]$number_of_deaths_male,
      data[["pop"]]$number_of_deaths_female)
  bhd_central[99] <- 0  
    

    testthat::expect_warning(
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
        population = population,
        bhd_central = bhd_central,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2]
      )$health_main$impact,
    regexp =
      "Zeros in bhd_ arguments are theoretically possible") # Result on 06 August 2026
})
