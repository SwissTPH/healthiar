testthat::test_that("results correct delta comparison lifetable daly rr single exposure", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::compare_daly_from_lifetable(
        approach_comparison = "delta",
        exp_central_1 = 8.85, # Fake data just for testing purposes
        prop_pop_exp_1 = 1, # Fake data just for testing purposes
        exp_central_2 = 6, # Fake data just for testing purposes
        prop_pop_exp_2 = 1, # Fake data just for testing purposes
        cutoff_central = 5, # PM2.5=5, WHO AQG 2021
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop_1 = 0,
        last_age_pop_1 = 99,
        deaths_male_1 = data[["pop"]]$number_of_deaths_male,
        deaths_female_1 = data[["pop"]]$number_of_deaths_female,
        population_midyear_male_1 = data_lifetable[["male"]]$population,
        population_midyear_female_1 = data_lifetable[["female"]]$population,
        year_of_analysis_1 = 2019,
        first_age_pop_2 = 0,
        last_age_pop_2 = 99,
        deaths_male_2 = data[["pop"]]$number_of_deaths_male,
        deaths_female_2 = data[["pop"]]$number_of_deaths_female,
        population_midyear_male_2 = data_lifetable[["male"]]$population,
        population_midyear_female_2 = data_lifetable[["female"]]$population,
        year_of_analysis_2 = 2019,
        info_1 = data_mort$pollutant[2],
        info_2 = data_mort$pollutant[2],
        min_age = 20,
        duration_central = 100,
        dw_central = 1) |>
      helper_extract_main_results(),
    expected =
      c(43288, 22679, 63720) # Result on 20 August 2024; no comparison study
  )
})

testthat::test_that("results correct delta comparison lifetable daly iteration rr single exposure", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::compare_daly_from_lifetable(
        approach_comparison = "delta",
        exp_central_1 = list(8.85, 8.0),# Fake data just for testing purposes
        exp_central_2 = list(6, 6.5),# Fake data just for testing purposes
        cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop_1 = 0,
        last_age_pop_1 = 99,
        deaths_male_1 = data[["pop"]]$number_of_deaths_male,
        deaths_female_1 = data[["pop"]]$number_of_deaths_female,
        population_midyear_male_1 = data_lifetable[["male"]]$population,
        population_midyear_female_1 = data_lifetable[["female"]]$population,
        year_of_analysis_1 = 2019,
        first_age_pop_2 = 0,
        last_age_pop_2 = 99,
        deaths_male_2 = data[["pop"]]$number_of_deaths_male,
        deaths_female_2 = data[["pop"]]$number_of_deaths_female,
        population_midyear_male_2 = data_lifetable[["male"]]$population,
        population_midyear_female_2 = data_lifetable[["female"]]$population,
        year_of_analysis_2 = 2019,
        info_1 = data_mort$pollutant[2],
        info_2 = data_mort$pollutant[2],
        min_age = 20,
        geo_id_disaggregated = c("a", "b"),
        geo_id_aggregated = rep("ch", 2),
        duration_central = 100,
        dw_central = 1) |>
      helper_extract_main_results(),
    expected =
      c(66082, 34618, 97279) # Result on 20 August 2024; no comparison study
  )
})

testthat::test_that("results correct pif comparison lifetable daly rr single exposure", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::compare_daly_from_lifetable(
        approach_comparison = "pif",
        exp_central_1 = 8.85, # Fake data just for testing purposes
        prop_pop_exp_1 = 1, # Fake data just for testing purposes
        exp_central_2 = 6, # Fake data just for testing purposes
        prop_pop_exp_2 = 1, # Fake data just for testing purposes
        cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop_1 = 0,
        last_age_pop_1 = 99,
        deaths_male_1 = data[["pop"]]$number_of_deaths_male,
        deaths_female_1 = data[["pop"]]$number_of_deaths_female,
        population_midyear_male_1 = data_lifetable[["male"]]$population,
        population_midyear_female_1 = data_lifetable[["female"]]$population,
        year_of_analysis_1 = 2019,
        first_age_pop_2 = 0,
        last_age_pop_2 = 99,
        deaths_male_2 = data[["pop"]]$number_of_deaths_male,
        deaths_female_2 = data[["pop"]]$number_of_deaths_female,
        population_midyear_male_2 = data_lifetable[["male"]]$population,
        population_midyear_female_2 = data_lifetable[["female"]]$population,
        year_of_analysis_2 = 2019,
        info_1 = NULL,
        info_2 = NULL,
        min_age = 20,
        duration_central = 100,
        dw_central = 1) |>
      helper_extract_main_results(),
    expected =
      c(43396, 22708, 63957) # Result on 20 August 2024; no comparison study
  )
})

testthat::test_that("results correct pif comparison lifetable daly iteration rr single exposure", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::compare_daly_from_lifetable(
        approach_comparison = "pif",
        exp_central_1 = list(8.85, 8.0),# Fake data just for testing purposes
        exp_central_2 = list(6, 6.5),# Fake data just for testing purposes
        cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop_1 = 0,
        last_age_pop_1 = 99,
        deaths_male_1 = data[["pop"]]$number_of_deaths_male,
        deaths_female_1 = data[["pop"]]$number_of_deaths_female,
        population_midyear_male_1 = data_lifetable[["male"]]$population,
        population_midyear_female_1 = data_lifetable[["female"]]$population,
        year_of_analysis_1 = 2019,
        first_age_pop_2 = 0,
        last_age_pop_2 = 99,
        deaths_male_2 = data[["pop"]]$number_of_deaths_male,
        deaths_female_2 = data[["pop"]]$number_of_deaths_female,
        population_midyear_male_2 = data_lifetable[["male"]]$population,
        population_midyear_female_2 = data_lifetable[["female"]]$population,
        year_of_analysis_2 = 2019,
        info_1 = data_mort$pollutant[2],
        info_2 = data_mort$pollutant[2],
        min_age = 20,
        geo_id_disaggregated = c("a", "b"),
        geo_id_aggregated = rep("ch", 2),
        duration_central = 100,
        dw_central = 1) |>
      helper_extract_main_results(),
    expected =
      c(66275, 34671, 97702) # Result on 20 August 2024; no comparison study
  )
})
