test_that("results correct delta comparison lifetable yld rr single exposure", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::compare_yld_from_lifetable(
        approach_comparison = "delta",
        exp_central_1 = 8.85, # Fake data just for testing purposes
        prop_pop_exp_1 = 1, # Fake data just for testing purposes
        exp_central_2 = 6, # Fake data just for testing purposes
        prop_pop_exp_2 = 1, # Fake data just for testing purposes
        cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        erf_increment = 10,
        erf_shape = "log_linear",
        first_age_pop_1 = 0,
        last_age_pop_1 = 99,
        deaths_male_1 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female_1 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male_1 = lifetable_withPopulation[["male"]]$population,
        population_midyear_female_1 = lifetable_withPopulation[["female"]]$population,
        year_of_analysis_1 = 2019,
        first_age_pop_2 = 0,
        last_age_pop_2 = 99,
        deaths_male_2 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female_2 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male_2 = lifetable_withPopulation[["male"]]$population,
        population_midyear_female_2 = lifetable_withPopulation[["female"]]$population,
        year_of_analysis_2 = 2019,
        info_1 = input_data_mortality$pollutant[2],
        info_2 = input_data_mortality$pollutant[2],
        min_age = 20,
        duration_central = 100,
        dw_central = 1) |>
      helper_extract_main_results(),
    expected =
      c(21644, 11340, 31860) # Result on 20 August 2024; no study to compare bestcost results to
  )
})

test_that("results correct delta comparison lifetable yld iteration rr single exposure", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::compare_yld_from_lifetable(
        approach_comparison = "delta",
        exp_central_1 = list(8.85, 8.0),# Fake data just for testing purposes
        exp_central_2 = list(6, 6.5),# Fake data just for testing purposes
        cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        erf_increment = 10,
        erf_shape = "log_linear",
        first_age_pop_1 = 0,
        last_age_pop_1 = 99,
        deaths_male_1 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female_1 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male_1 = lifetable_withPopulation[["male"]]$population,
        population_midyear_female_1 = lifetable_withPopulation[["female"]]$population,
        year_of_analysis_1 = 2019,
        first_age_pop_2 = 0,
        last_age_pop_2 = 99,
        deaths_male_2 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female_2 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male_2 = lifetable_withPopulation[["male"]]$population,
        population_midyear_female_2 = lifetable_withPopulation[["female"]]$population,
        year_of_analysis_2 = 2019,
        info_1 = input_data_mortality$pollutant[2],
        info_2 = input_data_mortality$pollutant[2],
        min_age = 20,
        geo_id_disaggregated = c("a", "b"),
        geo_id_aggregated = rep("ch", 2),
        duration_central = 100,
        dw_central = 1) |>
      helper_extract_main_results(),
    expected =
      c(33041, 17309, 48639) # Result on 20 August 2024; no study to compare bestcost results to
  )
})

test_that("results correct pif comparison lifetable yld rr single exposure", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::compare_yld_from_lifetable(
        approach_comparison = "pif",
        exp_central_1 = 8.85, # Fake data just for testing purposes
        prop_pop_exp_1 = 1, # Fake data just for testing purposes
        exp_central_2 = 6, # Fake data just for testing purposes
        prop_pop_exp_2 = 1, # Fake data just for testing purposes
        cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        erf_increment = 10,
        erf_shape = "log_linear",
        first_age_pop_1 = 0,
        last_age_pop_1 = 99,
        deaths_male_1 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female_1 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male_1 = lifetable_withPopulation[["male"]]$population,
        population_midyear_female_1 = lifetable_withPopulation[["female"]]$population,
        year_of_analysis_1 = 2019,
        first_age_pop_2 = 0,
        last_age_pop_2 = 99,
        deaths_male_2 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female_2 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male_2 = lifetable_withPopulation[["male"]]$population,
        population_midyear_female_2 = lifetable_withPopulation[["female"]]$population,
        year_of_analysis_2 = 2019,
        info_1 = NULL,
        info_2 = NULL,
        min_age = 20,
        duration_central = 100,
        dw_central = 1) |>
      helper_extract_main_results(),
    expected =
      c(21698, 11354, 31978) # Result on 20 August 2024; no study to compare bestcost results to
  )
})

test_that("results correct pif comparison lifetable yld iteration rr single exposure", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::compare_yld_from_lifetable(
        approach_comparison = "pif",
        exp_central_1 = list(8.85, 8.0),# Fake data just for testing purposes
        exp_central_2 = list(6, 6.5),# Fake data just for testing purposes
        cutoff_central = 5,   # PM2.5=5, WHO AQG 2021
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        erf_increment = 10,
        erf_shape = "log_linear",
        first_age_pop_1 = 0,
        last_age_pop_1 = 99,
        deaths_male_1 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female_1 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male_1 = lifetable_withPopulation[["male"]]$population,
        population_midyear_female_1 = lifetable_withPopulation[["female"]]$population,
        year_of_analysis_1 = 2019,
        first_age_pop_2 = 0,
        last_age_pop_2 = 99,
        deaths_male_2 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female_2 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male_2 = lifetable_withPopulation[["male"]]$population,
        population_midyear_female_2 = lifetable_withPopulation[["female"]]$population,
        year_of_analysis_2 = 2019,
        info_1 = input_data_mortality$pollutant[2],
        info_2 = input_data_mortality$pollutant[2],
        min_age = 20,
        geo_id_disaggregated = c("a", "b"),
        geo_id_aggregated = rep("ch", 2),
        duration_central = 100,
        dw_central = 1) |>
      helper_extract_main_results(),
    expected =
      c(33137, 17335, 48851) # Result on 20 August 2024; no study to compare bestcost results to
  )
})

