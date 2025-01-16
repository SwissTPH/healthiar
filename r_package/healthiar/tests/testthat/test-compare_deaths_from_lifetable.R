test_that("results correct delta comparison lifetable rr single exposure", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::compare_deaths_from_lifetable(
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
        min_age = 20) |>
      helper_extract_main_results(),
    expected =
      c(1915, 1013, 2795) # Result on 20 August 2024; no study to compare bestcost results to
  )
})

test_that("results from scenario 1 match those calculated by attribute call with same input data", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::compare_deaths_from_lifetable(
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
        min_age = 20) |>
      purrr::pluck("health_main") |>
      dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
      dplyr::select(impact_rounded_1) |>
      base::unlist() |>
      base::as.numeric(),
    expected =
      healthiar::attribute_deaths_from_lifetable(
        approach_exposure = "single_year",
        approach_newborns = "with_newborns",
        exp_central = airqplus_pm_deaths_yll[["input"]]$mean_concentration,
        cutoff_central = airqplus_pm_deaths_yll[["input"]]$cut_off_value,
        rr_central = airqplus_pm_deaths_yll[["input"]]$relative_risk,
        rr_lower = airqplus_pm_deaths_yll[["input"]]$relative_risk_lower,
        rr_upper = airqplus_pm_deaths_yll[["input"]]$relative_risk_upper,
        erf_increment = 10,
        erf_shape = gsub("-", "_", airqplus_pm_deaths_yll[["input"]]$calculation_method),
        first_age_pop = first(airqplus_pm_deaths_yll[["pop"]]$age_from...),
        last_age_pop = last(airqplus_pm_deaths_yll[["pop"]]$age_from...),
        deaths_male = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male = airqplus_pm_deaths_yll[["pop"]]$midyear_population_male,
        population_midyear_female = airqplus_pm_deaths_yll[["pop"]]$midyear_population_female,
        year_of_analysis =  airqplus_pm_deaths_yll[["input"]]$start_year,
        min_age = airqplus_pm_deaths_yll[["input"]]$apply_rr_from_age) |>
      helper_extract_main_results()
  )
})

test_that("results correct delta comparison lifetable iteration rr single exposure", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::compare_deaths_from_lifetable(
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
        deaths_female_2= airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male_2 = lifetable_withPopulation[["male"]]$population,
        population_midyear_female_2 = lifetable_withPopulation[["female"]]$population,
        year_of_analysis_2 = 2019,
        info_1 = input_data_mortality$pollutant[2],
        info_2 = input_data_mortality$pollutant[2],
        min_age = 20,
        geo_id_disaggregated = c("a", "b"),
        geo_id_aggregated = rep("ch", 2)) |>
      helper_extract_main_results(),
    expected =
      c(2925, 1546, 4269) # Result on 20 August 2024; no study to compare bestcost results to
  )
})

test_that("results correct pif comparison lifetable rr single exposure", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::compare_deaths_from_lifetable(
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
        approach_exposure_1 = "single_year",
        deaths_male_1 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female_1 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male_1 = lifetable_withPopulation[["male"]]$population,
        population_midyear_female_1 = lifetable_withPopulation[["female"]]$population,
        year_of_analysis_1 = 2019,
        first_age_pop_2 = 0,
        last_age_pop_2 = 99,
        approach_exposure_2 = "single_year",
        deaths_male_2 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female_2 = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male_2 = lifetable_withPopulation[["male"]]$population,
        population_midyear_female_2 = lifetable_withPopulation[["female"]]$population,
        year_of_analysis_2 = 2019,
        min_age = 20) |>
      helper_extract_main_results(),
    expected =
      c(1935, 1018, 2837) # Result on 20 August 2024; no study to compare bestcost results to
  )
})

test_that("results correct pif comparison lifetable iteration rr single exposure", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      healthiar::compare_deaths_from_lifetable(
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
        geo_id_aggregated = rep("ch", 2)) |>
      helper_extract_main_results(),
    expected =
      c(2961, 1556, 4346) # Result on 20 AUgust 2024; no study to compare bestcost results to
  )
})

