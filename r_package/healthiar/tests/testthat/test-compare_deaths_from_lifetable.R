testthat::test_that("results correct delta comparison lifetable rr single exposure", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

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
        min_age = 20) |>
      helper_extract_main_results(),
    expected =
      c(1915, 1013, 2795) # Result on 20 August 2024; no study to compare bestcost results to
  )
})

testthat::test_that("results from scenario 1 match those calculated by attribute call with same input data", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

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
        exp_central = data[["input"]]$mean_concentration,
        cutoff_central = data[["input"]]$cut_off_value,
        rr_central = data[["input"]]$relative_risk,
        rr_lower = data[["input"]]$relative_risk_lower,
        rr_upper = data[["input"]]$relative_risk_upper,
        rr_increment = 10,
        erf_shape = gsub("-", "_", data[["input"]]$calculation_method),
        first_age_pop = first(data[["pop"]]$age_from...),
        last_age_pop = last(data[["pop"]]$age_from...),
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        population_midyear_male = data[["pop"]]$midyear_population_male,
        population_midyear_female = data[["pop"]]$midyear_population_female,
        year_of_analysis =  data[["input"]]$start_year,
        min_age = data[["input"]]$apply_rr_from_age) |>
      helper_extract_main_results()
  )
})

testthat::test_that("results correct delta comparison lifetable iteration rr single exposure", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

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
        deaths_female_2= data[["pop"]]$number_of_deaths_female,
        population_midyear_male_2 = data_lifetable[["male"]]$population,
        population_midyear_female_2 = data_lifetable[["female"]]$population,
        year_of_analysis_2 = 2019,
        info_1 = data_mort$pollutant[2],
        info_2 = data_mort$pollutant[2],
        min_age = 20,
        geo_id_disaggregated = c("a", "b"),
        geo_id_aggregated = rep("ch", 2)) |>
      helper_extract_main_results(),
    expected =
      c(2925, 1546, 4269) # Result on 20 August 2024; no study to compare bestcost results to
  )
})

testthat::test_that("results correct pif comparison lifetable rr single exposure", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

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
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop_1 = 0,
        last_age_pop_1 = 99,
        approach_exposure_1 = "single_year",
        deaths_male_1 = data[["pop"]]$number_of_deaths_male,
        deaths_female_1 = data[["pop"]]$number_of_deaths_female,
        population_midyear_male_1 = data_lifetable[["male"]]$population,
        population_midyear_female_1 = data_lifetable[["female"]]$population,
        year_of_analysis_1 = 2019,
        first_age_pop_2 = 0,
        last_age_pop_2 = 99,
        approach_exposure_2 = "single_year",
        deaths_male_2 = data[["pop"]]$number_of_deaths_male,
        deaths_female_2 = data[["pop"]]$number_of_deaths_female,
        population_midyear_male_2 = data_lifetable[["male"]]$population,
        population_midyear_female_2 = data_lifetable[["female"]]$population,
        year_of_analysis_2 = 2019,
        min_age = 20) |>
      helper_extract_main_results(),
    expected =
      c(1935, 1018, 2837) # Result on 20 August 2024; no study to compare bestcost results to
  )
})

testthat::test_that("results correct pif comparison lifetable iteration rr single exposure", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

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
        geo_id_aggregated = rep("ch", 2)) |>
      helper_extract_main_results(),
    expected =
      c(2961, 1556, 4346) # Result on 20 AUgust 2024; no study to compare bestcost results to
  )
})

