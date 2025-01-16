testthat::test_that("results correct lifetable premature deaths single year exposure with newborns", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_deaths_from_lifetable(
        approach_exposure = "single_year",
        approach_newborns = "with_newborns",
        exp_central = data[["input"]]$mean_concentration,
        cutoff_central = data[["input"]]$cut_off_value,
        rr_central = data[["input"]]$relative_risk,
        rr_lower = data[["input"]]$relative_risk_lower,
        rr_upper = data[["input"]]$relative_risk_upper,
        erf_increment = 10,
        erf_shape = gsub("-", "_", data[["input"]]$calculation_method),
        first_age_pop = first(data[["pop"]]$age_from...),
        last_age_pop = last(data[["pop"]]$age_from...),
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        population_midyear_male = data[["pop"]]$midyear_population_male,
        population_midyear_female = data[["pop"]]$midyear_population_female,
        year_of_analysis =  data[["input"]]$start_year,
        min_age = data[["input"]]$apply_rr_from_age) |>
      helper_extract_main_results(),
    expected =
      c(2601, 1371, 3804) # Rounded impacts from "airqplus_deaths_yll_lifetable_adults.xlsx" (the YLL impacts were multiplied by 2 to obtain the total premature deaths deaths)
  )
})

testthat::test_that("results correct lifetable premature deaths exposure distribution with constant exposure and without newborns", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_deaths_from_lifetable(
        exp_central = c(8, 9, 10), # Fake data just for testing purposes
        prop_pop_exp = c(0.2, 0.3, 0.5), # Fake data just for testing purposes
        cutoff_central = data_mort$cutoff[2],   # PM2.5=5, NO2=10, i.e. WHO AQG 2021
        rr_central = data[["input"]]$relative_risk,
        rr_lower = data[["input"]]$relative_risk_lower,
        rr_upper = data[["input"]]$relative_risk_upper,
        erf_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 99,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = 20) |>
      helper_extract_main_results(),
    expected =
      c(2900, 1531, 4239) # Result on 20 August 2024; no study to compare bestcost results
  )
})

