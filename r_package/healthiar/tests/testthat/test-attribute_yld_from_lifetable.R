test_that("results correct lifetable yld single exposure and one year exposure without newborns", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_equal(
    object =
      attribute_yld_from_lifetable(
        exp_central = input_data_mortality$exp[2],
        prop_pop_exp = 1,
        cutoff_central = input_data_mortality$cutoff[2],
        rr_central = input_data_mortality[2,"rr_central"],
        rr_lower = input_data_mortality[2,"rr_lower"],
        rr_upper = input_data_mortality[2,"rr_upper"],
        erf_increment = 10,
        erf_shape = "log_linear",
        approach_exposure = "single_year",
        first_age_pop = 0,
        last_age_pop = 99,
        deaths_male = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male = lifetable_withPopulation[["male"]]$population,
        population_midyear_female = lifetable_withPopulation[["female"]]$population,
        year_of_analysis = 2019,
        info = input_data_mortality$pollutant[2],
        min_age = input_data_mortality$min_age[2], # min_age = 20,
        duration_central = 100,
        duration_lower = 50, duration_upper = 100,
        dw_central = 0.5, dw_lower = 0.1, dw_upper = 10
      ) |>
      purrr::pluck("health_main") |>
      dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
      dplyr::select(impact)  |>
      base::unlist() |>
      base::as.numeric(),
    expected =
      c(2974.89, 15328.16,	43118.30) / 2, # AirQ+ results from "Lifetable_CH_2019_PM_single_year_AP_no_newborns_default.csv" divided by 2 (because dw = 0.5)
    tolerance = 0.49 # I.e. less than 1 YLL
  )
})

test_that("no error lifetable yld exposure distribution and one year exposure without newborns", {

  base::load(testthat::test_path("data", "input_data_for_testing_Rpackage.Rdata"))

  testthat::expect_no_error(
    object =
      attribute_yld_from_lifetable(
        exp_central = c(8, 9, 10), # Fake data. Deactivate to test exposure distribution.
        prop_pop_exp = c(0.2, 0.3, 0.5), # Fake data. Deactivate to test exposure distribution.
        cutoff_central = input_data_mortality$cutoff[2],
        rr_central = input_data_mortality[2,"rr_central"],
        rr_lower = input_data_mortality[2,"rr_lower"],
        rr_upper = input_data_mortality[2,"rr_upper"],
        erf_increment = 10,
        erf_shape = "log_linear",
        approach_exposure = "single_year",
        first_age_pop = 0,
        last_age_pop = 99,
        deaths_male = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_male,
        deaths_female = airqplus_pm_deaths_yll[["pop"]]$number_of_deaths_female,
        population_midyear_male = lifetable_withPopulation[["male"]]$population,
        population_midyear_female = lifetable_withPopulation[["female"]]$population,
        year_of_analysis = 2019,
        info = input_data_mortality$pollutant[2],
        min_age = input_data_mortality$min_age[2], # min_age = 20,
        duration_central = 100,
        duration_lower = 50, duration_upper = 100,
        dw_central = 0.5, dw_lower = 0.1, dw_upper = 10
      )
  )
})

