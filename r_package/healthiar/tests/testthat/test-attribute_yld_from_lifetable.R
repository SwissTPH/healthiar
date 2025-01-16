testthat::test_that("results correct lifetable yld single exposure and one year exposure without newborns", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_equal(
    object =
      attribute_yld_from_lifetable(
        exp_central = data_mort$exp[2],
        prop_pop_exp = 1,
        cutoff_central = data_mort$cutoff[2],
        rr_central = data_mort[2,"rr_central"],
        rr_lower = data_mort[2,"rr_lower"],
        rr_upper = data_mort[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        approach_exposure = "single_year",
        first_age_pop = 0,
        last_age_pop = 99,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = data_mort$min_age[2],
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

testthat::test_that("no error lifetable yld exposure distribution and one year exposure without newborns", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  testthat::expect_no_error(
    object =
      attribute_yld_from_lifetable(
        exp_central = c(8, 9, 10), # Fake data. Deactivate to test exposure distribution.
        prop_pop_exp = c(0.2, 0.3, 0.5), # Fake data. Deactivate to test exposure distribution.
        cutoff_central = data_mort$cutoff[2],
        rr_central = data_mort[2,"rr_central"],
        rr_lower = data_mort[2,"rr_lower"],
        rr_upper = data_mort[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        approach_exposure = "single_year",
        first_age_pop = 0,
        last_age_pop = 99,
        deaths_male = data[["pop"]]$number_of_deaths_male,
        deaths_female = data[["pop"]]$number_of_deaths_female,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        year_of_analysis = 2019,
        info = data_mort$pollutant[2],
        min_age = data_mort$min_age[2],
        duration_central = 100,
        duration_lower = 50, duration_upper = 100,
        dw_central = 0.5, dw_lower = 0.1, dw_upper = 10
      )
  )
})

