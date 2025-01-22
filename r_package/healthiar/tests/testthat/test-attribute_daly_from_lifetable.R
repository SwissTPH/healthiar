testthat::test_that("results correct lifetable daly single exposure and one year exposure without newborns", {

  data <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))
  data_airqplus <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))

  testthat::expect_equal(
    object =
      attribute_daly_from_lifetable(
        exp_central = data$exp[2],
        prop_pop_exp = 1,
        cutoff_central = data$cutoff[2],
        rr_central = data[2,"rr_central"],
        rr_lower = data[2,"rr_lower"],
        rr_upper =data[2,"rr_upper"],
        rr_increment = 10,
        erf_shape = "log_linear",
        first_age_pop = 0,
        last_age_pop = 99,
        deaths_male = data_airqplus[["pop"]]$number_of_deaths_male,
        deaths_female = data_airqplus[["pop"]]$number_of_deaths_female,
        population_midyear_male = data_lifetable[["male"]]$population,
        population_midyear_female = data_lifetable[["female"]]$population,
        year_of_analysis = 2019,
        info = data$pollutant[2],
        min_age = data$min_age[2],
        duration_central = 100,
        dw_central = 1) |>
      purrr::pluck("health_main") |>
      dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
      dplyr::select(impact)  |>
      base::unlist() |>
      base::as.numeric(),
    expected =
      c(2974.89, 15328.16,	43118.30) * 2, # AirQ+ results from "Lifetable_CH_2019_PM_single_year_AP_no_newborns_default.csv" multiplied by 2 (because dw = 1 = YLL, so it's basically 2 times YLL calculation)
    tolerance = 0.49 # I.e. less than 1 YLL
  )
})
