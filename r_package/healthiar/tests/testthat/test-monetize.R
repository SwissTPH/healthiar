# QUANTITATIVE TEST ############################################################
## RAW INPUT ####################################################################

### NO DISCOUNTING ##############################################################

testthat::test_that("results correct |pathway_monetization|discount_rate_FALSE|discount_shape_exponential|inflation_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_copd <-
    healthiar::attribute_health(
      exp_central = data$mean_concentration,
      cutoff_central = data$cut_off_value,
      bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
      rr_central = data$relative_risk,
      rr_lower = data$relative_risk_lower,
      rr_upper = data$relative_risk_upper,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = paste0(data$pollutant,"_", data$evaluation_name))

  testthat::expect_equal(
    object =
      healthiar::monetize(
        output_attribute = bestcost_pm_copd,
        valuation = 1000,
        discount_rate = NULL
        )$monetization_main$monetized_impact_rounded,
    expect = # 1000 * airqplus_pm_copd
      round(1000 * bestcost_pm_copd[["health_main"]]$impact)
  )
})

### DISCOUNTING ############################################################

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_exponential|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::discount(
        impact = 2E4,
        discount_shape = "exponential",
        discount_rate = 0.03,
        discount_years = 20
        )$monetization_main$monetized_impact_rounded,
    expect =
      round(11074) # Results on 2025-04-15; no comparison study
  )
})

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_exponential|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = c(800, 1000, 1200, 1500, 1800, 2000),
        discount_shape = "exponential",
        discount_rate = 0.05,
        discount_years = 5,
        valuation = 1
        )$monetization_detailed$monetized_impact |> round(digits = 2),
    expect =
      c(800, 952.38,1088.44, 1295.76, 1480.86, 1567.05) # Results on 2025-03-04; Excel sheet of Uni Porto
  )
})

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_hyp_harvey|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = c(800, 1000, 1200, 1500, 1800, 2000),
        discount_shape = "hyperbolic_harvey_1986",
        discount_rate = 0.05,
        discount_years = 5,
        valuation = 1
        )$monetization_detailed$monetized_impact |> round(digits = 2),
    expect =
      c(800,965.94, 1135.86, 1399.55, 1660.83, 1828.62) # Results on 2025-04-15; Excel sheet of Uni Porto
  )
})

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_hyp_mazur|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = c(800, 1000, 1200, 1500, 1800, 2000),
        discount_shape = "hyperbolic_mazur_1987",
        discount_rate = 0.05,
        discount_years = 5,
        valuation = 1
        )$monetization_detailed$monetized_impact |> round(digits = 2),
    expect =
      c(800, 952.38, 1090.91, 1304.35, 1500.00, 1600.00) # Results on 2025-04-15; Excel sheet of Uni Porto
  )
})

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_exponential|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 2E4,
        discount_shape = "exponential",
        discount_rate = 0.03,
        discount_years = 20,
        valuation = 1
      )$monetization_main$monetized_impact_rounded,
    expect = c(11074) # Result on 2024-03-10; from ChatGPT
  )
})

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_exponential|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 50,
        discount_shape = "exponential",
        discount_rate = 0.03,
        discount_years = 5,
        valuation = 20
      )$monetization_main$monetized_impact_rounded,
    expect = c(863) # Excel file from University of Porto "WP2_Examples.xlsx"
  )
})

testthat::test_that("results the same |fake_monetization|discount_rate_TRUE|discount_shape_exponential|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::discount(
        impact = 2E4,
        discount_shape = "exponential",
        discount_rate = 0.03,
        discount_years = 20
        )$monetization_main$monetized_impact_rounded,
    expect = 11074 # Result on 15 Jan 2025 ; no comparison study
  )
})

#### INFLATION ##################################################################

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_exponential|inflation_TRUE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 1,
        discount_shape = "exponential",
        discount_rate = 0.05,
        discount_years = 5,
        inflation = 0.08,
        valuation = 1E3
        )$monetization_main$monetized_impact |> base::round(digits = 2),
    expect =
      783.53 # Results on 2025-03-04;  Excel sheet of Uni Porto
  )

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 1,
        discount_shape = "exponential",
        discount_rate = 0.04,
        discount_years = 5,
        inflation = 0.03,
        valuation = 1E4
        )$monetization_main$monetized_impact_rounded,
    expect =
      8219 # Results on 2025-03-10; ChatGPT
  )

})

## HEALTHIAR INPUT ##############################################################

### NO DISCOUNTING ##############################################################

### DISCOUNTING #################################################################
testthat::test_that("results the same |fake_monetization|discount_rate_TRUE|discount_shape_exponential|inflation_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_deaths_yll.rds"))
  data_mort <- base::readRDS(testthat::test_path("data", "input_data_mortality.rds"))
  data_lifetable <- base::readRDS(testthat::test_path("data", "lifetable_withPopulation.rds"))

  bestcost_pm_yll_exposure_single_year_lifetable_geluft <-
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      approach_exposure = "single_year",
      exp_central = data_mort$exp[2], #exp CH 2019
      prop_pop_exp = 1,
      cutoff_central = data_mort$cutoff[2], # WHO AQG 2021
      rr_central = data_mort[2,"rr_central"],
      rr_lower = data_mort[2,"rr_lower"],
      rr_upper =data_mort[2,"rr_upper"],
      rr_increment = 10,
      erf_shape = "log_linear",
      first_age_pop = 0,
      last_age_pop = 99,
      population_midyear_male = data_lifetable[["male"]]$population,
      population_midyear_female = data_lifetable[["female"]]$population,
      deaths_male = data[["pop"]]$number_of_deaths_male,
      deaths_female = data[["pop"]]$number_of_deaths_female,
      year_of_analysis = 2019,
      info = data_mort$pollutant[2],
      min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2])

  testthat::expect_equal(
    object =
      healthiar::monetize(
        output_attribute = bestcost_pm_yll_exposure_single_year_lifetable_geluft,
        discount_shape = "exponential",
        discount_rate = 0.01,
        valuation = 1
      )$monetization_main$monetized_impact_rounded,
    expect = c(26493, 13877, 39006) # Result on 13 Dec 2024 ; no comparison study
  )
})

testthat::test_that("results the same |fake_monetization|discount_rate_TRUE|discount_shape_exponential|inflation_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_copd <-
    healthiar::attribute_health(
      exp_central = data$mean_concentration,
      cutoff_central = data$cut_off_value,
      bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
      rr_central = data$relative_risk,
      rr_lower = data$relative_risk_lower,
      rr_upper = data$relative_risk_upper,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = paste0(data$pollutant,"_", data$evaluation_name))

  testthat::expect_equal(
    object =
      healthiar::monetize(
        output_attribute = bestcost_pm_copd,
        discount_shape = "exponential",
        discount_rate = 0.03,
        discount_years = 5,
        valuation = 20
      )$monetization_main$monetized_impact_rounded,
    expect = c(60416, 23343, 94436) # Result on 9 Jan 2025 ; no comparison study
  )
})

#### WITH INFLATION #############################################################

testthat::test_that("results the same |pathway_monetization|discount_rate_TRUE|discount_shape_exponential|inflation_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_copd <- healthiar::attribute_health(
    exp_central = data$mean_concentration,
    exp_lower = data$mean_concentration-0.5,
    exp_upper = data$mean_concentration+0.5,
    cutoff_central = data$cut_off_value,
    bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
    rr_central = data$relative_risk,
    rr_lower = data$relative_risk_lower,
    rr_upper = data$relative_risk_upper,
    rr_increment = 10,
    erf_shape = "log_linear",
    info = paste0(data$pollutant,"_", data$evaluation_name))

  testthat::expect_equal(
    object = healthiar::monetize(
      output_attribute = bestcost_pm_copd,
      discount_shape = "exponential",
      discount_rate = 0.05,
      discount_years = 5,
      inflation = 0.08,
      valuation = 1E3
      )$monetization_main$monetized_impact_rounded,
    expect =
      c(2743879, 1060162, 4288935) # Results on 2025-04-15; no comparison study
  )
})

# ERROR OR WARNING ########

## ERROR #########


testthat::test_that("error if negative valuation", {

  testthat::expect_error(
    object =
      healthiar::monetize(
        impact = c(800, 1000, 1200, 1500, 1800, 2000),
        discount_shape = "exponential",
        discount_rate = 0.05,
        discount_years = 5,
        valuation = -10
      ),
    "valuation must be higher than 0."
  )
})

testthat::test_that("error if negative discount_years", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_copd <- healthiar::attribute_health(
    exp_central = data$mean_concentration,
    exp_lower = data$mean_concentration-0.5,
    exp_upper = data$mean_concentration+0.5,
    cutoff_central = data$cut_off_value,
    bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
    rr_central = data$relative_risk,
    rr_lower = data$relative_risk_lower,
    rr_upper = data$relative_risk_upper,
    rr_increment = 10,
    erf_shape = "log_linear",
    info = paste0(data$pollutant,"_", data$evaluation_name))

  testthat::expect_error(
    object =
      healthiar::monetize(
        output_attribute = bestcost_pm_copd,
        discount_shape = "exponential",
        discount_rate = 0.05,
        discount_years = -5,
        inflation = 0.08,
        valuation = 1E3
      ),
    "discount_years must be higher than 0."
  )
})

testthat::test_that("error if discount_rate higher than 1", {

  testthat::expect_error(
    object =
      healthiar::monetize(
        impact = c(800, 1000, 1200, 1500, 1800, 2000),
        discount_shape = "exponential",
        discount_rate = 1.5,
        discount_years = 5,
        valuation = 10
      ),
    "discount_rate must be higher than 0 and lower than 1."
  )
})

testthat::test_that("error if inflation higher than 1", {

  testthat::expect_error(
    object =
      healthiar::monetize(
        impact = c(800, 1000, 1200, 1500, 1800, 2000),
        discount_shape = "exponential",
        discount_rate = 0.05,
        discount_years = 5,
        valuation = 10,
        inflation = 1.15
      ),
    "inflation must be higher than 0 and lower than 1."
  )
})

testthat::test_that("error if both impact and output_attribute are entered", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_copd <- healthiar::attribute_health(
    exp_central = data$mean_concentration,
    exp_lower = data$mean_concentration-0.5,
    exp_upper = data$mean_concentration+0.5,
    cutoff_central = data$cut_off_value,
    bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
    rr_central = data$relative_risk,
    rr_lower = data$relative_risk_lower,
    rr_upper = data$relative_risk_upper,
    rr_increment = 10,
    erf_shape = "log_linear",
    info = paste0(data$pollutant,"_", data$evaluation_name))

  testthat::expect_error(
    object =
      healthiar::monetize(
        impact = 1000,
        output_attribute = bestcost_pm_copd,
        discount_shape = "exponential",
        discount_rate = 0.05,
        discount_years = 5,
        inflation = 0.08,
        valuation = 1E3
      ),
    "Enter a value for impact or for output_attribute but not both."
  )
})

testthat::test_that("error if no right category", {

  testthat::expect_error(
    object =
      healthiar::monetize(
        impact = c(800, 1000, 1200, 1500, 1800, 2000),
        discount_shape = "exp",
        discount_rate = 0.05,
        valuation = 10
      ),
    "Please, check spelling. discount_shape must have one of this values: exponential, hyperbolic_harvey_1986, hyperbolic_mazur_1987"
  )
})

## WARNING #########

testthat::test_that("warning if no discount year but other discount arguments", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  bestcost_pm_copd <- healthiar::attribute_health(
    exp_central = data$mean_concentration,
    exp_lower = data$mean_concentration-0.5,
    exp_upper = data$mean_concentration+0.5,
    cutoff_central = data$cut_off_value,
    bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
    rr_central = data$relative_risk,
    rr_lower = data$relative_risk_lower,
    rr_upper = data$relative_risk_upper,
    rr_increment = 10,
    erf_shape = "log_linear",
    info = paste0(data$pollutant,"_", data$evaluation_name))

  testthat::expect_warning(
    object =
      healthiar::monetize(
        output_attribute = bestcost_pm_copd,
        discount_shape = "exponential",
        discount_rate = 0.05,
        inflation = 0.08,
        valuation = 1E3
      ),
    base::paste0("You entered some value in arguments for discount,",
                 " but discount_year is 0 (default value).\n",
                 "Therefore no discount is applied."),
    # To match the messages fixed  = TRUE.
    # Otherwise, for some reason, testthat does not recognize the same text
    fixed = TRUE
  )
})


testthat::test_that("warning if user pass discount_years with impact", {

  testthat::expect_warning(
    object =
      healthiar::monetize(
        impact = c(800, 1000, 1200, 1500, 1800, 2000),
        discount_shape = "exponential",
        discount_rate = 0.05,
        discount_years = 5,
        valuation = 10
      ),
    base::paste0("discount_years (aimed for output_attribute) will be here ignored,\n",
                 "because the length of impact is used."),
    # To match the messages fixed  = TRUE.
    # Otherwise, for some reason, testthat does not recognize the same text
    fixed = TRUE
  )
})
