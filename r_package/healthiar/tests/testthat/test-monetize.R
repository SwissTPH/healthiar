# QUANTITATIVE TEST ############################################################
## RAW INPUT ###################################################################

### NO DISCOUNTING #############################################################

#### EXPONENTIAL ###############################################################

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
        )$monetization_main$monetized_impact_rounded,
    expect = # 1000 * airqplus_pm_copd
      round(1000 * bestcost_pm_copd[["health_main"]]$impact)
  )
})

testthat::test_that("results correct |pathway_monetization|discount_rate_FALSE|discount_shape_exponential|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 19800,
        valuation = 541000,
        discount_shape = "exponential",
        discount_years = 5
      )$monetization_main$monetized_impact_rounded,
    expect =
      10711800000
  )

## RESULT(S) COMPARISON ASSESSMENT:
## 10711.8 million $
## ASSESSOR:
## Iracy Pimenta
## ASSESSMENT DETAILS:
## Monetization of Ozone reduction policy on premature mortality in China
## INPUT DATA DETAILS:
## Example adapted from Chen et al (2015) data to 5 years policy, with no discount rate
## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
## DOI: https://doi.org/10.1007/s11270-015-2316-7

})

testthat::test_that("results correct |pathway_monetization|discount_rate_FALSE|discount_shape_exponential|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 197000,
        valuation = 541000,
        discount_shape = "exponential",
        discount_years = 5
      )$monetization_main$monetized_impact_rounded,
    expect =
      106577000000
  )

  ## RESULT(S) COMPARISON ASSESSMENT:
  ## 106577 million $
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## Monetization of PM2.5 reduction policy on premature mortality in China
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 5 years policy, with no discount rate
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

#### HARVEY ####################################################################

testthat::test_that("results correct |pathway_monetization|discount_rate_FALSE|discount_shape_hyp_harvey|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 197000,
        valuation = 541000,
        discount_shape = "hyperbolic_harvey_1986",
        discount_years = 5
      )$monetization_main$monetized_impact_rounded,
    expect =
      106577000000
  )

  ## RESULT(S) COMPARISON ASSESSMENT:
  ## 106577 million $
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## Monetization of PM2.5 reduction policy on premature mortality in China
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 5 years policy, with no discount rate
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_monetization|discount_rate_FALSE|discount_shape_hyp_harvey|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 19800,
        valuation = 541000,
        discount_shape = "hyperbolic_harvey_1986",
        discount_years = 5
      )$monetization_main$monetized_impact_rounded,
    expect =
      10711800000
  )

  ## RESULT(S) COMPARISON ASSESSMENT:
  ## 10711.8 million $
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## Monetization of ozone reduction policy on premature mortality in China
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 5 years policy, with no discount rate
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

#### MAZURE ####################################################################

testthat::test_that("results correct |pathway_monetization|discount_rate_FALSE|discount_shape_hyp_mazur|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 19800,
        valuation = 541000,
        discount_shape = "hyperbolic_mazur_1987",
        discount_years = 5
      )$monetization_main$monetized_impact_rounded,
    expect =
      10711800000
  )

  ## RESULT(S) COMPARISON ASSESSMENT:
  ## 10711.8 million $
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## Monetization of Ozone reduction policy on premature mortality in China
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 5 years policy, with no discount rate
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_monetization|discount_rate_FALSE|discount_shape_hyp_mazur|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 197000,
        valuation = 541000,
        discount_shape = "hyperbolic_mazur_1987",
        discount_years = 5
      )$monetization_main$monetized_impact_rounded,
    expect =
      106577000000
  )

  ## RESULT(S) COMPARISON ASSESSMENT:
  ## 106577 million $
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## Monetization of Ozone reduction policy on premature mortality in China
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 5 years policy, with no discount rate
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})




### DISCOUNTING ################################################################

#### EXPONENTIAL ###############################################################

##### NO INFLATION #############################################################

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
        valuation = 1,
        info = base::data.frame(year = c(2020:2025))
        )$monetization_detailed$monetized_impact |> round(digits = 2),
    expect =
      c(800, 952.38,1088.44, 1295.76, 1480.86, 1567.05) # Results on 2025-03-04; Excel sheet of Uni Porto
  )
})

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_exponential|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 19800,
        valuation = 541000,
        discount_rate = 0.03,
        discount_shape = "exponential",
        discount_years = 5
      )$monetization_main$monetized_impact_rounded,
    expect =
      9240092777
  )
    ## RESULT(S) COMPARISON ASSESSMENT:
    ## 9240092777 $ (5 years)
    ## ASSESSOR:
    ## Iracy Pimenta
    ## ASSESSMENT DETAILS:
    ## Monetization of Ozone reduction policy on premature mortality in China with exponential discount rate
    ## INPUT DATA DETAILS:
    ## Example adapted from Chen et al (2015) data to 5 years policy, with discount rate = 0,03 and exponential function
    ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
    ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_exponential|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 197000,
        valuation = 541000,
        discount_rate = 0.03,
        discount_shape = "exponential",
        discount_years = 5
      )$monetization_main$monetized_impact_rounded,
    expect =
      91934256413
  )
  ## RESULT(S) COMPARISON ASSESSMENT:
  ## 91934256413 $ (5 years)
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## Monetization of PM2.5 reduction policy on premature mortality in China with exponential discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 5 years policy, with discount rate = 0,03 and exponential function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
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

##### INFLATION ################################################################

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
})

testthat::test_that("results the same |pathway_monetization|discount_rate_TRUE|discount_shape_exponential|inflation_TRUE|", {
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

#### HARVEY ####################################################################

##### NO INFLATION #############################################################

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_hyp_harvey|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = c(800, 1000, 1200, 1500, 1800, 2000),
        discount_shape = "hyperbolic_harvey_1986",
        discount_rate = 0.05,
        valuation = 1
        )$monetization_detailed$monetized_impact |> round(digits = 2),
    expect =
      c(800,965.94, 1135.86, 1399.55, 1660.83, 1828.62) # Results on 2025-04-15; Excel sheet of Uni Porto
  )
})

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_hyp_harvey|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 19800,
        valuation = 541000,
        discount_rate = 0.03,
        discount_shape = "hyperbolic_harvey_1986",
        discount_years = 5
      )$monetization_main$monetized_impact,
    expect =
      10151212470
  )
  ## RESULT(S) COMPARISON ASSESSMENT:
  ## 10151212470 million $ (5 years)
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## Monetization of Ozone reduction policy on premature mortality in China with hyperbolic (Harvey) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 5 years policy, with discount rate = 0,03 and hyperbolic (Harvey) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_hyp_harvey|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = 197000,
        valuation = 541000,
        discount_rate = 0.03,
        discount_shape = "hyperbolic_harvey_1986",
        discount_years = 5
      )$monetization_main$monetized_impact,
    expect =
      100999437198
  )
  ## RESULT(S) COMPARISON ASSESSMENT:
  ## 100999437198 million $ (5 years)
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## Moneztization of PM2.5 reduction policy on premature mortality in China with hyperbolic (Harvey) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 5 years policy, with discount rate = 0,03 and hyperbolic (Harvey) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})


##### INFLATION ################################################################

#### MAZUR #####################################################################

##### NO INFLATION #############################################################

testthat::test_that("results correct |pathway_monetization|discount_rate_TRUE|discount_shape_hyp_mazur|inflation_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::monetize(
        impact = c(800, 1000, 1200, 1500, 1800, 2000),
        discount_shape = "hyperbolic_mazur_1987",
        discount_rate = 0.05,
        valuation = 1
        )$monetization_detailed$monetized_impact |> round(digits = 2),
    expect =
      c(800, 952.38, 1090.91, 1304.35, 1500.00, 1600.00) # Results on 2025-04-15; Excel sheet of Uni Porto
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
      min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2])

  testthat::expect_equal(
    object =
      healthiar::monetize(
        output_attribute = bestcost_pm_yll_exposure_single_year_lifetable_geluft,
        discount_shape = "exponential",
        discount_rate = 0.01,
        valuation = 1
      )$monetization_main$monetized_impact_rounded,
    expect = c(26101, 13670, 38432) # Result on 9 July 2025 ; no comparison study
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
    regexp = "valuation must be higher than 0."
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
    regexp = "discount_years must be higher than 0."
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
    regexp = "discount_rate must be higher than 0 and lower than 1."
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
    regexp = "inflation must be higher than 0 and lower than 1."
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
    regexp = "Enter a value for impact or for output_attribute but not both."
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
    regexp = "Please, check spelling. discount_shape must have one of this values: exponential, hyperbolic_harvey_1986, hyperbolic_mazur_1987."
  )
})

testthat::test_that("error if incompatible size of info", {

  testthat::expect_error(
    object =
      healthiar::monetize(
        impact = c(800, 1000, 1200, 1500, 1800, 2000),
        discount_shape = "exponential",
        discount_rate = 0.05,
        valuation = 10,
        info = data.frame(id = 1:20)
      ),
    regexp = "The info vector or data frame columns must have a length of 1 or the same length as impact."
  )
})

## WARNING #########

testthat::test_that("warning if no discount_rate but other discount arguments", {

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
    regexp = base::paste0("You entered some value in discount_rate,",
                          " but discount_year is 0 (default value).",
                          " Therefore no discount is applied."),
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
    regexp = base::paste0(
      "discount_years is aimed for output_attribute (excluding life table)",
      " and for impact (excluding vector form).",
      " Therefore discount_years is ignored here and the length of the vector impact is used instead."),
    # To match the messages fixed  = TRUE.
    # Otherwise, for some reason, testthat does not recognize the same text
    fixed = TRUE
  )
})

testthat::test_that("warning if user pass discount_years with life table", {
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
      min_age = if(is.na(data_mort$min_age[2])) NULL else data_mort$min_age[2])

  testthat::expect_warning(
    object =
      healthiar::monetize(
        output_attribute = bestcost_pm_yll_exposure_single_year_lifetable_geluft,
        discount_shape = "exponential",
        discount_rate = 0.01,
        discount_years = 5,
        valuation = 1,
      ),
    regexp = base::paste0(
      "discount_years is aimed for any output_attribute and for impact with single value (no vector).",
      " Therefore discount_years is ignored here and the length life table is used instead."),
    # To match the messages fixed  = TRUE.
    # Otherwise, for some reason, testthat does not recognize the same text
    fixed = TRUE
  )
})


