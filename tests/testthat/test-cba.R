# QUANTITATIVE TEST ############################################################

## RAW INPUT ###################################################################

### NO DISCOUNTING #############################################################

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_FALSE|discount_rate_cost_FALSE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        n_years_benefit = 10,
        n_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect =
      2511800000 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of ozone reduction policy on premature mortality in China
  ## Example adapted from Chen et al (2015) data to 10 years policy with no discount rate
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_shape = "exponential",
        n_years_benefit = 10,
        n_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect =
      106054000000 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of PM2.5 reduction policy on premature mortality in China with no discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015)
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})


### DISCOUNTING ################################################################

#### EXPONENTIAL ###############################################################

##### BENEFIT & COST DISCOUNTED ################################################

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      healthiar::cba(
        positive_impact = 50,
        valuation = 20,
        cost = 100,
        discount_shape = "exponential",
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        n_years_benefit = 5,
        n_years_cost = 5
        )$cba_main$net_benefit_rounded,
    expect = 776 # Excel file from University of Porto "WP2_Examples.xlsx"
  )
})

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_shape = "exponential",
        n_years_benefit = 10,
        n_years_cost = 10
        )$cba_main$net_benefit_rounded,
    expect =
      1869015095 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of ozone reduction policy on premature mortality in China with exponential discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and exponential function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_shape = "exponential",
        n_years_benefit = 10,
        n_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect =
      78914136050 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of PM2.5 reduction policy on premature mortality in China with exponential discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and exponential function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

##### ONLY BENEFIT DISCOUNTED ##################################################

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_FALSE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_benefit = 0.03,
        discount_shape = "exponential",
        n_years_benefit = 10,
      )$cba_main$net_benefit_rounded,
    expect =
      -229414802 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of ozone reduction policy on premature mortality in China with exponential discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and exponential function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_FALSE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_rate_benefit = 0.03,
        discount_shape = "exponential",
        n_years_benefit = 10,
      )$cba_main$net_benefit_rounded,
    expect =
      78780297168 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of PM2.5 reduction policy on premature mortality in China with exponential discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and exponential function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})


##### ONLY COST DISCOUNTED #####################################################

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_FALSE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_cost = 0.03,
        discount_shape = "exponential",
        n_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect =
      4610229898 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of ozone reduction policy on premature mortality in China with exponential discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and exponential function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_FALSE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_rate_cost = 0.03,
        discount_shape = "exponential",
        n_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect =
      106187838883 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of PM2.5 reduction policy on premature mortality in China with exponential discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and exponential function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

#### HARVEY ####################################################################

##### BENEFIT & COST DISCOUNTED ################################################

testthat::test_that("results correct |pathway_cba|discount_shape_harvey|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_shape = "hyperbolic_harvey_1986",
        n_years_benefit = 10,
        n_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect =
      2337455091 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of ozone reduction policy on premature mortality in China with hyperbolic (Harvey) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Harvey) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_cba|discount_shape_harvey|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_shape = "hyperbolic_harvey_1986",
        n_years_benefit = 10,
        n_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect =
      98692755067 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of PM2.5 reduction policy on premature mortality in China with hyperbolic (Harvey) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Harvey) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

##### ONLY BENEFIT DISCOUNTED ##################################################

testthat::test_that("results correct |pathway_cba|discount_shape_harvey|discount_rate_benefit_TRUE|discount_rate_cost_FALSE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_benefit = 0.03,
        discount_shape = "hyperbolic_harvey_1986",
        n_years_benefit = 10,
      )$cba_main$net_benefit_rounded,
    expect =
      1768290246 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of ozone reduction policy on premature mortality in China with hyperbolic (Harvey) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Harvey) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_cba|discount_shape_harvey|discount_rate_benefit_TRUE|discount_rate_cost_FALSE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_rate_benefit = 0.03,
        discount_shape = "hyperbolic_harvey_1986",
        n_years_benefit = 10,
      )$cba_main$net_benefit_rounded,
    expect =
      98656453456 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of PM2.5 reduction policy on premature mortality in China with hyperbolic (Harvey) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Harvey) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

##### ONLY COST DISCOUNTED #####################################################

testthat::test_that("results correct |pathway_cba|discount_shape_harvey|discount_rate_benefit_FALSE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_cost = 0.03,
        discount_shape = "hyperbolic_harvey_1986",
        n_years_cost = 10,
      )$cba_main$net_benefit_rounded,
    expect =
      3080964845 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of ozone reduction policy on premature mortality in China with hyperbolic (Harvey) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Harvey) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_cba|discount_shape_harvey|discount_rate_benefit_FALSE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_rate_cost = 0.03,
        discount_shape = "hyperbolic_harvey_1986",
        n_years_cost = 10,
      )$cba_main$net_benefit_rounded,
    expect =
      106090301611 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of PM2.5 reduction policy on premature mortality in China with hyperbolic (Harvey) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Harvey) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

#### MAZUR #####################################################################

##### BENEFIT & COST DISCOUNTED ################################################

testthat::test_that("results correct |pathway_cba|discount_shape_mazur|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_shape = "hyperbolic_mazur_1987",
        n_years_benefit = 10,
        n_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect =
      1932153846 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of ozone reduction policy on premature mortality in China with hyperbolic (Mazur) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Mazur) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_cba|discount_shape_mazur|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_shape = "hyperbolic_mazur_1987",
        n_years_benefit = 10,
        n_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect =
      81580000000 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of PM2.5 reduction policy on premature mortality in China with hyperbolic (Mazur) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Mazur) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

##### ONLY BENEFIT DISCOUNTED ##################################################

testthat::test_that("results correct |pathway_cba|discount_shape_mazur|discount_rate_benefit_TRUE|discount_rate_cost_FALSE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_benefit = 0.03,
        discount_shape = "hyperbolic_mazur_1987",
        n_years_benefit = 10,
      )$cba_main$net_benefit_rounded,
    expect =
      39846154 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of ozone reduction policy on premature mortality in China with hyperbolic (Mazur) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Mazur) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_cba|discount_shape_mazur|discount_rate_benefit_TRUE|discount_rate_cost_FALSE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_rate_benefit = 0.03,
        discount_shape = "hyperbolic_mazur_1987",
        n_years_benefit = 10,
      )$cba_main$net_benefit_rounded,
    expect =
      81459307692 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of PM2.5 reduction policy on premature mortality in China with hyperbolic (Mazur) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Mazur) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

##### ONLY COST DISCOUNTED #####################################################

testthat::test_that("results correct |pathway_cba|discount_shape_mazur|discount_rate_benefit_FALSE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_cost = 0.03,
        discount_shape = "hyperbolic_mazur_1987",
        n_years_cost = 10,
      )$cba_main$net_benefit_rounded,
    expect =
      4404107692 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of ozone reduction policy on premature mortality in China with hyperbolic (Mazur) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Mazur) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

testthat::test_that("results correct |pathway_cba|discount_shape_mazur|discount_rate_benefit_FALSE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_rate_cost = 0.03,
        discount_shape = "hyperbolic_mazur_1987",
        n_years_cost = 10,
      )$cba_main$net_benefit_rounded,
    expect =
      106174692308 # benefit year 10 - cost year 10
  )
  ## ASSESSOR:
  ## Iracy Pimenta
  ## ASSESSMENT DETAILS:
  ## CBA of PM2.5 reduction policy on premature mortality in China with hyperbolic (Mazur) discount rate
  ## INPUT DATA DETAILS:
  ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Mazur) function
  ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
  ## DOI: https://doi.org/10.1007/s11270-015-2316-7
})

## HEALTHIAR INPUT #############################################################

testthat::test_that("results the same |fake_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

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
      healthiar::cba(
        output_attribute = bestcost_pm_copd,
        valuation = 20,
        cost = 100,
        discount_shape = "exponential",
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        n_years_benefit = 5,
        n_years_cost = 5
        )$cba_main$net_benefit_rounded,
    expect = c(60330,23257,94350) # Results on 2025-03-06; no comparison study
  )
})

testthat::test_that("results the same |fake_cba|discount_shape_exp|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

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
      healthiar::cba(
        output_attribute = bestcost_pm_copd,
        valuation = 20,
        cost = 100,
        discount_shape = "exponential",
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        n_years_benefit = 5,
        n_years_cost = 5
        )$cba_main$net_benefit_rounded,
    expect = c(60416, 23343, 94436) - 86 # Results on 2025-02-05 ; no comparison study
  )
})

# ERROR OR WARNING ########
## ERROR #########

## WARNING #########



