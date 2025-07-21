# QUANTITATIVE TEST ############################################################

## RAW INPUT ###################################################################

### NO DISCOUNTING #############################################################

#### EXPONENTIAL ###############################################################

#### HARVEY ###############################################################

#### MAZUR ###############################################################

### DISCOUNTING ################################################################

#### EXPONENTIAL ###############################################################

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
        discount_years_benefit = 5,
        discount_years_cost = 5
        )$cba_main$net_benefit_rounded,
    expect = 776 # Excel file from University of Porto "WP2_Examples.xlsx"
  )
})

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =

      ## Original
      # CBA_ozone_exp <- cba(
      #   positive_impact = 19800,
      #   valuation = 541000,
      #   cost = 8200,
      #   discount_rate_benefit = 0.3,
      #   discount_rate_cost = 0.3,
      #   discount_shape = "exponential",
      #   discount_years_benefit = 10,
      #   discount_years_cost = 10
      # )
      ## RESULT(S) COMPARISON ASSESSMENT:
      ## Total benefit:	91373.83 million $ / total cost:	69947.66 million $ / Net benefit: 21426,16 million $
      ## ROI: 0.31	/ CBR: 1.31
      ## ASSESSOR:
      ## Iracy Pimenta
      ## ASSESSMENT DETAILS:
      ## CBA of ozone reduction policy on premature mortality in China with exponential discount rate
      ## INPUT DATA DETAILS:
      ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and exponential function
      ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
      ## DOI: https://doi.org/10.1007/s11270-015-2316-7

      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_shape = "exponential",
        discount_years_benefit = 10,
        discount_years_cost = 10
        )$cba_main$net_benefit_rounded,
    expect = 1869015095 # benefit year 10 - cost year 10
  )
})

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =

      ## Original
      # healthiar::cba(
      #   positive_impact = 197000,
      #   valuation = 541000,
      #   cost = 523,
      #   discount_rate_benefit = 0.3,
      #   discount_rate_cost = 0.3,
      #   discount_shape = "exponential",
      #   discount_years_benefit = 10,
      #   discount_years_cost = 10
      # )
      ## RESULT(S) COMPARISON ASSESSMENT:
      ## Total benefit:	909123.43 million $ / total cost:	4461.30 million $ / Net benefit: 904662.13 million $
      ## ROI: 202.78	/ CBR: 203.78
      ## ASSESSOR:
      ## Iracy Pimenta
      ## ASSESSMENT DETAILS:
      ## CBA of PM2.5 reduction policy on premature mortality in China with exponential discount rate
      ## INPUT DATA DETAILS:
      ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and exponential function
      ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
      ## DOI: https://doi.org/10.1007/s11270-015-2316-7

      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_shape = "exponential",
        discount_years_benefit = 10,
        discount_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect = 78914136050 # benefit year 10 - cost year 10
  )
})

#### HARVEY ####################################################################

testthat::test_that("results correct |pathway_cba|discount_shape_harvey|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =

      ## Original
      # healthiar::cba(
      #   positive_impact = 19800,
      #   valuation = 541000,
      #   cost = 8200,
      #   discount_rate_benefit = 0.3,
      #   discount_rate_cost = 0.3,
      #   discount_shape = "hyperbolic_harvey_1986",
      #   discount_years_benefit = 10,
      #   discount_years_cost = 10
      # )
      ## RESULT(S) COMPARISON ASSESSMENT:
      ## Total benefit:	101651.37 million $ / total cost:	77815.24 million $ / Net benefit: 23836.14 million $
      ## ROI: 0.31	/ CBR: 1.31
      ## ASSESSOR:
      ## Iracy Pimenta
      ## ASSESSMENT DETAILS:
      ## CBA of ozone reduction policy on premature mortality in China with hyperbolic (Harvey) discount rate
      ## INPUT DATA DETAILS:
      ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Harvey) function
      ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality

      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_shape = "hyperbolic_harvey_1986",
        discount_years_benefit = 10,
        discount_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect = 2337455091 # benefit year 10 - cost year 10
  )
})

testthat::test_that("results correct |pathway_cba|discount_shape_harvey|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =

      ## Original
      # healthiar::cba(
      #   positive_impact = 197000,
      #   valuation = 541000,
      #   cost = 523,
      #   discount_rate_benefit = 0.03,
      #   discount_rate_cost = 0.03,
      #   discount_shape = "harvey",
      #   discount_years_benefit = 10,
      #   discount_years_cost = 10
      # )
      ## RESULT(S) COMPARISON ASSESSMENT:
      ## Total benefit:	909123.43 million $ / total cost:	4461.30 million $ / Net benefit: 904662.13 million $
      ## ROI: 202.78	/ CBR: 203.78
      ## ASSESSOR:
      ## Iracy Pimenta
      ## ASSESSMENT DETAILS:
      ## CBA of PM2.5 reduction policy on premature mortality in China with exponential discount rate
      ## INPUT DATA DETAILS:
      ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and exponential function
      ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
      ## DOI: https://doi.org/10.1007/s11270-015-2316-7

      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_shape = "hyperbolic_harvey_1986",
        discount_years_benefit = 10,
        discount_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect = 98692755067 # benefit year 10 - cost year 10
  )
})

#### MAZUR #####################################################################

testthat::test_that("results correct |pathway_cba|discount_shape_mazur|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =

      ## Original
      # healthiar::cba(
      #   positive_impact = 19800,
      #   valuation = 541000,
      #   cost = 8200,
      #   discount_rate_benefit = 0.03,
      #   discount_rate_cost = 0.03,
      #   discount_shape = "hyperbolic_mazur_1987",
      #   discount_years_benefit = 10,
      #   discount_years_cost = 10
      # )
      ## RESULT(S) COMPARISON ASSESSMENT:
      ## Total benefit:	101651.37 million $ / total cost:	77815.24 million $ / Net benefit: 23836.14 million $
      ## ROI: 0.31	/ CBR: 1.31
      ## ASSESSOR:
      ## Iracy Pimenta
      ## ASSESSMENT DETAILS:
      ## CBA of ozone reduction policy on premature mortality in China with hyperbolic (Harvey) discount rate
      ## INPUT DATA DETAILS:
      ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and hyperbolic (Harvey) function
      ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality

      ## Adapted
      healthiar::cba(
        positive_impact = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_shape = "hyperbolic_mazur_1987",
        discount_years_benefit = 10,
        discount_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect = 1932153846 # benefit year 10 - cost year 10
  )
})

testthat::test_that("results correct |pathway_cba|discount_shape_mazur|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =

      ## Original
      # healthiar::cba(
      #   positive_impact = 197000,
      #   valuation = 541000,
      #   cost = 523,
      #   discount_rate_benefit = 0.3,
      #   discount_rate_cost = 0.3,
      #   discount_shape = "hyperbolic_mazur_1987",
      #   discount_years_benefit = 10,
      #   discount_years_cost = 10
      # )
      ## RESULT(S) COMPARISON ASSESSMENT:
      ## Total benefit:	909123.43 million $ / total cost:	4461.30 million $ / Net benefit: 904662.13 million $
      ## ROI: 202.78	/ CBR: 203.78
      ## ASSESSOR:
      ## Iracy Pimenta
      ## ASSESSMENT DETAILS:
      ## CBA of PM2.5 reduction policy on premature mortality in China with exponential discount rate
      ## INPUT DATA DETAILS:
      ## Example adapted from Chen et al (2015) data to 10 years policy, with discount rate = 0,03 and exponential function
      ## Paper title: Cost–Benefit Analysis of Reducing Premature Mortality
      ## DOI: https://doi.org/10.1007/s11270-015-2316-7

      ## Adapted
      healthiar::cba(
        positive_impact = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        discount_shape = "hyperbolic_mazur_1987",
        discount_years_benefit = 10,
        discount_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect = 81580000000 # benefit year 10 - cost year 10
  )
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
        discount_years_benefit = 5,
        discount_years_cost = 5
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
        discount_years_benefit = 5,
        discount_years_cost = 5
        )$cba_main$net_benefit_rounded,
    expect = c(60416, 23343, 94436) - 86 # Results on 2025-02-05 ; no comparison study
  )
})

# ERROR OR WARNING ########
## ERROR #########

## WARNING #########



