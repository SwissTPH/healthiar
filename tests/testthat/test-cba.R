# QUANTITATIVE TEST ############################################################

## RAW INPUT ###################################################################

### WITHOUT DISCOUNT #############################################################

#### WITHOUT INFLATION #############################################################

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_FALSE|discount_rate_cost_FALSE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 19800,
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
        impact_benefit = 197000,
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

#### WITH INFLATION AND GROWTH #############################################################

testthat::test_that("results the same |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_shape = "exponential",
        inflation_rate = 0.05,
        n_years_benefit = 10,
        n_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect =
      65107956191 # Results on 2026-03-02
  )
})

testthat::test_that("results the same |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 197000,
        valuation = 541000,
        cost = 523000000,
        discount_shape = "exponential",
        inflation_rate = 0.05,
        real_growth_rate = 0.05,
        n_years_benefit = 10,
        n_years_cost = 10
      )$cba_main$net_benefit_rounded,
    expect =
      106054000000 # Results on 2026-03-02
  )
})

### WITH DISCOUNT ################################################################

#### WITHOUT INFLATION #############################################################

##### EXPONENTIAL ###############################################################

###### BENEFIT & COST DISCOUNTED ################################################

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      healthiar::cba(
        impact_benefit = 50,
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
        impact_benefit = 19800,
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
        impact_benefit = 197000,
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

###### ONLY BENEFIT DISCOUNTED ##################################################

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_FALSE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 19800,
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
        impact_benefit = 197000,
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


###### ONLY COST DISCOUNTED #####################################################

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_FALSE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 19800,
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
        impact_benefit = 197000,
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

##### HARVEY ####################################################################

###### BENEFIT & COST DISCOUNTED ################################################

testthat::test_that("results correct |pathway_cba|discount_shape_harvey|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 19800,
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
        impact_benefit = 197000,
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

###### ONLY BENEFIT DISCOUNTED ##################################################

testthat::test_that("results correct |pathway_cba|discount_shape_harvey|discount_rate_benefit_TRUE|discount_rate_cost_FALSE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 19800,
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
        impact_benefit = 197000,
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

###### ONLY COST DISCOUNTED #####################################################

testthat::test_that("results correct |pathway_cba|discount_shape_harvey|discount_rate_benefit_FALSE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 19800,
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
        impact_benefit = 197000,
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

##### MAZUR #####################################################################

###### BENEFIT & COST DISCOUNTED ################################################

testthat::test_that("results correct |pathway_cba|discount_shape_mazur|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 19800,
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
        impact_benefit = 197000,
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

###### ONLY BENEFIT DISCOUNTED ##################################################

testthat::test_that("results correct |pathway_cba|discount_shape_mazur|discount_rate_benefit_TRUE|discount_rate_cost_FALSE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 19800,
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
        impact_benefit = 197000,
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

###### ONLY COST DISCOUNTED #####################################################

testthat::test_that("results correct |pathway_cba|discount_shape_mazur|discount_rate_benefit_FALSE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 19800,
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
        impact_benefit = 197000,
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

#### WITH INFLATION ################################################

##### EXPONENTIAL ###############################################################

###### BENEFIT & COST DISCOUNTED ################################################
# The results are the same as above because if discount and inflation are provided
# is because discount factor adjusts for inflation so no changes in results

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      healthiar::cba(
        impact_benefit = 50,
        valuation = 20,
        cost = 100,
        discount_shape = "exponential",
        discount_rate_benefit = 0.03,
        discount_rate_cost = 0.03,
        inflation_rate = 0.05,
        real_growth_rate = 0.05,
        n_years_benefit = 5,
        n_years_cost = 5
      )$cba_main$net_benefit_rounded,
    expect = 776 # Excel file from University of Porto "WP2_Examples.xlsx"
  )
})

###### ONLY BENEFIT DISCOUNTED ##################################################

testthat::test_that("results the same |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_FALSE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_benefit = 0.03,
        discount_shape = "exponential",
        inflation_rate = 0.05,
        n_years_benefit = 10
      )$cba_main$net_benefit_rounded,
    expect =
      -3306752109.0 # Result on 2026-03-02
  )
})

###### ONLY COST DISCOUNTED #####################################################

testthat::test_that("results the same |pathway_cba|discount_shape_exponential|discount_rate_benefit_FALSE|discount_rate_cost_TRUE|", {

  testthat::expect_equal(
    object =
      ## Adapted
      healthiar::cba(
        impact_benefit = 19800,
        valuation = 541000,
        cost = 8200000000,
        discount_rate_cost = 0.03,
        discount_shape = "exponential",
        n_years_cost = 5,
        n_years_benefit = 0,
        inflation_rate = 0.05
      )$cba_main$net_benefit_rounded,
    expect =
      5169612257.0 # Result on 2026-03-02
  )

})

## HEALTHIAR INPUT #############################################################

testthat::test_that("results correct |pathway_cba|discount_shape_exponential|discount_rate_benefit_TRUE|discount_rate_cost_TRUE|airqplus|hm_treasury_2026|", {

  # Two published sources are chained here:
  #  - the benefit comes from the COPD example of the Swiss GeLuft assessment
  #    in WHO AirQ+, which reports 3,502 (1,353-5,474) attributable cases;
  #  - discounting benefit and cost over five years at the Social Time
  #    Preference Rate of 3.5% must apply the factor of 0.8420 published in
  #    HM Treasury, "Discounting: Green Book supplementary guidance"
  #    (February 2026), Annex A, Table A.1.
  # Valuation and cost are round numbers, since AirQ+ does not monetize
  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))
  valuation <- 20
  cost <- 100
  green_book_factor_year_5 <- 0.8420

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

  # The attributable cases entering the cost-benefit analysis are the ones
  # published by AirQ+
  testthat::expect_equal(
    object = base::round(bestcost_pm_copd$health_main$impact),
    expected =
      c(data$estimated_number_of_attributable_cases_central,
        data$estimated_number_of_attributable_cases_lower,
        data$estimated_number_of_attributable_cases_upper))

  # Benefit and cost are discounted at the same rate over the same number of
  # years, so the undiscounted net benefit is reduced by exactly that factor
  net_benefit_undiscounted <-
    bestcost_pm_copd$health_main$impact * valuation - cost

  testthat::expect_equal(
    object =
      base::round(
        healthiar::cba(
          output_attribute = bestcost_pm_copd,
          valuation = valuation,
          cost = cost,
          discount_shape = "exponential",
          discount_rate_benefit = 0.035,
          discount_rate_cost = 0.035,
          n_years_benefit = 5,
          n_years_cost = 5)$cba_main$net_benefit / net_benefit_undiscounted,
        digits = 4),
    expected = base::rep(green_book_factor_year_5, times = 3))
})

# ERROR OR WARNING ########
## ERROR #########

## WARNING #########



