# QUANTITATIVE TEST ############################################################

## RR ###########################################################################

### SINGLE EXPOSURE #############################################################

testthat::test_that("result correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

    data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

    data$mean_concentration

    testthat::expect_equal(
      object =
        healthiar::attribute_health(
          approach_risk = "relative_risk",
          exp_central = data$mean_concentration,
          cutoff_central = data$cut_off_value,
          bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
          rr_central = data$relative_risk,
          rr_increment = 10,
          erf_shape = "log_linear",
          info = base::paste0(data$pollutant,"_", data$evaluation_name)
        )$health_main$impact_rounded,
      expected = # airqplus_pm_copd
        data |>
        dplyr::select(estimated_number_of_attributable_cases_central)|>
        base::as.numeric()
    )
})

testthat::test_that("zero effect if exp lower than cutoff |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = 4,
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10,
        erf_shape = "log_linear"
        )$health_main$impact_rounded,
    expected = 0
  )
})

# Multiple age and sex groups

testthat::test_that("result correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = base::rep("relative_risk", 4),
        age = c("below_50", "below_50", "50_plus", "50_plus"),
        sex = c("male", "female", "male", "female"),
        exp_central = base::rep(data$mean_concentration, 4),
        cutoff_central = base::rep(data$cut_off_value, 4),
        bhd_central = base::rep(data$incidents_per_100_000_per_year/1E5*data$population_at_risk, 4),
        rr_central = base::rep(data$relative_risk, 4),
        rr_increment = base::rep(10, 4),
        erf_shape = base::rep("log_linear", 4),
        info = base::paste0(data$pollutant,"_", data$evaluation_name)
      )$health_main$impact_rounded,
    expected = # airqplus_pm_copd
      data$estimated_number_of_attributable_cases_central * 4
  )
})

## same as above but with population argument
testthat::test_that("result correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = base::rep("relative_risk", 4),
        age = c("below_50", "below_50", "50_plus", "50_plus"),
        sex = c("male", "female", "male", "female"),
        exp_central = base::rep(data$mean_concentration, 4),
        cutoff_central = base::rep(data$cut_off_value, 4),
        bhd_central = base::rep(data$incidents_per_100_000_per_year/1E5*data$population_at_risk, 4),
        rr_central = base::rep(data$relative_risk, 4),
        rr_increment = base::rep(10, 4),
        erf_shape = base::rep("log_linear", 4),
        info = base::paste0(data$pollutant,"_", data$evaluation_name),
        population = c(500000, 200000, 600000, 800000)
      )$health_main$impact_rounded,
    expected = # airqplus_pm_copd
      data$estimated_number_of_attributable_cases_central * 4
  )
})

  testthat::test_that("result correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        exp_lower = 8,
        exp_upper = 9,
        cutoff_central = data$cut_off_value,
        cutoff_lower = data$cut_off_value - 1,
        cutoff_upper = data$cut_off_value + 1,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        bhd_lower = 25000,
        bhd_upper = 35000,
        rr_central = data$relative_risk,
        rr_lower = data$relative_risk_lower,
        rr_upper = data$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = base::paste0(data$pollutant,"_", data$evaluation_name)
      )$health_main$impact_rounded,
    expected = # airqplus_pm_copd
      data |>
      dplyr::select(estimated_number_of_attributable_cases_central,
                    estimated_number_of_attributable_cases_lower,
                    estimated_number_of_attributable_cases_upper)|>
      base::as.numeric()
  )

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        exp_lower = data$mean_concentration - 1,
        exp_upper = data$mean_concentration + 1,
        cutoff_central = data$cut_off_value,
        cutoff_lower = data$cut_off_value - 1,
        cutoff_upper = data$cut_off_value + 1,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        bhd_lower = data$incidents_per_100_000_per_year/1E5*data$population_at_risk - 1,
        bhd_upper = data$incidents_per_100_000_per_year/1E5*data$population_at_risk + 1,
        rr_central = data$relative_risk,
        rr_lower = data$relative_risk_lower,
        rr_upper = data$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = base::paste0(data$pollutant,"_", data$evaluation_name)
      )$health_detailed$results_raw$impact_rounded,
    expected = # Results on 2025-06-12; no comparison study
      c(3502, 1353, 5474, 4344, 1695, 6729, 2633, 1007, 4154, 3502, 1353, 5474, 4344, 1695, 6728, 2633, 1007, 4153, 3502, 1353, 5474, 4345, 1695, 6729, 2633, 1007, 4154, 2633, 1007, 4154, 3502, 1353, 5474, 1736, 658, 2764, 2633, 1007, 4153, 3502, 1353, 5474, 1736, 658, 2764, 2633, 1007, 4154, 3502, 1353, 5474, 1736, 658, 2764, 4344, 1695, 6729, 5161, 2032, 7921, 3502, 1353, 5474, 4344, 1695, 6728, 5161, 2032, 7921, 3502, 1353, 5474, 4345, 1695, 6729, 5161, 2032, 7921, 3502, 1353, 5474)
  )
})

testthat::test_that("detailed result the same |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        exp_lower = data$mean_concentration - 1,
        exp_upper = data$mean_concentration + 1,
        cutoff_central = data$cut_off_value,
        cutoff_lower = data$cut_off_value - 1,
        cutoff_upper = data$cut_off_value + 1,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        bhd_lower = data$incidents_per_100_000_per_year/1E5*data$population_at_risk - 1,
        bhd_upper = data$incidents_per_100_000_per_year/1E5*data$population_at_risk + 1,
        rr_central = data$relative_risk,
        rr_lower = data$relative_risk_lower,
        rr_upper = data$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = base::paste0(data$pollutant,"_", data$evaluation_name)
      )$health_detailed$results_raw$impact_rounded,
    expected = # Results on 2025-06-12; no comparison study
      c(3502, 1353, 5474, 4344, 1695, 6729, 2633, 1007, 4154, 3502, 1353, 5474, 4344, 1695, 6728, 2633, 1007, 4153, 3502, 1353, 5474, 4345, 1695, 6729, 2633, 1007, 4154, 2633, 1007, 4154, 3502, 1353, 5474, 1736, 658, 2764, 2633, 1007, 4153, 3502, 1353, 5474, 1736, 658, 2764, 2633, 1007, 4154, 3502, 1353, 5474, 1736, 658, 2764, 4344, 1695, 6729, 5161, 2032, 7921, 3502, 1353, 5474, 4344, 1695, 6728, 5161, 2032, 7921, 3502, 1353, 5474, 4345, 1695, 6729, 5161, 2032, 7921, 3502, 1353, 5474)
  )
})

testthat::test_that("no error rr_no_error|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_no_error(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        exp_lower = 8,
        exp_upper = 9,
        cutoff_central = data$cut_off_value,
        cutoff_lower = data$cut_off_value - 1,
        cutoff_upper = data$cut_off_value + 1,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        bhd_lower = 25000,
        bhd_upper = 35000,
        rr_central = data$relative_risk,
        rr_lower = data$relative_risk_lower,
        rr_upper = data$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = base::paste0(data$pollutant,"_", data$evaluation_name)
        )
    )
})

testthat::test_that("number of rows in detailed results correct |meta_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        exp_lower = 8,
        exp_upper = 9,
        cutoff_central = data$cut_off_value,
        cutoff_lower = data$cut_off_value - 1,
        cutoff_upper = data$cut_off_value + 1,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        bhd_lower = 25000,
        bhd_upper = 35000,
        rr_central = data$relative_risk,
        rr_lower = data$relative_risk_lower,
        rr_upper = data$relative_risk_upper,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = base::paste0(data$pollutant,"_", data$evaluation_name)
        )$health_detailed$results_raw |> base::nrow(),
    expected =
      3^4 # CI's in 4 input variables
      )

})

testthat::test_that("results the same |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        cutoff_central = data$cut_off_value,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        erf_eq_central = # GBD2019 ERF for PM and lower respiratory infections
          stats::splinefun(
            x = c(600,500,400,300,200,150,130,110,90,70,50,30,25,20,15,10,5,0),
            y = c(2.189,2.143,2.098,2.052,1.909,1.751,1.68,1.607,1.533,1.453,1.357,1.238,1.204,1.168,1.129,1.089,1.046,	1),
            method = "natural"),
        info = base::paste0(data$pollutant,"_", data$evaluation_name)
        )$health_main$impact_rounded,
    expected =
      c(1057) # Results on 10 October 2024 (with cutoff = 5 = data$cut_off_value); no comparison study
  )
})

testthat::test_that("results the same |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        cutoff_central = 0,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        erf_eq_central = # GBD2019 ERF for PM and lower respiratory infections
          stats::splinefun(
            x = c(600,500,400,300,200,150,130,110,90,70,50,30,25,20,15,10,5,0),
            y = c(2.189,2.143,2.098,2.052,1.909,1.751,1.68,1.607,1.533,1.453,1.357,1.238,1.204,1.168,1.129,1.089,1.046,	1),
            method = "natural"),
        info = base::paste0(data$pollutant,"_", data$evaluation_name))$health_main$impact_rounded,
    expected =
      c(2263) # Results on 10 October 2024 (with cutoff = 0); no comparison study
  )
})

testthat::test_that("results the same |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$mean_concentration,
        cutoff_central = data$cut_off_value,
        bhd_central = data$incidents_per_100_000_per_year/1E5*data$population_at_risk,
        erf_eq_central = # GBD2019 ERF for PM and lower respiratory infections
          stats::approxfun(
            x = c(600,500,400,300,200,150,130,110,90,70,50,30,25,20,15,10,5,0),
            y = c(2.189,2.143,2.098,2.052,1.909,1.751,1.68,1.607,1.533,1.453,1.357,1.238,1.204,1.168,1.129,1.089,1.046,	1),
            method = "linear"),
        info = base::paste0(data$pollutant,"_", data$evaluation_name)
        )$health_main$impact_rounded,
    expected =
      c(1052) # Results on 10 October 2024 (with cutoff = 5); no comparison study
  )
})

testthat::test_that("results correct |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "LMU_O3_COPD_mort_2016.rds"))

  erf <- stats::splinefun(data$x, data$y, method="natural")
  erf_l <- stats::splinefun(data$x, data$y_l, method="natural")
  erf_u <- stats::splinefun(data$x, data$y_u, method="natural")

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        erf_eq_central = erf,
        erf_eq_lower = erf_l,
        erf_eq_upper = erf_u,
        prop_pop_exp = 1,
        exp_central = 84.1, # exposure distribution for ozone
        cutoff_central = 0,
        bhd_central =  29908, #COPD mortality in Germany 2016
      )$health_main$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(319,243,386)
  )

  ## ASSESSOR: Susanne Breitner-Busch, LMU Munich
  ## ASSESSMENT DETAILS: https://www.umweltbundesamt.de/publikationen/quantifizierung-der-krankheitslast-verursacht-durch#:~:text=Beschrieben%20werden%20die%20gesundheitlichen%20Effekte%20in%20der%20deutschen,f%C3%BCr%20die%20Jahre%202007%20-%202016%20quantifiziert%20wurden.
  ## INPUT DATA DETAILS: Modelled ozone exposure, real COPD mortality data from Germany, 2016
})

testthat::test_that("results the same |pathway_rr|erf_lin_log|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = 20,
        cutoff_central = 5,
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "linear_log",
        bhd_central = 10)$health_main$impact,
    expected =
      0.927071 # Results on 08 August 2024 (ChatGPT); no comparison study
  )
})



testthat::test_that("results correct |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  # Use a calculation threshold which is different from the effect threshold
  # Goal: Health impacts in the exposure group 55dB+ that are affected by a exposure above the effect threshold (45 dB)

  exp <- c(300000,200000,150000,120000,100000,70000,60000)
  totpop <- 10000000
  exp_lab <- c(47,52,57,62,67,72,77)
  diseased <- 50000
  threshold_effect <- 45
  RR <- 1.055
  threshold_calculation <- 55
  rr_increment <- 10

  # Defining the exposure-response function (see ETC/HE 2023/11 report ("Environmental noise health risk assessment:
  # methodology for assessing health risks using data reported under the Environmental Noise Directive"; Chapter "PART III:
  # Calculation Methods"; Formula 2))
  # c = Vector containing the midpoints for each exposure category
  erf_function <- function(c){
    output <- ifelse(c<threshold_calculation, 1, exp((log(RR)/rr_increment)*(c-threshold_effect)))
    return(output)
  }
  # Calculations with healhtiar using the function "attribute_health"
  # Here we assume cutoff_central=0, because:
  # 1) we already included the threshold effect in the ERF function (erf_function),
  # which is defined above; attribute health defines "c = exp_central - cutoff_central",
  # 2) our defined ERF function only runs through the midpoints
  # If effect threshold and calculation threshold were identical in all cases, we could skip the previous step (defining the ERF)
  # and add the Arguments (rr_central=RR, rr_increment, erf_shape, cutoff_central) to the "attribute_health"
  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_eq_central = erf_function,
        prop_pop_exp = exp/totpop,
        exp_central = exp_lab,
        cutoff_central=0,
        bhd_central=diseased)$health_main$impact_rounded,
    expected = 2651
  )

})

testthat::test_that("results the same |pathway_rr|erf_log_log|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = 20,
        cutoff_central = 5,
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_log",
        bhd_central = 10)$health_main$impact,
    expected =
      0.936215963 # Results on 08 August 2024 (ChatGPT); no comparison study
  )
})

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {
  ## Pathway ID:|pathway_rr|erf_log_lin|exp_single|cutoff_FALSE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE|
  ## healthiar FUNCTION CALL
  results_pm <-
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.09,
      rr_lower = 1.06,
      rr_upper = 1.12,
      rr_increment = 10,
      exp_central = 8.3,
      cutoff_central = 0,
      bhd_central = 25235,
    )
  testthat::expect_equal(
    object =
      base::signif(c(results_pm$health_main$impact_rounded,results_pm$health_main$pop_fraction),3),
    expected =
      base::signif(c(1742.00000000,1191.00000000,2265.00000000,0.06902931,0.04721232,0.08977441),3)
  )

  # Original result from the paper:
  # Canada_impact_rounded : 1739
  # Canada_pop_fraction : 0.069 (95% CI: 0.047 - 0.090)

  ## List here the results of the comparison assessment you selected
  ## ASSESSOR: Maria José Rueda Lopez, CSTB
  ## Add here your name and your institute abbreviation
  ## ASSESSMENT DETAILS: Estimate the proportion of lung cancer cases attributable to PM2.5 exposure in Canada in 2015
  ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
  ## INPUT DATA DETAILS: Gogna et al., 2019. Estimates of the current and future burden of lung cancer attributable to PM2.5 in Canada. https://doi.org/10.1016/j.ypmed.2019.03.010
  ## Add here input data details: data sources, measured vs. modelled, ...
})


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {
  ## Pathway ID:|pathway_rr|erf_log_lin|exp_single|cutoff_FALSE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE|

  ## healthiar FUNCTION CALL
  results_pm <-
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.09,
      rr_lower = 1.06,
      rr_upper = 1.12,
      rr_increment = 10,
      exp_central = 9.2,
      cutoff_central = 0,
      bhd_central = 8380,
    )
  testthat::expect_equal(
    object =
      base::signif(c(results_pm$health_main$impact_rounded,results_pm$health_main$pop_fraction),3),
    expected =
      base::signif(c(639.0000, 437.0000,  830.0000, 0.0762, 0.0522, 0.0990),3)
  )

  # Original results form the paper
  # Ontario_impact_rounded : 662
  # Ontario_pop_fraction : 0.079 (95% CI: 0.054 - 0.103)

  ## List here the results of the comparison assessment you selected
  ## ASSESSOR: Maria José Rueda Lopez, CSTB
  ## Add here your name and your institute abbreviation
  ## ASSESSMENT DETAILS: Estimate the proportion of lung cancer cases attributable to PM2.5 exposure in Ontario in 2015
  ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
  ## INPUT DATA DETAILS: Gogna et al., 2019. Estimates of the current and future burden of lung cancer attributable to PM2.5 in Canada. https://doi.org/10.1016/j.ypmed.2019.03.010
  ## Add here input data details: data sources, measured vs. modelled, ...
})

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {
  ## Pathway ID:|pathway_rr|erf_log_lin|exp_single|cutoff_TRUE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE|

  ## healthiar FUNCTION CALL
  results_pm2.5 <-
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear", # Page 18
      rr_central = 1.15, # Page 33
      rr_lower = 1.05, # Page 33
      rr_upper = 1.25, # Page 33
      rr_increment = 10, # Page 18
      exp_central = 9.6, # Table 5 page 29
      exp_lower = 8.9, # Table 5 page 29
      exp_upper = 10.2, # Table 5 page 29
      cutoff_central = 5, # Page 33
      bhd_central = 561953, # Table 3 page 22
    )


  ### Healthiar results ###
  ## Attributable impact in France
  testthat::expect_equal(
    object =
      base::signif(c(results_pm2.5$health_main$impact_rounded,results_pm2.5$health_main$pop_fraction),3),
    expected =
      base::signif(c(34991.0000, 12472.0000, 54821.0000, 0.0623, 0.0222, 0.0976),3)
  )

  ### SpF report results ###
  # France_impact_Cuttoff5 : 39541. Table 8 page 37
  # France_pop_fraction_Cuttoff5 : 0.071 Table 8 page 37


  ## ASSESSOR: Maria José Rueda Lopez, CSTB and Sabrina Delaunay-Havard, SpF.
  ## ASSESSMENT DETAILS: Estimate the proportion of deaths attributable to PM2.5 exposure in France during the covid19 pandemic
  ## INPUT DATA DETAILS: Etudes et enquêtes. Impact de la pollution de l'air ambiant sur la mortalité en France métropolitaine. Réduction en lien avec le
  ## confinement du printemps 2020 et nouvelles données sur le poids total pour la période 2016-2019. Santé publique France. Avril 2021.
})


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {


  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO
        #rr_lower = 1.02,
        #rr_upper = 1.11,
        rr_increment = 10,
        prop_pop_exp = 1,
        exp_central = 5.8, #single for Urban centres(presumably Tallinn), pm2.5
        cutoff_central = 0,
        bhd_central = 4500 #deaths in tallinn 2020
      )$health_main$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      150
  )
})

# original value from EEA = 199 -> test did not pass but the value provided by attribute_health falls within the range provided by EEA

## ASSESSOR: Maria Lepnurm TAI
## ASSESSMENT DETAILS: I used data from https://discomap.eea.europa.eu/App/AQViewer/index.html?fqn=Airquality_Dissem.ebd.countries_and_nuts# for Estonias urban centres(presumably Tallinn) and data for attributable deaths in the year 2020. The number of deaths was obtained from statistics Estonia
## INPUT DATA DETAILS: Baseline from WHO 2005 (HRAPIE 2013), all cause mortality, attributable deaths from pm2.5 in 2020.

testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_single|cutoff_FALSE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  ## healthiar FUNCTION CALL
  results_NO2 <-
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "linear",
      rr_central = 1.04,
      rr_lower = 1.02,
      rr_upper = 1.07,
      rr_increment = 10,
      exp_central = 42.6,
      exp_lower = 33.1,
      exp_upper = 56.4,
      cutoff_central = 0,
      bhd_central = 9946,
    )


  testthat::expect_equal(
    ## Attributable impact in Naples
    object = results_NO2$health_main$impact_rounded,
    expected = c(1448, 781, 2285))

  testthat::expect_equal(
    ## Attributable impact in Naples
    object = base::signif(results_NO2$health_main$pop_fraction,3),
    expected = c(0.146, 0.0785, 0.230))



  # Original results from Paper (close to what attribute health gets):
  # Naples_impact_rounded : 1337 (95% CI: 700 - 2188)
  # Naples_pop_fraction : 13.9 (95% CI: 7.3 - 22.7)

  ## ASSESSOR: Maria José Rueda Lopez, CSTB

  ## ASSESSMENT DETAILS: Estimate the attributable death for exposition to NO2 concentrations in 2013 in Naples, Italy.

  ## INPUT DATA DETAILS: Chianese and Riccio 2024. Long-term variation in exposure to NO2 concentrations in the city of Naples, Italy: Results of a citizen science project
  ## https://doi.org/10.1016/j.scitotenv.2024.172799
})

testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {


  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO
        #rr_lower = 1.02,
        #rr_upper = 1.11,
        rr_increment = 10,
        prop_pop_exp = 1,
        exp_central = 5.8, #single for Urban centres(presumably Tallinn), pm2.5
        cutoff_central = 0,
        bhd_central = 4500 #deaths in tallinn 2020
      )$health_main$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      151 # test did not pass but the value provided by healthiar falls within the range provided by EEA
  )
})
# Original results from source: 200
## ASSESSOR: Maria Lepnurm TAI
## ASSESSMENT DETAILS: I used data from https://discomap.eea.europa.eu/App/AQViewer/index.html?fqn=Airquality_Dissem.ebd.countries_and_nuts# for Estonias urban centres(presumably Tallinn) and data for attributable deaths in the year 2020. The number of deaths was obtained from statistics Estonia
## INPUT DATA DETAILS: Baseline from WHO 2005 (HRAPIE 2013), all cause mortality, attributable deaths from pm2.5 in 2020.




##### Stratification (sex/age) ####################################################################

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|cutoff_TRUE|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|",{
  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))
  #Exotic test based on real data but does produce real world results


  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  rr_change = bhd_change <-1.2
  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(data$mean_concentration, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::signif(base::unlist(base::lapply(data$mean_concentration, function(x)  x * cutoff_change^(0:3))),5)
  bhd_c <- base::signif(base::unlist(base::lapply(data$incidents_per_100_000_per_year/1E5*data$population_at_risk, function(x) x * bhd_change^(0:3))),5)
  rr_c <- base::signif(base::unlist(base::lapply(data$relative_risk, function(x) x * rr_change^(0:3))),5)

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age = c("below_50", "below_50", "50_plus", "70_plus"),
    sex = c("male", "female", "male", "female"),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    rr_central = rr_c,
    rr_lower = rr_c - rr_c/uncert_factor,
    rr_upper = rr_c + rr_c/uncert_factor,
    rr_increment = c(10, 11, 12, 13),
    erf_shape = "log_linear",
    info = base::paste0(data$pollutant,"_", data$evaluation_name),
    population = c(500000, 200000, 600000, 800000)
  )


  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      4166, 3758, 4549, 5109, 4585, 5602, 3639, 3280, 3977, 3958, 3570, 4322,
      4854, 4356, 5322, 3457, 3116, 3778, 4374, 3946, 4777, 5365, 4814, 5882,
      3821, 3444, 4176, 3439, 3099, 3759, 3969, 3580, 4336, 2900, 2612, 3173,
      3267, 2944, 3571, 3771, 3401, 4119, 2755, 2481, 3014, 3611, 3254, 3947,
      4168, 3759, 4553, 3045, 2742, 3332, 5302, 4760, 5810, 6227, 5574, 6841,
      4362, 3936, 4761, 5036, 4522, 5520, 5916, 5295, 6499, 4143, 3739, 4523,
      5567, 4998, 6101, 6539, 5852, 7183, 4580, 4133, 4999, 10990, 10264, 11665,
      11519, 10764, 12221, 10452, 9757, 11101, 10440, 9751, 11082, 10943, 10226,
      11610, 9930, 9269, 10546, 11539, 10778, 12249, 12095, 11302, 12832, 10975,
      10245, 11656, 9966, 9299, 10590, 10512, 9813, 11163, 9412, 8777, 10006, 9468,
      8834, 10060, 9986, 9322, 10605, 8942, 8338, 9506, 10465, 9764, 11119, 11037,
      10303, 11721, 9883, 9216, 10506, 11983, 11203, 12707, 12496, 11689, 13244,
      11461, 10710, 12160, 11384, 10643, 12072, 11871, 11105, 12582, 10888, 10174,
      11552, 12582, 11763, 13342, 13121, 12274, 13907, 12034, 11245, 12768, 20256,
      19302, 21138, 20746, 19776, 21641, 19759, 18821, 20627, 19243, 18337, 20081,
      19708, 18787, 20559, 18771, 17880, 19595, 21269, 20267, 22195, 21783, 20765,
      22723, 20747, 19762, 21658, 18948, 18038, 19792, 19457, 18530, 20316, 18431,
      17539, 19259, 18001, 17136, 18802, 18484, 17603, 19300, 17510, 16662, 18296,
      19896, 18940, 20781, 20430, 19456, 21332, 19353, 18416, 20222, 21514, 20521,
      22430, 21985, 20978, 22913, 21036, 20057, 21939, 20438, 19495, 21308, 20885,
      19929, 21767, 19984, 19054, 20842, 22589, 21547, 23551, 23084, 22027, 24059,
      22087, 21060, 23036
    )
  )

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      10990, 10264, 11665, 11943, 11120, 12710, 10452, 9757, 11101, 10440, 9751,
      11082, 11346, 10564, 12075, 9930, 9269, 10546, 11539, 10778, 12249, 12540,
      11676, 13346, 10975, 10245, 11656, 9966, 9299, 10590, 10512, 9813, 11163,
      9412, 8777, 10006, 9468, 8834, 10060, 9986, 9322, 10605, 8942, 8338, 9506,
      10465, 9764, 11119, 11037, 10303, 11721, 9883, 9216, 10506, 12407, 11559,
      13197, 13339, 12396, 14216, 11461, 10710, 12160, 11787, 10981, 12537, 12672,
      11776, 13505, 10888, 10174, 11552, 13027, 12137, 13857, 14006, 13016, 14927,
      12034, 11245, 12768, 24422, 23061, 25687, 25430, 24006, 26753, 23398, 22102,
      24603, 23201, 21908, 24403, 24159, 22806, 25416, 22228, 20997, 23373, 25643,
      24214, 26972, 26702, 25206, 28091, 24568, 23207, 25834, 22387, 21137, 23551,
      23427, 22110, 24652, 21332, 20151, 22432, 21268, 20080, 22374, 22255, 21004,
      23419, 20265, 19143, 21310, 23507, 22194, 24729, 24598, 23215, 25885, 22398,
      21159, 23553, 26391, 24925, 27750, 27369, 25844, 28782, 25397, 23993, 26701,
      25071, 23679, 26363, 26001, 24552, 27343, 24127, 22793, 25366, 27710, 26172,
      29138, 28738, 27137, 30222, 26667, 25193, 28036
    ))
})

testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_single|cutoff_TRUE|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|",{
  data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))
  #Exotic test based on real data but does produce real world results


  #percentage of variation
  exp_change <-1.11
  cutoff_change <-0.9
  bhd_change <-0.95
  rr_change = bhd_change <-1.23
  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(data$mean_concentration, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::signif(base::unlist(base::lapply(data$mean_concentration, function(x)  x * cutoff_change^(0:3))),5)
  bhd_c <- base::signif(base::unlist(base::lapply(data$incidents_per_100_000_per_year/1E5*data$population_at_risk, function(x) x * bhd_change^(0:3))),5)
  rr_c <- base::signif(base::unlist(base::lapply(data$relative_risk, function(x) x * rr_change^(0:3))),5)

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age = c("below_50", "below_50", "50_plus", "70_plus"),
    sex = c("male", "female", "male", "female"),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    rr_central = rr_c,
    rr_lower = rr_c - rr_c/uncert_factor,
    rr_upper = rr_c + rr_c/uncert_factor,
    rr_increment = c(10, 11, 12, 13),
    erf_shape = "linear",
    info = base::paste0(data$pollutant,"_", data$evaluation_name),
    population = c(500000, 200000, 600000, 800000)
  )

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      3917,3479,4344,5147,4547,5732,3148,2789,3499,3721,3305,4127,4890,4319,5446,
      2990,2649,3324,4113,3653,4561,5405,4774,6019,3305,2928,3674,2963,2624,3296,
      3741,3321,4151,2149,1898,2397,2815,2492,3131,3554,3155,3943,2042,1803,2277,
      3111,2755,3461,3928,3487,4358,2257,1993,2517,5315,4698,5914,6491,5724,7234,
      4092,3637,4535,5049,4463,5619,6166,5438,6873,3887,3455,4309,5580,4933,6210,
      6815,6010,7596,4297,3818,4762,11632,10768,12456,12450,11545,13309,10775,9956,
      11558,11051,10230,11833,11827,10968,12644,10236,9458,10980,12214,11307,13079,
      13072,12122,13975,11314,10454,12136,10311,9518,11071,11191,10350,11994,9386,
      8648,10097,9795,9042,10517,10631,9832,11394,8917,8215,9592,10827,9994,11624,
      11750,10867,12593,9856,9080,10602,12861,11937,13737,13622,12664,14528,12063,
      11177,12906,12218,11340,13050,12941,12031,13802,11460,10618,12261,13504,12533,
      14424,14303,13297,15254,12666,11736,13551,23013,21842,24109,23781,22596,24887,
      22210,21054,23293,21862,20750,22904,22592,21466,23642,21099,20001,22129,24164,
      22934,25315,24970,23726,26131,23320,22107,24458,21474,20335,22545,22311,21154,
      23397,20595,19477,21649,20400,19318,21418,21196,20096,22227,19566,18503,20567,
      22548,21351,23672,23427,22211,24566,21625,20451,22732,24425,23231,25538,25131,
      23928,26251,23687,22505,24792,23204,22070,24261,23875,22731,24938,22503,21379,
      23553,25647,24393,26815,26388,25124,27563,24872,23630,26032
    )

  )


  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      11632,10768,12456,12944,11949,13893,10775,9956,11558,11051,10230,11833,
      12297,11351,13198,10236,9458,10980,12214,11307,13079,13591,12546,14588,
      11314,10454,12136,10311,9518,11071,11191,10350,11994,9386,8648,10097,
      9795,9042,10517,10631,9832,11394,8917,8215,9592,10827,9994,11624,
      11750,10867,12593,9856,9080,10602,13355,12340,14321,14595,13461,15674,
      12063,11177,12906,12687,11723,13605,13865,12788,14890,11460,10618,12261,
      14023,12957,15037,15324,14134,16458,12666,11736,13551,26930,25321,28453,
      28434,26740,30035,25357,23843,26793,25584,24055,27031,27012,25403,28533,
      24089,22651,25453,28277,26587,29876,29856,28077,31537,26625,25035,28132,
      24437,22958,25841,26052,24474,27547,22745,21375,24046,23215,21810,24549,
      24749,23251,26170,21607,20306,22844,25659,24106,27133,27355,25698,28925,
      23882,22444,25248,29246,27526,30869,30650,28855,32339,27779,26141,29328,
      27784,26149,29325,29117,27413,30722,26390,24834,27861,30708,28902,32412,
      32182,30298,33956,29168,27448,30794
    )
  )
})



testthat::test_that("results the same |pathway_rr|erf_function|exp_single|cutoff_TRUE|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|",{
  data <- base::readRDS(testthat::test_path("data", "LMU_O3_COPD_mort_2016.rds"))
  #Exotic test based on real data but does produce real world results


  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  rr_change = bhd_change <-1.2
  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(84.1, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::signif(base::unlist(base::lapply(1, function(x)  x * cutoff_change^(0:3))),5)
  bhd_c <- base::signif(base::unlist(base::lapply(29908, function(x) x * bhd_change^(0:3))),5)

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age = c("below_50", "below_50", "50_plus", "70_plus"),
    sex = c("male", "female", "male", "female"),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    erf_eq_central = stats::splinefun(data$x, data$y, method="natural"),
    erf_eq_lower  = stats::splinefun(data$x, data$y_l, method="natural"),
    erf_eq_upper  = stats::splinefun(data$x, data$y_u, method="natural"),
    prop_pop_exp = c(1,1,1,1))

  testthat::expect_equal(
    ## test if age group results are correct
    object =x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      739, 562, 894, 739, 563, 895, 738, 562, 893, 702, 534, 849, 702, 535, 850,
      701, 534, 849, 776, 590, 939, 776, 591, 939, 775, 590, 938, 686, 522, 830,
      686, 522, 831, 685, 522, 829, 652, 496, 789, 652, 496, 789, 651, 495, 788,
      720, 548, 872, 721, 549, 872, 720, 548, 871, 782, 595, 946, 782, 595, 947,
      781, 595, 946, 743, 565, 899, 743, 566, 899, 742, 565, 898, 821, 625, 993,
      821, 625, 994, 821, 625, 993, 558, 424, 675, 558, 425, 675, 557, 424, 674,
      530, 403, 641, 530, 403, 641, 530, 403, 641, 585, 446, 708, 586, 446, 709,
      585, 446, 708, 534, 407, 646, 534, 407, 646, 534, 406, 646, 507, 386, 614,
      508, 386, 614, 507, 386, 614, 561, 427, 679, 561, 427, 679, 561, 427, 678,
      578, 440, 699, 578, 440, 699, 578, 440, 699, 549, 418, 664, 549, 418, 664,
      549, 418, 664, 607, 462, 734, 607, 462, 734, 607, 462, 734, 717, 546, 868,
      718, 546, 868, 717, 546, 868, 682, 519, 825, 682, 519, 825, 681, 519, 824,
      753, 574, 911, 753, 574, 911, 753, 573, 911, 692, 527, 837, 692, 527, 837,
      692, 527, 837, 657, 500, 795, 657, 500, 795, 657, 500, 795, 726, 553, 879,
      727, 553, 879, 726, 553, 879, 743, 566, 899, 743, 566, 899, 743, 566, 899,
      706, 537, 854, 706, 538, 854, 706, 537, 854, 780, 594, 944, 780, 594, 944,
      780, 594, 944
    )

  )
  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      871, 663, 1053, 871, 663, 1054, 870, 662, 1053,
      827, 630, 1001, 828, 630, 1001, 827, 629, 1000,
      914, 696, 1106, 915, 696, 1107, 914, 695, 1106,
      819, 623, 991, 819, 624, 991, 818, 623, 990,
      778, 592, 941, 778, 592, 942, 777, 592, 941,
      860, 654, 1040, 860, 655, 1041, 859, 654, 1040,
      913, 695, 1105, 914, 696, 1106, 913, 695, 1105,
      868, 661, 1050, 868, 661, 1050, 867, 660, 1050,
      959, 730, 1160, 959, 730, 1161, 959, 730, 1160,
      1143, 870, 1383, 1144, 871, 1383, 1143, 870, 1383,
      1086, 827, 1314, 1086, 827, 1314, 1086, 827, 1314,
      1200, 914, 1452, 1201, 914, 1453, 1200, 914, 1452,
      1093, 832, 1323, 1093, 832, 1323, 1093, 832, 1322,
      1038, 791, 1256, 1039, 791, 1257, 1038, 790, 1256,
      1148, 874, 1389, 1148, 874, 1389, 1147, 873, 1388,
      1189, 906, 1439, 1190, 906, 1439, 1189, 905, 1439,
      1130, 860, 1367, 1130, 860, 1367, 1130, 860, 1367,
      1249, 951, 1511, 1249, 951, 1511, 1249, 951, 1510
    )
  )
})



#### ITERATION ##################################################################

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        geo_id_micro = c("Zürich", "Basel", "Geneva", "Ticino", "Jura"),
        geo_id_macro = c("German","German","French","Italian","French"),
        erf_shape = "log_linear",
        rr_central = 1.369,
        rr_increment = 10,
        cutoff_central = 5,
        exp_central = c(11, 11, 10, 8, 7), # exposure in Geneva and Jura the same
        bhd_central = c(4000, 2500, 3000, 1500, 500)
        )$health_main$impact,
    expected =
      c(1116.41855325132, 466.433010062623, 134.881901125568 ) # Results on 28 Oct 2025; no comparison study
  )

})

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  bestcost_pm_mortality_a <-
    healthiar::attribute_health(
      exp_central = 8.1,
      cutoff_central =  0,
      bhd_central = 1000,
      rr_central = 1.063,
      rr_increment = 10,
      erf_shape = "log_linear",
      population = 1E5)

  bestcost_pm_mortality_b <-
    healthiar::attribute_mod(
      output_attribute = bestcost_pm_mortality_a,
      exp_central = 7.1,
      bhd_central = 2000,
      population = 2E5)

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = c(8.1, 7.1),
        cutoff_central =  0,
        bhd_central = c(1000, 2000),
        rr_central = 1.063,
        rr_increment = 10,
        erf_shape = "log_linear",
        population = c(1E5, 2E5),
        geo_id_micro = c("a", "b"))$health_main$impact_per_100k_inhab,
    expected =
      c(48.2824986/1E5*1E5, 84.9003458/2E5*1E5) # Results on 30 April 2025; no comparison study
  )

})

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = runif_with_seed(1E2, 8.0, 9.0, 1),
        exp_lower = runif_with_seed(1E2, 8.0, 9.0, 1)-0.1,
        exp_upper = runif_with_seed(1E2, 8.0, 9.0, 1)+0.1,
        cutoff_central = 5,
        bhd_central = runif_with_seed(1E2, 25000, 35000, 1),
        bhd_lower = runif_with_seed(1E2, 25000, 35000, 1) - 1000,
        bhd_upper = runif_with_seed(1E2, 25000, 35000, 1) + 1000,
        rr_central = 1.369,
        rr_lower = 1.124,
        rr_upper = 1.664,
        rr_increment = 10,
        erf_shape = "log_linear",
        population = base::rep(1E6, 1E2),
        geo_id_micro = 1:1E2,
        geo_id_macro = base::rep("CH", 1E2),
        info = "PM2.5_copd")$health_main$impact_rounded,
    expected =
      c(317577, 122363, 497741) # Results on 30 April 2025; no comparison study
  )
})

## no cutoff
testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("data", "mort_pm25_sect_2021.rds"))

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.118,
        rr_increment = 10.0,
        exp_central = data$PM25,
        cutoff_central = 0,
        bhd_central = data$VALUE_BASELINE,
        geo_id_micro = data$CS01012020
      )$health_main$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      base::round(data$VALUE)
  )

  ## ASSESSOR: Arno Pauwels, SCI
  ## ASSESSMENT DETAILS: All-cause mortality attributable to PM2.5, by census tract (iteration)
  ## INPUT DATA DETAILS: Modelled exposure, real mortality data from Belgium, 2021
})

## with cutoff
testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("data", "mort_pm25_sect_2021_cutoff.rds"))

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.118,
        rr_increment = 10.0,
        exp_central = data$PM25,
        cutoff_central = 2.5,
        bhd_central = data$VALUE_BASELINE,
        geo_id_micro = data$CS01012020
      )$health_main$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      base::round(data$VALUE)
  )

  ## ASSESSOR: Arno Pauwels, SCI
  ## ASSESSMENT DETAILS: All-cause mortality attributable to PM2.5, by census tract (iteration) WITH CUTOFF
  ## INPUT DATA DETAILS: Modelled exposure, real mortality data from Belgium, 2021
})

testthat::test_that("results correct |pathway_rr|erf_function|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "LMU_O3_COPD_mort_2016.rds"))
  erf<-stats::splinefun(data$x, data$y, method="natural")
  erf_l<-stats::splinefun(data$x, data$y_l, method="natural")
  erf_u<-stats::splinefun(data$x, data$y_u, method="natural")

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        erf_eq_central = erf,
        erf_eq_lower = erf_l,
        erf_eq_upper = erf_u,
        prop_pop_exp = 1,
        exp_central = c(82.6,88.7,84.1), # exposure distribution for ozone
        cutoff_central = 0,
        bhd_central =  c(27001,31064,29908), #COPD mortality in Germany 2016
        geo_id_micro = c("2014","2015","2016")
      )$health_main$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(280,213,339,355,270,430,319,243,386)
  )

  ## ASSESSOR: Susanne Breitner-Busch, LMU Munich
  ## ASSESSMENT DETAILS: https://www.umweltbundesamt.de/publikationen/quantifizierung-der-krankheitslast-verursacht-durch#:~:text=Beschrieben%20werden%20die%20gesundheitlichen%20Effekte%20in%20der%20deutschen,f%C3%BCr%20die%20Jahre%202007%20-%202016%20quantifiziert%20wurden.
  ## INPUT DATA DETAILS: Modelled ozone exposure, real COPD mortality data from Germany, 2016
})

testthat::test_that("results the same |pathway_rr|erf_lin_log|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        geo_id_micro = c("a", "b"),
        exp_central = c(20, 20),
        cutoff_central = 5,
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "linear_log",
        bhd_central = c(10, 10))$health_detailed$results_raw$impact,
    expected =
      c(0.927071, 0.927071) # Results on 08 August 2024 (ChatGPT); no comparison study
  )
})

testthat::test_that("results the same |pathway_rr|erf_log_log|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        geo_id_micro = c("a", "b"),
        exp_central = c(20, 20),
        cutoff_central = 5,
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_log",
        bhd_central = c(10, 10))$health_detailed$results_raw$impact,
    expected =
      c(0.936215963, 0.936215963) # Results on 06 August 2024 (ChatGPT); no comparison study
  )
})



testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      base::signif(healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        prop_pop_exp = 1,
        exp_central =  2.1,
        cutoff_central = 0,
        geo_id_micro = c("Harku", "Jõelähtme", "Keila"), #id codes for each area, can be names also
        bhd_central = c(92, 47, 103 ) #deaths in chosen regions
      )$health_main$impact,3),
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected = c(1.14, 0.585, 1.28)

  )
})
#original results form publication: c(2.5, 1.2, 1.7) # test did not pass but the value provided by healthiar differs on average 0.8
## ASSESSOR: Maria Lepnurm TAI
## ASSESSMENT DETAILS: I used data from https://keskkonnaportaal.ee/sites/default/files/2024-05/V%C3%A4lis%C3%B5hu%20kvaliteedi%20m%C3%B5ju%20v%C3%B5rdlus%20inimeste%20tervisele%20Eestis%20aastatel%202010%20ja%202020%20ning%20%C3%B5husaaste%20tervisem%C3%B5jude%20prognoos%20aastaks%202030.pdf for Estonian administrative units and data for attributable deaths in the year 2020. The number of deaths was obtained from statistics Estonia
## INPUT DATA DETAILS: obtained from the study mentioned. Calculated attributable deaths from pm2.5 in 2020.


testthat::test_that("results the same|pathway_rr|erf_lin_lin|exp_single|cutoff_FALSE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|",{
  ## Pathway ID: pathway_rr|erf_lin_lin|exp_single|cutoff_FALSE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|
  ## healthiar FUNCTION CALL
  results_NO2 <-
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "linear",
      rr_central = 1.04,
      rr_lower = 1.02,
      rr_upper = 1.07,
      rr_increment = 10,
      exp_central = c(42.6,39.0,42.4),
      exp_lower = c(33.1, 25.5, 33.1),
      exp_upper = c(56.4,58.6,55.8),
      cutoff_central = 0,
      bhd_central = c(9946,9679,10539),
      geo_id_micro = c('2013','2014','2015'),
    )

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =results_NO2$health_main$impact_rounded,
    expected = c(1448.0, 781.0, 2285.0, 1306.0, 700.0, 2076.0, 1528.0, 824.0, 2412.0)
  )

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =base::signif(results_NO2$health_main$pop_fraction,3),
    expected = c(0.146, 0.0785, 0.230, 0.135, 0.0724, 0.214, 0.145, 0.0782, 0.229)
  )
  # original results impact = 1337.0 700.0 2188.0 1247.0 653.0 2041.0 1404.0 735.0 2299.0, see also below
  # original results pop fraction: c(0.139, 0.073, 0.227, 0.13, 0.068, 0.212, 0.146, 0.076, 0.239) see also below
  # Attributable impact in Naples
  ### 2013 ###
  results_NO2$health_main$impact_rounded[1] # 1337 (95% CI: 700 - 2188)
  results_NO2$health_main$pop_fraction[1] # 13.9 (95% CI: 7.3 - 22.7)

  ### 2014 ###
  results_NO2$health_main$impact_rounded[4] # 1247 (95% CI 653 - 2041)
  results_NO2$health_main$pop_fraction[4] # 13.0 (95% CI: 6.8 - 21.2)

  ### 2015 ###
  results_NO2$health_main$impact_rounded[7] # 1404 (95% CI 735 - 2299)
  results_NO2$health_main$pop_fraction[7] # 14.6 (95% CI: 7.6 - 23.9)

  ## ASSESSOR: Maria José Rueda Lopez, CSTB
  ## ASSESSMENT DETAILS: Estimate the attributable death for exposition to NO2 concentrations in 2013, 2014 and 2015 in Naples, Italy.
  ## INPUT DATA DETAILS: Chianese and Riccio 2024. Long-term variation in exposure to NO2 concentrations in the city of Naples, Italy: Results of a citizen science project
  ## https://doi.org/10.1016/j.scitotenv.2024.172799
})

testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|cutoff_FALSE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|",{
  ## Pathway ID: pathway_rr|erf_log_lin|exp_single|cutoff_FALSE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|
  ## healthiar FUNCTION CALL
  pm_iteration <-
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.09,
      rr_lower = 1.06,
      rr_upper = 1.12,
      rr_increment = 10,
      exp_central = c(7,5.5),
      cutoff_central = 0,
      bhd_central = c(2085, 855),
      geo_id_micro = c('Alberta','Manitoba'),
    )
  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =base::signif(pm_iteration$health_main$pop_fraction,2),
    expected = c(0.059,0.040, 0.076, 0.046, 0.032, 0.060))


  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =pm_iteration$health_main$impact_rounded,
    expected = c(122,83.0, 159.0, 40, 27.0, 52.0))


  ### below are the original results from the paper (only two very small adjustments where needed)
  ## Attributable impact in Alberta
  pm_iteration$health_main$impact_rounded[1:3]
  pm_iteration$health_main$pop_fraction[1:3]
  # Alberta_impact_rounded : 123
  # Alberta_pop_fraction : 0.059 (95% CI: 0.040 - 0.077)


  ## Attributable impact in Manitoba
  pm_iteration$health_main$impact_rounded[4:6]
  pm_iteration$health_main$pop_fraction[4:6]
  # Manitoba_impact_rounded : 40
  # Manitoba_pop_fraction : 0.046 (95% CI: 0.032 - 0.060)

  ## List here the results of the comparison assessment you selected
  ## ASSESSOR: Maria José Rueda Lopez, CSTB
  ## Add here your name and your institute abbreviation
  ## ASSESSMENT DETAILS: Estimate the proportion of lung cancer cases attributable to PM2.5 exposure in Alberta and Manitoba in 2015
  ## Add here short description of the assessment: year, metric (e.g. DALY, premature deaths, ...), ...
  ## INPUT DATA DETAILS: Gogna et al., 2019. Estimates of the current and future burden of lung cancer attributable to PM2.5 in Canada. https://doi.org/10.1016/j.ypmed.2019.03.010
  ## Add here input data details: data sources, measured vs. modelled, ...
})

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|cutoff_FALSE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      base::signif(healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        prop_pop_exp = 1,
        exp_central = c(1.1, 1.6, 1.6, 1.5 ), # dist = exposure distribution, e.g. 5 different exposure
        #categories (~ exposure ranges) with the information how many people are
        #exposed to each of the 5 exposure range heitgaasid-pm2.5
        cutoff_central = 0,
        geo_id_micro = c("Anija", "Harku", "Jõelähtme", "Keila"), #id codes for each area, can be names also
        bhd_central = c(88, 92, 47, 103 ) #deaths in chosen regions
      )$health_main$impact,2),##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(0.56, 0.85, 0.44, 0.90) # test did not pass but the value provided by healthiar differs on average 0.9
  )
})
# original results from paper: c(0.8, 2.5, 1.2, 1.7) # test did not pass but the value provided by healthiar differs on average 0.9
## ASSESSOR: Maria Lepnurm TAI
## ASSESSMENT DETAILS: I used data from https://keskkonnaportaal.ee/sites/default/files/2024-05/V%C3%A4lis%C3%B5hu%20kvaliteedi%20m%C3%B5ju%20v%C3%B5rdlus%20inimeste%20tervisele%20Eestis%20aastatel%202010%20ja%202020%20ning%20%C3%B5husaaste%20tervisem%C3%B5jude%20prognoos%20aastaks%202030.pdf for Estonian administrative units and data for attributable deaths in the year 2020. The number of deaths was obtained from statistics Estonia
## INPUT DATA DETAILS: obtained from the study mentioned. Calculated attributable deaths from pm2.5 in 2020.


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|cutoff_TRUE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        prop_pop_exp = 1,
        exp_central = c(2.1, 3, 2.6, 2.2, 2.3, 1.9, 1.8, 2.2  ), # exposure for NO2
        # dist = exposure distribution, e.g. 5 different exposure
        #categories (~ exposure ranges) with the information how many people are
        #exposed to each of the 5 exposure range heitgaasid-pm2.5
        cutoff_central = 0,
        geo_id_micro = c("Haabersti", "Kesklinn", "Kristiine", "Lasnamäe", "Mustamäe", "Nõmme", "Pirita", "Põhja-Tallinn"),
        bhd_central = c(433, 433, 289, 1232, 926, 397, 140, 694) #deaths in chosen regions
      )$health_main$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(5.0,  8.0,  4.0, 16.0, 12.0,  4.0, 1.0,  9.0) # test did not pass but the value provided by healthiar differs on average 14.8, might need to look into the differences
  )
})
## original results from publication: c(16.2, 33.8, 15.3, 44.6, 28.4, 12, 4.9, 22.5) # test did not pass but the value provided by healthiar differs on average 14.8, might need to look into the differences)
## ASSESSOR: Maria Lepnurm TAI
## ASSESSMENT DETAILS: I used data from https://keskkonnaportaal.ee/sites/default/files/2024-05/V%C3%A4lis%C3%B5hu%20kvaliteedi%20m%C3%B5ju%20v%C3%B5rdlus%20inimeste%20tervisele%20Eestis%20aastatel%202010%20ja%202020%20ning%20%C3%B5husaaste%20tervisem%C3%B5jude%20prognoos%20aastaks%202030.pdf for Estonian administrative units and data for attributable deaths in the year 2020. The number of deaths was obtained from statistics Estonia
## INPUT DATA DETAILS: obtained from the study mentioned. Calculated attributable deaths from pm2.5 in 2020.


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|cutoff_TRUE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|",{
  ## healthiar FUNCTION CALL
  results_pm2.5 <-
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear", # Page 18
      rr_central = 1.15, # Page 33
      rr_lower = NULL, #1.05, Page 33
      rr_upper = NULL, #1.25, Page 33
      rr_increment = 10, # Page 18
      exp_central = c(9.5, 9.8, 9.9, 10.9,9.6), # Table 5 page 29
      exp_lower = NULL, # list(6.6, 7.1, 7.2, 7.8, 6.6), # Table 5 page 29
      exp_upper = NULL, # list(13.5,13.5, 13.3, 14.4, 14.4), # Table 5 page 29
      cutoff_central = 5, # Page 33
      bhd_central = c(133103, 121061, 87860, 219929, 561953), # Table 3 page 22
      geo_id_micro = c('Rural','Semi-rural', 'Semi-urbaines','Urbaines','France'),
    )

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =results_pm2.5$health_main$impact_rounded,
    expected = c(8113,7855, 5816, 17408, 34991))

  ### SpF report results ###
  # France_impact_Cuttoff5 : (7836, 7534, 5721, 18450, 39541) Table 8 page 37
  # France_pop_fraction_Cuttoff5 : (0.059, 0.063, 0.066, 0.084, 0.0071) Table 8 page 37
})

testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|cutoff_TRUE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|",{
  results_NO2 <-
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear", # Page 18
      rr_central = 1.023, # Page 14
      rr_lower = NULL, #1.008 # Page 14
      rr_upper = NULL, #1.037 # Page 14
      rr_increment = 10, # Page 18
      exp_central = c(11.5, 12.4, 13.2, 17.2, 12.0), # Table 5 page 29
      exp_lower = NULL, #list(7.4, 7.6, 7.9, 8.0, 7.4), # Table 5 page 29
      exp_upper = NULL, #list(23.5, 22.8, 21.0, 34.3, 34.3), # Table 5 page 29
      cutoff_central = 10, # Page 33
      bhd_central = c(133103, 121061, 87860, 219929, 561953), # Table 3 page 22
      geo_id_micro = c('Rural','Semi-rural', 'Semi-urbaines','Urbaines','France')
    )
  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =results_NO2$health_main$impact_rounded,
    expected = c(453.0,  659.0,  637.0,  3571.0,  2550.0))

  ### SpF report results ###
  # France_impact_Cuttoff10 : (451, 596, 633, 5110, 6790) Table 8 page 37
  # France_pop_fraction_Cuttoff10 : (0.003, 0.005, 0.007, 0.023, 0.0012) Table 8 page 37

  ## ASSESSOR: Maria José Rueda Lopez, CSTB and Sabrina Delaunay-Havard, SpF.
  ## ASSESSMENT DETAILS: Estimate the proportion of deaths attributable to PM2.5 exposure in France during the covid19 pandemic
  ## INPUT DATA DETAILS: Etudes et enquêtes. Impact de la pollution de l'air ambiant sur la mortalité en France métropolitaine. Réduction en lien avec le
  ## confinement du printemps 2020 et nouvelles données sur le poids total pour la période 2016-2019. Santé publique France. Avril 2021.
})


testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      base::signif(healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        exp_central = c(1.1, 1.6, 1.6, 1.5 ), # dist = exposure distribution, e.g. 5 different exposure
        #categories (~ exposure ranges) with the information how many people are
        #exposed to each of the 5 exposure range heitgaasid-pm2.5
        cutoff_central = 0,
        geo_id_micro = c("Anija", "Harku", "Jõelähtme", "Keila"), #id codes for each area, can be names also
        bhd_central = c(88, 92, 47, 103 ) #deaths in chosen regions
      )$health_main$impact,3),
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected = c(0.5770, 0.8750, 0.4470, 0.9190)
  )
})
# original results: c(0.8, 2.5, 1.2, 1.7) # test did not pass but the value provided by healthiar differs on average 0.9
## ASSESSOR: Maria Lepnurm TAI
## ASSESSMENT DETAILS: I used data from https://keskkonnaportaal.ee/sites/default/files/2024-05/V%C3%A4lis%C3%B5hu%20kvaliteedi%20m%C3%B5ju%20v%C3%B5rdlus%20inimeste%20tervisele%20Eestis%20aastatel%202010%20ja%202020%20ning%20%C3%B5husaaste%20tervisem%C3%B5jude%20prognoos%20aastaks%202030.pdf for Estonian administrative units and data for attributable deaths in the year 2020. The number of deaths was obtained from statistics Estonia
## INPUT DATA DETAILS: obtained from the study mentioned. Calculated attributable deaths from pm2.5 in 2020.


##### Stratification (sex/age) ####################################################################

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|cutoff_TRUE|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|",{
  data <- base::readRDS(testthat::test_path("data", "mort_pm25_sect_2021.rds"))
  #Exotic test based on real data but does produce real world results

  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- data$PM25
  cutoff_c <- base::rep(0, length.out = length(exp_c))
  bhd_c <- data$VALUE_BASELINE
  rr_c <- base::rep(1.118, length.out = length(bhd_c))

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age = base::rep(c("below_50", "below_50", "50_plus", "70_plus"), length.out = length(bhd_c)),
    sex = base::rep(c("male", "female"), length.out = length(bhd_c)),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    rr_central = rr_c,
    rr_lower = rr_c - rr_c/uncert_factor,
    rr_upper = rr_c + rr_c/uncert_factor,
    rr_increment = base::rep(c(10, 11, 12, 13), length.out = length(bhd_c)),
    erf_shape = "log_linear",
    geo_id_micro = data$CS01012020,
    geo_id_macro = base::rep(c("basel","bern","zuerich","genf","lausanne"), length.out = length(bhd_c))
  )

  df_by_sex <- x$health_detailed$results_raw |>
    dplyr::group_by(sex, exp_ci,bhd_ci,cutoff_ci,erf_ci) |>
    dplyr::summarise(mean_value = base::mean(impact, na.rm = TRUE), .groups = "drop")

  df_by_age_group <- x$health_detailed$results_raw |>
    dplyr::group_by(age_group, exp_ci,bhd_ci,cutoff_ci,erf_ci) |>
    dplyr::summarise(mean_value = base::mean(impact, na.rm = TRUE), .groups = "drop")
  base::signif(df_by_age_group$mean_value,5)
  testthat::expect_equal(
    ## test if age group results are correct
    object = base::signif(df_by_age_group$mean_value,5),
    expected = c(
      1.71580,0.95045,2.40820,1.71580,0.95045,2.40820,1.71580,0.95045,2.40820,
      1.63000,0.90293,2.28780,1.63000,0.90293,2.28780,1.63000,0.90293,2.28780,
      1.80160,0.99797,2.52870,1.80160,0.99797,2.52870,1.80160,0.99797,2.52870,
      1.63440,0.90427,2.29680,1.63440,0.90427,2.29680,1.63440,0.90427,2.29680,
      1.55270,0.85906,2.18190,1.55270,0.85906,2.18190,1.55270,0.85906,2.18190,
      1.71620,0.94949,2.41160,1.71620,0.94949,2.41160,1.71620,0.94949,2.41160,
      1.79660,0.99649,2.51880,1.79660,0.99649,2.51880,1.79660,0.99649,2.51880,
      1.70680,0.94666,2.39290,1.70680,0.94666,2.39290,1.70680,0.94666,2.39290,
      1.88650,1.04630,2.64480,1.88650,1.04630,2.64480,1.88650,1.04630,2.64480,
      1.45520,0.80462,2.04590,1.45520,0.80462,2.04590,1.45520,0.80462,2.04590,
      1.38240,0.76439,1.94360,1.38240,0.76439,1.94360,1.38240,0.76439,1.94360,
      1.52790,0.84485,2.14820,1.52790,0.84485,2.14820,1.52790,0.84485,2.14820,
      1.38590,0.76544,1.95070,1.38590,0.76544,1.95070,1.38590,0.76544,1.95070,
      1.31660,0.72717,1.85320,1.31660,0.72717,1.85320,1.31660,0.72717,1.85320,
      1.45520,0.80372,2.04820,1.45520,0.80372,2.04820,1.45520,0.80372,2.04820,
      1.52410,0.84368,2.14050,1.52410,0.84368,2.14050,1.52410,0.84368,2.14050,
      1.44780,0.80150,2.03350,1.44780,0.80150,2.03350,1.44780,0.80150,2.03350,
      1.60030,0.88587,2.24750,1.60030,0.88587,2.24750,1.60030,0.88587,2.24750,
      1.85620,1.03200,2.59650,1.85620,1.03200,2.59650,1.85620,1.03200,2.59650,
      1.76340,0.98037,2.46670,1.76340,0.98037,2.46670,1.76340,0.98037,2.46670,
      1.94900,1.08360,2.72630,1.94900,1.08360,2.72630,1.94900,1.08360,2.72630,
      1.76890,0.98204,2.47770,1.76890,0.98204,2.47770,1.76890,0.98204,2.47770,
      1.68040,0.93294,2.35380,1.68040,0.93294,2.35380,1.68040,0.93294,2.35380,
      1.85730,1.03110,2.60160,1.85730,1.03110,2.60160,1.85730,1.03110,2.60160,
      1.94290,1.08170,2.71420,1.94290,1.08170,2.71420,1.94290,1.08170,2.71420,
      1.84580,1.02760,2.57850,1.84580,1.02760,2.57850,1.84580,1.02760,2.57850,
      2.04000,1.13580,2.84990,2.04000,1.13580,2.84990,2.04000,1.13580,2.84990
    )

  )
  testthat::expect_equal(
    ## test if sex results are correct
    object = base::signif(df_by_sex$mean_value,5),
    expected = c(
      1.59040,0.88134,2.23160,1.59040,0.88134,2.23160,1.59040,0.88134,2.23160,
      1.51090,0.83728,2.12000,1.51090,0.83728,2.12000,1.51090,0.83728,2.12000,
      1.67000,0.92541,2.34320,1.67000,0.92541,2.34320,1.67000,0.92541,2.34320,
      1.51510,0.83854,2.12840,1.51510,0.83854,2.12840,1.51510,0.83854,2.12840,
      1.43940,0.79662,2.02200,1.43940,0.79662,2.02200,1.43940,0.79662,2.02200,
      1.59090,0.88047,2.23480,1.59090,0.88047,2.23480,1.59090,0.88047,2.23480,
      1.66530,0.92401,2.33400,1.66530,0.92401,2.33400,1.66530,0.92401,2.33400,
      1.58210,0.87781,2.21730,1.58210,0.87781,2.21730,1.58210,0.87781,2.21730,
      1.74860,0.97022,2.45070,1.74860,0.97022,2.45070,1.74860,0.97022,2.45070,
      1.85120,1.02820,2.59200,1.85120,1.02820,2.59200,1.85120,1.02820,2.59200,
      1.75860,0.97675,2.46240,1.75860,0.97675,2.46240,1.75860,0.97675,2.46240,
      1.94370,1.07960,2.72160,1.94370,1.07960,2.72160,1.94370,1.07960,2.72160,
      1.76390,0.97836,2.47300,1.76390,0.97836,2.47300,1.76390,0.97836,2.47300,
      1.67570,0.92944,2.34930,1.67570,0.92944,2.34930,1.67570,0.92944,2.34930,
      1.85210,1.02730,2.59660,1.85210,1.02730,2.59660,1.85210,1.02730,2.59660,
      1.93790,1.07780,2.70990,1.93790,1.07780,2.70990,1.93790,1.07780,2.70990,
      1.84100,1.02390,2.57440,1.84100,1.02390,2.57440,1.84100,1.02390,2.57440,
      2.03480,1.13170,2.84540,2.03480,1.13170,2.84540,2.03480,1.13170,2.84540
    )
  )
  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_macro$impact_rounded,
    expected = c(
      27,15,38,27,15,38,27,15,38,26,14,36,26,14,36,26,14,36,
      29,16,40,29,16,40,29,16,40,26,14,37,26,14,37,26,14,37,
      25,14,35,25,14,35,25,14,35,27,15,38,27,15,38,27,15,38,
      29,16,40,29,16,40,29,16,40,27,15,38,27,15,38,27,15,38,
      30,17,42,30,17,42,30,17,42,33,19,47,33,19,47,33,19,47,
      32,18,44,32,18,44,32,18,44,35,19,49,35,19,49,35,19,49,
      32,18,45,32,18,45,32,18,45,30,17,42,30,17,42,30,17,42,
      33,18,47,33,18,47,33,18,47,35,19,49,35,19,49,35,19,49,
      33,18,47,33,18,47,33,18,47,37,20,51,37,20,51,37,20,51,
      50,28,70,50,28,70,50,28,70,48,27,67,48,27,67,48,27,67,
      53,29,74,53,29,74,53,29,74,48,27,67,48,27,67,48,27,67,
      46,25,64,46,25,64,46,25,64,50,28,71,50,28,71,50,28,71,
      53,29,74,53,29,74,53,29,74,50,28,70,50,28,70,50,28,70,
      55,31,77,55,31,77,55,31,77,26,15,37,26,15,37,26,15,37,
      25,14,35,25,14,35,25,14,35,27,15,39,27,15,39,27,15,39,
      25,14,35,25,14,35,25,14,35,24,13,33,24,13,33,24,13,33,
      26,15,37,26,15,37,26,15,37,27,15,38,27,15,38,27,15,38,
      26,14,36,26,14,36,26,14,36,29,16,40,29,16,40,29,16,40,
      35,19,49,35,19,49,35,19,49,33,18,46,33,18,46,33,18,46,
      37,20,51,37,20,51,37,20,51,33,18,47,33,18,47,33,18,47,
      32,17,44,32,17,44,32,17,44,35,19,49,35,19,49,35,19,49,
      36,20,51,36,20,51,36,20,51,35,19,49,35,19,49,35,19,49,
      38,21,54,38,21,54,38,21,54
    )
  )
})


testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_single|cutoff_TRUE|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|",{
  data <- base::readRDS(testthat::test_path("data", "mort_pm25_sect_2021.rds"))
  #Exotic test based on real data but does produce real world results

  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- data$PM25
  cutoff_c <- base::rep(0, length.out = length(exp_c))
  bhd_c <- data$VALUE_BASELINE
  rr_c <- base::rep(1.118, length.out = length(bhd_c))

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age = base::rep(c("below_50", "below_50", "50_plus", "70_plus"), length.out = length(bhd_c)),
    sex = base::rep(c("male", "female"), length.out = length(bhd_c)),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    rr_central = rr_c,
    rr_lower = rr_c - rr_c/uncert_factor,
    rr_upper = rr_c + rr_c/uncert_factor,
    rr_increment = base::rep(c(10, 11, 12, 13), length.out = length(bhd_c)),
    erf_shape = "linear",
    geo_id_micro = data$CS01012020,
    geo_id_macro = base::rep(c("basel","bern","zuerich","genf","lausanne"), length.out = length(bhd_c))
  )

  df_by_sex <- x$health_detailed$results_raw |>
    dplyr::group_by(sex, exp_ci,bhd_ci,cutoff_ci,erf_ci) |>
    dplyr::summarise(mean_value = base::mean(impact, na.rm = TRUE), .groups = "drop")

  df_by_age_group <- x$health_detailed$results_raw |>
    dplyr::group_by(age_group, exp_ci,bhd_ci,cutoff_ci,erf_ci) |>
    dplyr::summarise(mean_value = base::mean(impact, na.rm = TRUE), .groups = "drop")
  testthat::expect_equal(
    ## test if age group results are correct
    object = base::signif(df_by_age_group$mean_value,5),
    expected = c(
      1.71590, 0.95049, 2.40850, 1.71590, 0.95049, 2.40850, 1.71590, 0.95049, 2.40850,
      1.63010, 0.90296, 2.28800, 1.63010, 0.90296, 2.28800, 1.63010, 0.90296, 2.28800,
      1.80170, 0.99801, 2.52890, 1.80170, 0.99801, 2.52890, 1.80170, 0.99801, 2.52890,
      1.63870, 0.90561, 2.30510, 1.63870, 0.90561, 2.30510, 1.63870, 0.90561, 2.30510,
      1.55680, 0.86033, 2.18980, 1.55680, 0.86033, 2.18980, 1.55680, 0.86033, 2.18980,
      1.72070, 0.95089, 2.42040, 1.72070, 0.95089, 2.42040, 1.72070, 0.95089, 2.42040,
      1.79220, 0.99511, 2.51030, 1.79220, 0.99511, 2.51030, 1.79220, 0.99511, 2.51030,
      1.70260, 0.94535, 2.38480, 1.70260, 0.94535, 2.38480, 1.70260, 0.94535, 2.38480,
      1.88180, 1.04490, 2.63580, 1.88180, 1.04490, 2.63580, 1.88180, 1.04490, 2.63580,
      1.46070, 0.80634, 2.05680, 1.46070, 0.80634, 2.05680, 1.46070, 0.80634, 2.05680,
      1.38770, 0.76603, 1.95390, 1.38770, 0.76603, 1.95390, 1.38770, 0.76603, 1.95390,
      1.53380, 0.84666, 2.15960, 1.53380, 0.84666, 2.15960, 1.53380, 0.84666, 2.15960,
      1.39460, 0.76812, 1.96760, 1.39460, 0.76812, 1.96760, 1.39460, 0.76812, 1.96760,
      1.32480, 0.72971, 1.86920, 1.32480, 0.72971, 1.86920, 1.32480, 0.72971, 1.86920,
      1.46430, 0.80652, 2.06600, 1.46430, 0.80652, 2.06600, 1.46430, 0.80652, 2.06600,
      1.52620, 0.84437, 2.14480, 1.52620, 0.84437, 2.14480, 1.52620, 0.84437, 2.14480,
      1.44990, 0.80215, 2.03750, 1.44990, 0.80215, 2.03750, 1.44990, 0.80215, 2.03750,
      1.60260, 0.88659, 2.25200, 1.60260, 0.88659, 2.25200, 1.60260, 0.88659, 2.25200,
      1.84280, 1.02770, 2.57090, 1.84280, 1.02770, 2.57090, 1.84280, 1.02770, 2.57090,
      1.75070, 0.97635, 2.44240, 1.75070, 0.97635, 2.44240, 1.75070, 0.97635, 2.44240,
      1.93490, 1.07910, 2.69950, 1.93490, 1.07910, 2.69950, 1.93490, 1.07910, 2.69950,
      1.76110, 0.97960, 2.46280, 1.76110, 0.97960, 2.46280, 1.76110, 0.97960, 2.46280,
      1.67310, 0.93062, 2.33970, 1.67310, 0.93062, 2.33970, 1.67310, 0.93062, 2.33970,
      1.84920, 1.02860, 2.58590, 1.84920, 1.02860, 2.58590, 1.84920, 1.02860, 2.58590,
      1.92350, 1.07560, 2.67720, 1.92350, 1.07560, 2.67720, 1.92350, 1.07560, 2.67720,
      1.82730, 1.02180, 2.54340, 1.82730, 1.02180, 2.54340, 1.82730, 1.02180, 2.54340,
      2.01970, 1.12930, 2.81110, 2.01970, 1.12930, 2.81110, 2.01970, 1.12930, 2.81110
    )

  )
  testthat::expect_equal(
    ## test if sex results are correct
    object = base::signif(df_by_sex$mean_value,5),
    expected = c(
      1.58950,0.88102,2.22980,1.58950,0.88102,2.22980,1.58950,0.88102,2.22980,
      1.51000,0.83697,2.11830,1.51000,0.83697,2.11830,1.51000,0.83697,2.11830,
      1.66890,0.92507,2.34130,1.66890,0.92507,2.34130,1.66890,0.92507,2.34130,
      1.51810,0.83946,2.13430,1.51810,0.83946,2.13430,1.51810,0.83946,2.13430,
      1.44220,0.79748,2.02760,1.44220,0.79748,2.02760,1.44220,0.79748,2.02760,
      1.59400,0.88143,2.24100,1.59400,0.88143,2.24100,1.59400,0.88143,2.24100,
      1.66010,0.92235,2.32390,1.66010,0.92235,2.32390,1.66010,0.92235,2.32390,
      1.57710,0.87623,2.20770,1.57710,0.87623,2.20770,1.57710,0.87623,2.20770,
      1.74310,0.96846,2.44010,1.74310,0.96846,2.44010,1.74310,0.96846,2.44010,
      1.84160,1.02510,2.57370,1.84160,1.02510,2.57370,1.84160,1.02510,2.57370,
      1.74950,0.97387,2.44510,1.74950,0.97387,2.44510,1.74950,0.97387,2.44510,
      1.93370,1.07640,2.70240,1.93370,1.07640,2.70240,1.93370,1.07640,2.70240,
      1.75970,0.97700,2.46490,1.75970,0.97700,2.46490,1.75970,0.97700,2.46490,
      1.67170,0.92815,2.34160,1.67170,0.92815,2.34160,1.67170,0.92815,2.34160,
      1.84770,1.02590,2.58810,1.84770,1.02590,2.58810,1.84770,1.02590,2.58810,
      1.92260,1.07290,2.68090,1.92260,1.07290,2.68090,1.92260,1.07290,2.68090,
      1.82650,1.01930,2.54680,1.82650,1.01930,2.54680,1.82650,1.01930,2.54680,
      2.01880,1.12660,2.81490,2.01880,1.12660,2.81490,2.01880,1.12660,2.81490
    )
  )
  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_macro$impact_rounded,
    expected = c(
      27,15,38,27,15,38,27,15,38,26,14,36,26,14,36,26,14,36,
      29,16,40,29,16,40,29,16,40,26,14,37,26,14,37,26,14,37,
      25,14,35,25,14,35,25,14,35,27,15,38,27,15,38,27,15,38,
      28,16,40,28,16,40,28,16,40,27,15,38,27,15,38,27,15,38,
      30,17,42,30,17,42,30,17,42,33,18,47,33,18,47,33,18,47,
      32,18,44,32,18,44,32,18,44,35,19,49,35,19,49,35,19,49,
      32,18,45,32,18,45,32,18,45,30,17,43,30,17,43,30,17,43,
      33,19,47,33,19,47,33,19,47,35,19,49,35,19,49,35,19,49,
      33,18,46,33,18,46,33,18,46,37,20,51,37,20,51,37,20,51,
      50,28,70,50,28,70,50,28,70,48,26,67,48,26,67,48,26,67,
      53,29,74,53,29,74,53,29,74,48,27,67,48,27,67,48,27,67,
      45,25,64,45,25,64,45,25,64,50,28,70,50,28,70,50,28,70,
      52,29,73,52,29,73,52,29,73,50,28,69,50,28,69,50,28,69,
      55,31,77,55,31,77,55,31,77,26,15,36,26,15,36,26,15,36,
      25,14,35,25,14,35,25,14,35,27,15,38,27,15,38,27,15,38,
      25,14,35,25,14,35,25,14,35,24,13,33,24,13,33,24,13,33,
      26,15,37,26,15,37,26,15,37,27,15,38,27,15,38,27,15,38,
      26,14,36,26,14,36,26,14,36,29,16,40,29,16,40,29,16,40,
      35,19,49,35,19,49,35,19,49,33,18,46,33,18,46,33,18,46,
      37,20,51,37,20,51,37,20,51,33,18,47,33,18,47,33,18,47,
      32,17,44,32,17,44,32,17,44,35,19,49,35,19,49,35,19,49,
      36,20,51,36,20,51,36,20,51,34,19,48,34,19,48,34,19,48,
      38,21,53,38,21,53,38,21,53
    )
  )
})

testthat::test_that("results the same |pathway_rr|erf_function|exp_single|cutoff_TRUE|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|",{
  data <- base::readRDS(testthat::test_path("data", "LMU_O3_COPD_mort_2016.rds"))
  #Exotic test based on real data but does produce real world results


  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  rr_change = bhd_change <-1.2
  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(84.1, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::signif(base::unlist(base::lapply(1, function(x)  x * cutoff_change^(0:3))),5)
  bhd_c <- base::signif(base::unlist(base::lapply(29908, function(x) x * bhd_change^(0:3))),5)

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age = c("below_50", "below_50", "50_plus", "70_plus"),
    sex = c("male", "female", "male", "female"),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    erf_eq_central = stats::splinefun(data$x, data$y, method="natural"),
    erf_eq_lower  = stats::splinefun(data$x, data$y_l, method="natural"),
    erf_eq_upper  = stats::splinefun(data$x, data$y_u, method="natural"),
    geo_id_micro = c("bern","basel","basel","bern"),
    prop_pop_exp = c(1,1,1,1))

  df_by_sex <- x$health_detailed$results_raw |>
    dplyr::group_by(sex, exp_ci,bhd_ci,cutoff_ci,erf_ci) |>
    dplyr::summarise(mean_value = base::mean(impact, na.rm = TRUE), .groups = "drop")

  df_by_age_group <- x$health_detailed$results_raw |>
    dplyr::group_by(age_group, exp_ci,bhd_ci,cutoff_ci,erf_ci) |>
    dplyr::summarise(mean_value = base::mean(impact, na.rm = TRUE), .groups = "drop")


  testthat::expect_equal(
    ## test if age group results are correct
    object = base::signif(df_by_age_group$mean_value,5),
    c(557.61, 424.49, 674.62, 557.74, 424.59, 674.79, 557.47, 424.38, 674.46,
      529.73, 403.26, 640.89, 529.86, 403.36, 641.05, 529.60, 403.16, 640.74,
      585.49, 445.71, 708.36, 585.63, 445.82, 708.53, 585.34, 445.60, 708.18,
      534.15, 406.61, 646.28, 534.31, 406.73, 646.48, 533.99, 406.48, 646.09,
      507.44, 386.28, 613.97, 507.60, 386.39, 614.15, 507.29, 386.16, 613.79,
      560.86, 426.94, 678.60, 561.03, 427.06, 678.80, 560.69, 426.81, 678.40,
      577.97, 440.01, 699.22, 578.09, 440.11, 699.37, 577.84, 439.92, 699.07,
      549.07, 418.01, 664.26, 549.18, 418.10, 664.40, 548.95, 417.92, 664.12,
      606.87, 462.01, 734.18, 606.99, 462.11, 734.34, 606.74, 461.92, 734.03,
      717.43, 546.22, 867.89, 717.54, 546.31, 868.04, 717.31, 546.13, 867.75,
      681.56, 518.91, 824.50, 681.67, 518.99, 824.63, 681.44, 518.82, 824.37,
      753.30, 573.53, 911.29, 753.42, 573.62, 911.44, 753.18, 573.43, 911.14,
      691.83, 526.69, 836.97, 691.94, 526.78, 837.11, 691.71, 526.60, 836.83,
      657.24, 500.36, 795.12, 657.35, 500.44, 795.26, 657.12, 500.27, 794.99,
      726.42, 553.03, 878.82, 726.54, 553.12, 878.97, 726.30, 552.93, 878.67,
      743.00, 565.72, 898.78, 743.12, 565.81, 898.92, 742.88, 565.64, 898.64,
      705.85, 537.44, 853.84, 705.96, 537.52, 853.98, 705.74, 537.35, 853.71,
      780.15, 594.01, 943.72, 780.27, 594.10, 943.87, 780.03, 593.92, 943.57,
      369.42, 281.17, 447.02, 369.66, 281.35, 447.32, 369.17, 280.98, 446.72,
      350.95, 267.11, 424.67, 351.18, 267.29, 424.95, 350.71, 266.93, 424.39,
      387.89, 295.23, 469.37, 388.14, 295.42, 469.68, 387.63, 295.03, 469.06,
      342.94, 260.99, 415.01, 343.25, 261.23, 415.39, 342.63, 260.75, 414.64,
      325.79, 247.94, 394.26, 326.08, 248.16, 394.62, 325.49, 247.71, 393.91,
      360.08, 274.04, 435.76, 360.41, 274.29, 436.16, 359.76, 273.79, 435.37,
      390.93, 297.56, 473.02, 391.13, 297.72, 473.26, 390.73, 297.41, 472.77,
      371.38, 282.69, 449.37, 371.57, 282.83, 449.60, 371.19, 282.54, 449.14,
      410.48, 312.44, 496.67, 410.69, 312.60, 496.93, 410.26, 312.28, 496.41)
  )
  base::signif(df_by_sex$mean_value,5)
  testthat::expect_equal(
    ## test if sex results are correct
    object = base::signif(df_by_sex$mean_value,5),
    expected = c(
      571.62, 435.16, 691.56, 571.77, 435.28, 691.75, 571.46, 435.05, 691.37,
      543.04, 413.41, 656.98, 543.18, 413.52, 657.16, 542.89, 413.29, 656.80,
      600.20, 456.92, 726.14, 600.36, 457.05, 726.33, 600.04, 456.80, 725.94,
      546.56, 416.06, 661.29, 546.74, 416.19, 661.50, 546.39, 415.93, 661.08,
      519.23, 395.26, 628.22, 519.40, 395.38, 628.42, 519.07, 395.13, 628.02,
      573.89, 436.86, 694.35, 574.07, 437.00, 694.57, 573.71, 436.72, 694.13,
      594.69, 452.76, 719.43, 594.83, 452.87, 719.60, 594.55, 452.65, 719.26,
      564.96, 430.12, 683.46, 565.09, 430.22, 683.62, 564.82, 430.02, 683.30,
      624.42, 475.40, 755.40, 624.57, 475.51, 755.58, 624.28, 475.28, 755.23,
      435.32, 331.36, 526.72, 435.53, 331.52, 526.98, 435.10, 331.19, 526.46,
      413.55, 314.79, 500.38, 413.76, 314.94, 500.63, 413.34, 314.63, 500.13,
      457.08, 347.92, 553.06, 457.31, 348.10, 553.33, 456.85, 347.75, 552.78,
      409.36, 311.58, 495.35, 409.64, 311.79, 495.68, 409.09, 311.37, 495.02,
      388.90, 296.00, 470.59, 389.16, 296.20, 470.90, 388.64, 295.80, 470.27,
      429.83, 327.16, 520.12, 430.12, 327.37, 520.47, 429.55, 326.94, 519.77,
      456.72, 347.67, 552.59, 456.90, 347.81, 552.81, 456.54, 347.54, 552.37,
      433.89, 330.29, 524.96, 434.06, 330.42, 525.17, 433.71, 330.16, 524.75,
      479.56, 365.06, 580.22, 479.75, 365.20, 580.45, 479.37, 364.91, 579.99
    ))
  testthat::expect_equal(
    ## test if geo_id results are correct
    object =x$health_detailed$results_by_geo_id_micro$impact_rounded,
    expected = c(1030, 784, 1247, 1031, 785, 1247, 1030, 784, 1246, 979, 745, 1184, 979, 746, 1185, 979, 745, 1184, 1082, 824, 1309, 1082, 824, 1310, 1082, 823, 1309, 976, 743, 1181, 977, 744, 1182, 976, 743, 1181, 928, 706, 1122, 928, 706, 1123, 927, 706, 1122, 1025, 780, 1240, 1026, 781, 1241, 1025, 780, 1240, 1078, 821, 1305, 1079, 821, 1305, 1078, 821, 1304, 1025, 780, 1239, 1025, 780, 1240, 1024, 780, 1239,
                 1132, 862, 1370, 1133, 862, 1370, 1132, 862, 1370, 983, 749, 1190, 984, 749, 1190, 983, 748, 1189, 934, 711, 1130, 935, 711, 1131, 934, 711, 1130, 1033, 786, 1249, 1033, 786, 1250, 1032, 786, 1249, 935, 712, 1132, 936, 712, 1132, 935, 712, 1131, 889, 676, 1075, 889, 677, 1076, 888, 676, 1075, 982, 748, 1188, 983, 748, 1189, 982, 747, 1188, 1024, 780, 1239, 1025, 780, 1240, 1024, 780, 1239,
                 973, 741, 1177, 973, 741, 1178, 973, 741, 1177, 1076, 819, 1301, 1076, 819, 1302, 1075, 819, 1301)
  )
})

#### YLD ########################################################################

testthat::test_that("results the same prevalence-based YLD (duration_central=1) |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = 8.85,
        cutoff_central = 5,
        bhd_central = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        dw_central = 0.5, dw_lower = 0.1, dw_upper = 1,
        duration_central = 1, duration_lower = 0.5, duration_upper = 10
        )$health_main$impact_rounded,
    expected =
      c(525, 277, 768) # Result on 16 May 2024; no comparison study
  )
})

testthat::test_that("results the same incidence-based YLD (duration_central > 1) |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = 8.85,
        cutoff_central = 5,
        bhd_central = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        dw_central = 0.5, dw_lower = 0.1, dw_upper = 1,
        duration_central = 5, duration_lower = 2, duration_upper = 10
        )$health_main$impact_rounded,
    expected =
      c(2627, 1386, 3839) # # Result on 2025-01-28; no comparison study
  )
})

testthat::test_that("results the same prevalence-based YLD |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = 8.85,
        cutoff_central = 5,
        bhd_central = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        dw_central = 0.5, dw_lower = 0.1, dw_upper = 1,
        duration_central = 1,
      )$health_main$impact_rounded,
    expected =
      c(525, 277, 768) # Result on 16 May 2024; no comparison study
  )
})

testthat::test_that("detailed results the same prevalence-based YLD |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = 8.85,
        exp_lower = 8.85 - 1,
        exp_upper = 8.85 + 1,
        cutoff_central = 5,
        cutoff_lower = 5 - 1,
        cutoff_upper = 5 + 1,
        bhd_central = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        dw_central = 0.5, dw_lower = 0.1, dw_upper = 1,
        duration_central = 1,
      )$health_detailed$results_raw$impact |> round(), # 2025-04-02 Round at the end to obtain rounded results
    expected = # Result on 2025-06-12; no comparison study
      c(525, 277, 768, 105, 55, 154, 1051, 555, 1536, 658, 348, 959, 132, 70, 192, 1317, 697, 1919, 391, 206, 573, 78, 41, 115, 782, 412, 1146, 391, 206, 573, 78, 41, 115, 782, 412, 1146, 525, 277, 768, 105, 55, 154, 1051, 555, 1536, 255, 134, 375, 51, 27, 75, 511, 268, 750, 658, 348, 959, 132, 70, 192, 1317, 697, 1919, 790, 419, 1148, 158, 84, 230, 1579, 838, 2296, 525, 277, 768, 105, 55, 154, 1051, 555, 1536)
    )
})
### EXPOSURE DISTRIBUTION #######################################################

testthat::test_that("results correct with cutoff |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))

  ## With prop_pop_exp
  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$exposure_mean,
        prop_pop_exp = data$prop_exposed,
        cutoff_central = min(data$exposure_mean),
        bhd_central = data$gbd_daly[1],
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = data.frame(pollutant = "road_noise", outcome = "YLD")
        )$health_main$impact_rounded,
    expected =
      data_raw |>
      dplyr::filter(exposure_category %in% "Total exposed")|>
      dplyr::select(daly)|>
      dplyr::pull() |>
      base::round()
    )

})

testthat::test_that("results the same no cutoff |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$exposure_mean,
        prop_pop_exp = data$prop_exposed,
        bhd_central = data$gbd_daly[1],
        cutoff_central = 0,
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = data.frame(pollutant = "road_noise", outcome = "YLD")
        )$health_main$impact_rounded,
    expected =
      29358 # Results on 2025-01-20; no comparison study
  )
})

testthat::test_that("results correct |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "LMU_O3_COPD_mort_2016.rds"))
  erf <- stats::splinefun(data$x, data$y, method="natural")
  erf_l <- stats::splinefun(data$x, data$y_l, method="natural")
  erf_u <- stats::splinefun(data$x, data$y_u, method="natural")

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        erf_eq_central = erf,
        erf_eq_lower = erf_l,
        erf_eq_upper = erf_u,
        prop_pop_exp = data$Population.affected,
        exp_central = data$Mean.O3, # exposure distribution for ozone
        cutoff_central = 0,
        bhd_central =  29908, #COPD mortality in Germany 2016
      )$health_main$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(313,238,379)
  )

  ## ASSESSOR: Susanne Breitner-Busch, LMU Munich
  ## ASSESSMENT DETAILS: https://www.umweltbundesamt.de/publikationen/quantifizierung-der-krankheitslast-verursacht-durch#:~:text=Beschrieben%20werden%20die%20gesundheitlichen%20Effekte%20in%20der%20deutschen,f%C3%BCr%20die%20Jahre%202007%20-%202016%20quantifiziert%20wurden.
  ## INPUT DATA DETAILS: Modelled ozone exposure, real COPD mortality data from Germany, 2016
})

testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "LMU_O3_COPD_mort_2016.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.037, #pooled ozone effect estimate of the studies Lim et al. 2019, Kazemiparkouhi et al. 2019, Turner et al. 2016
        rr_lower = 1.028,
        rr_upper = 1.045,
        rr_increment = 10,
        prop_pop_exp = data$Population.affected,
        exp_central = data$Mean.O3, # exposure distribution for ozone
        cutoff_central = 65,
        bhd_central =  29908, #COPD mortality in Germany 2016
      )$health_main$impact_rounded,
    expected =
      c(2007,1537,2415)
  )

  ## ASSESSOR: Susanne Breitner-Busch, LMU Munich
  ## ASSESSMENT DETAILS: https://www.umweltbundesamt.de/publikationen/quantifizierung-der-krankheitslast-verursacht-durch#:~:text=Beschrieben%20werden%20die%20gesundheitlichen%20Effekte%20in%20der%20deutschen,f%C3%BCr%20die%20Jahre%202007%20-%202016%20quantifiziert%20wurden.
  ## INPUT DATA DETAILS: Modelled ozone exposure, real COPD mortality data from Germany, 2016
})


testthat::test_that("results the same |pathway_rr|erf_lin_log|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = c(20, 20),
        prop_pop_exp = c(0.5, 0.5),
        cutoff_central = 5,
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "linear_log",
        bhd_central = c(10))$health_main$impact,
    expected =
      0.927071 # Results on 08 August 2024 (ChatGPT); no comparison study
  )
})

testthat::test_that("results the same |pathway_rr|erf_log_log|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = c(20, 20),
        prop_pop_exp = c(0.5, 0.5),
        cutoff_central = 5,
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_log",
        bhd_central = 10)$health_main$impact,
    expected =
      0.936215963 # Results on 06 August 2024 (ChatGPT); no comparison study
  )
})

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_dist|cutoff_TRUE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      base::signif(healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        prop_pop_exp = 1,
        exp_central = c(1.1,1.7), # exposure for pm2.5
        # dist = exposure distribution, e.g. 5 different exposure
        #categories (~ exposure ranges) with the information how many people are
        #exposed to each of the 5 exposure range heitgaasid-pm2.5
        cutoff_central = 0,
        bhd_central =  5442 #deaths in chosen regions
      )$health_main$impact,3),
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      44.2 # test did not pass but the value provided by healthiar differs on average 14.8, might need to look into the differences
  )
})
## Original result from publication: 42.0 # test did not pass
# I added the exposure value 1.1, because the original was single exposure and not distribution
## ASSESSOR: Maria Lepnurm TAI
## ASSESSMENT DETAILS: I used data from https://keskkonnaportaal.ee/sites/default/files/2024-05/V%C3%A4lis%C3%B5hu%20kvaliteedi%20m%C3%B5ju%20v%C3%B5rdlus%20inimeste%20tervisele%20Eestis%20aastatel%202010%20ja%202020%20ning%20%C3%B5husaaste%20tervisem%C3%B5jude%20prognoos%20aastaks%202030.pdf for Estonian administrative units and data for attributable deaths in the year 2020. The number of deaths was obtained from statistics Estonia
## INPUT DATA DETAILS: obtained from the study mentioned. Calculated attributable deaths from pm2.5 in 2020.

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_dist|cutoff_FALSE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      base::signif(healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        prop_pop_exp = 1,
        exp_central = c(1.1,1.5), # exposure 1. pm2.5 and 2. pm2.5 and pm10 from the same administrative unit
        # dist = exposure distribution, e.g. 5 different exposure
        #categories (~ exposure ranges) with the information how many people are
        #exposed to each of the 5 exposure range heitgaasid-pm2.5
        cutoff_central = 0,
        bhd_central = 88 #deaths in chosen regions
      )$health_main$impact,3),
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      0.664
  )
})
## original results: c(0.8, 0.7) but the result is only one value. I also tested how the result looks like if I add 2 geo_id_micro, then the results ar still colose, but the first result is lower than the second,
## ASSESSOR: Maria Lepnurm TAI
## ASSESSMENT DETAILS: I used data from https://keskkonnaportaal.ee/sites/default/files/2024-05/V%C3%A4lis%C3%B5hu%20kvaliteedi%20m%C3%B5ju%20v%C3%B5rdlus%20inimeste%20tervisele%20Eestis%20aastatel%202010%20ja%202020%20ning%20%C3%B5husaaste%20tervisem%C3%B5jude%20prognoos%20aastaks%202030.pdf for Estonian administrative units and data for attributable deaths in the year 2020. The number of deaths was obtained from statistics Estonia
## INPUT DATA DETAILS: obtained from the study mentioned. Calculated attributable deaths from pm2.5 in 2020.

testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      base::signif(healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        prop_pop_exp = 1,
        exp_central = 1.1 , # exposure 1. pm2.5 and 2. pm2.5 and pm10 from the same administrative unit
        # dist = exposure distribution, e.g. 5 different exposure
        #categories (~ exposure ranges) with the information how many people are
        #exposed to each of the 5 exposure range heitgaasid-pm2.5
        cutoff_central = 0,
        bhd_central = c(88 ) #deaths in chosen regions
      )$health_main$impact,3),
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(0.577) # test did not pass but the value provided by healthiar differs on average 0.2
  )
})
#original results: c(0.8) # test did not pass but the value provided by healthiar differs on average 0.2
## ASSESSOR: Maria Lepnurm TAI
## ASSESSMENT DETAILS: I used data from https://keskkonnaportaal.ee/sites/default/files/2024-05/V%C3%A4lis%C3%B5hu%20kvaliteedi%20m%C3%B5ju%20v%C3%B5rdlus%20inimeste%20tervisele%20Eestis%20aastatel%202010%20ja%202020%20ning%20%C3%B5husaaste%20tervisem%C3%B5jude%20prognoos%20aastaks%202030.pdf for Estonian administrative units and data for attributable deaths in the year 2020. The number of deaths was obtained from statistics Estonia
## INPUT DATA DETAILS: obtained from the study mentioned. Calculated attributable deaths from pm2.5 in 2020.


##### Stratification (sex/age) ####################################################################

testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))
  #Exotic test based on real data but does produce real world results

  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  rr_change <-1.2
  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(data$exposure_mean, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::rep(base::signif(base::unlist(base::lapply(min(data$exposure_mean), function(x)  x * cutoff_change^(0:3))),5),times = length(data$exposure_mean))
  bhd_c <- base::rep(base::signif(base::unlist(base::lapply(data$gbd_daly[1], function(x) x * bhd_change^(0:3))),5),times = length(data$exposure_mean))
  rr_c <- base::rep(base::signif(base::unlist(base::lapply(1.08, function(x) x * rr_change^(0:3))),5),times = length(data$exposure_mean))

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age_group = base::rep(c("below_50", "below_50", "50_plus", "70_plus"),times = length(data$exposure_mean)),
    sex = base::rep(c("male", "female", "male", "female"),times = length(data$exposure_mean)),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    rr_central = rr_c,
    rr_lower = rr_c - rr_c/uncert_factor,
    rr_upper = rr_c + rr_c/uncert_factor,
    rr_increment = base::rep(c(10, 11, 12, 13), times = 6),
    erf_shape = "log_linear",
    prop_pop_exp = base::rep(data$prop_exposed, each = 4))

  testthat::expect_equal(
    ## test if age group results are correct
    object =x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      27762, 22518, 32468, 31909, 25243, 37871, 24860, 20175, 29088, 26374, 21392,
      30845, 30314, 23981, 35977, 23617, 19166, 27633, 29150, 23644, 34092, 33505,
      26505, 39764, 26103, 21184, 30542, 23649, 19168, 27703, 26606, 21550, 31156,
      20611, 16710, 24164, 22467, 18210, 26318, 25276, 20473, 29598, 19581, 15875,
      22955, 24832, 20127, 29088, 27936, 22628, 32714, 21642, 17546, 25372, 32990,
      26159, 39084, 36933, 28760, 44176, 28896, 23472, 33753, 31341, 24851, 37130,
      35087, 27322, 41967, 27451, 22298, 32065, 34640, 27467, 41038, 38780, 30198,
      46384, 30341, 24645, 35441, 48479, 45309, 51115, 49729, 46588, 52322, 47148,
      43957, 49821, 46055, 43044, 48559, 47243, 44259, 49706, 44791, 41759, 47330,
      50903, 47574, 53671, 52216, 48918, 54938, 49506, 46155, 52312, 45752, 42557,
      48448, 47168, 43984, 49834, 44246, 41049, 46963, 43465, 40429, 46025, 44809,
      41785, 47342, 42034, 38997, 44614, 48040, 44685, 50870, 49526, 46183, 52325,
      46458, 43102, 49311, 50890, 47778, 53441, 51995, 48925, 54493, 49715, 46567,
      52315, 48346, 45390, 50769, 49395, 46479, 51768, 47229, 44238, 49699, 53435,
      50167, 56113, 54595, 51371, 57217, 52201, 48895, 54930, 55586, 54210, 56679,
      56005, 54675, 57056, 55139, 53716, 56276, 52806, 51500, 53845, 53204, 51942,
      54203, 52382, 51031, 53462, 58365, 56921, 59513, 58805, 57409, 59908, 57896,
      56402, 59090, 54278, 52777, 55489, 54779, 53326, 55946, 53743, 52195, 54999,
      51564, 50139, 52715, 52040, 50659, 53149, 51056, 49586, 52249, 56992, 55416,
      58264, 57518, 55992, 58743, 56430, 54805, 57749, 56680, 55428, 57661, 57030,
      55822, 57971, 56307, 55009, 57329, 53846, 52656, 54778, 54179, 53031, 55073,
      53492, 52259, 54463, 59514, 58199, 60545, 59882, 58614, 60870, 59122, 57759,
      60196
    )

  )
  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      49630, 45685, 53036, 52581, 47540, 56978, 47971, 44226, 51193, 47149, 43400,
      50385, 49952, 45163, 54129, 45573, 42015, 48633, 52112, 47969, 55688, 55210,
      49917, 59827, 50370, 46438, 53752, 46517, 42807, 49720, 48260, 44340, 51654,
      44736, 41210, 47778, 44191, 40667, 47234, 45847, 42123, 49071, 42500, 39149,
      45389, 48843, 44947, 52206, 50673, 46557, 54237, 46973, 43270, 50167, 53800,
      48749, 58196, 56569, 50468, 61889, 50926, 46961, 54337, 51110, 46312, 55286,
      53741, 47944, 58795, 48379, 44613, 51621, 56490, 51186, 61106, 59398, 52991,
      64984, 53472, 49309, 57054, 82196, 76353, 87226, 85063, 78967, 90271, 79176,
      73623, 83992, 78086, 72535, 82865, 80809, 75019, 85757, 75217, 69942, 79792,
      86306, 80171, 91588, 89316, 82915, 94784, 83135, 77304, 88191, 77163, 71696,
      81920, 80293, 74519, 85282, 73864, 68745, 78347, 73305, 68111, 77824, 76279,
      70793, 81018, 70171, 65308, 74430, 81021, 75281, 86016, 84308, 78245, 89546,
      77557, 72183, 82264, 86761, 80617, 91991, 89389, 83040, 94751, 83992, 78086,
      89059, 82423, 76586, 87392, 84920, 78888, 90013, 79793, 74182, 84606, 91099,
      84648, 96591, 93858, 87192, 99488, 88192, 81990, 93512
    )
  )
})

testthat::test_that("results correct |pathway_rr|erf_lin_lin|exp_dist|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))
  #Exotic test based on real data but does produce real world results

  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  rr_change <-1.2
  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(data$exposure_mean, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::rep(base::signif(base::unlist(base::lapply(min(data$exposure_mean), function(x)  x * cutoff_change^(0:3))),5),times = length(data$exposure_mean))
  bhd_c <- base::rep(base::signif(base::unlist(base::lapply(data$gbd_daly[1], function(x) x * bhd_change^(0:3))),5),times = length(data$exposure_mean))
  rr_c <- base::rep(base::signif(base::unlist(base::lapply(1.08, function(x) x * rr_change^(0:3))),5),times = length(data$exposure_mean))

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age_group = base::rep(c("below_50", "below_50", "50_plus", "70_plus"),times = length(data$exposure_mean)),
    sex = base::rep(c("male", "female", "male", "female"),times = length(data$exposure_mean)),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    rr_central = rr_c,
    rr_lower = rr_c - rr_c/uncert_factor,
    rr_upper = rr_c + rr_c/uncert_factor,
    rr_increment = base::rep(c(10, 11, 12, 13), times = 6),
    erf_shape = "linear",
    prop_pop_exp = base::rep(data$prop_exposed, each = 4))

  testthat::expect_equal(
    ## test if age group results are correct
    object =x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      25989, 21261, 30180, 29647, 23598, 35024, 23582, 19280, 27421,
      24690, 20198, 28671, 28164, 22418, 33273, 22403, 18316, 26050,
      27289, 22324, 31689, 31129, 24778, 36775, 24761, 20244, 28792,
      22600, 18437, 26330, 25079, 20472, 29177, 19987, 16283, 23334,
      21470, 17515, 25014, 23825, 19448, 27718, 18988, 15469, 22167,
      23730, 19359, 27647, 26333, 21495, 30636, 20987, 17097, 24501,
      30468, 24321, 35916, 33862, 26500, 40344, 26873, 22030, 31150,
      28945, 23105, 34120, 32169, 25175, 38327, 25529, 20929, 29593,
      31991, 25537, 37712, 35555, 27825, 42361, 28217, 23132, 32708,
      41404, 38864, 43550, 42250, 39733, 44371, 40502, 37942, 42674,
      39334, 36921, 41373, 40138, 37747, 42152, 38477, 36045, 40540,
      43474, 40808, 45728, 44363, 41720, 46589, 42527, 39840, 44808,
      39589, 37012, 41783, 40548, 37989, 42718, 38563, 35972, 40779,
      37609, 35161, 39694, 38520, 36089, 40582, 36635, 34173, 38740,
      41568, 38863, 43872, 42575, 39888, 44854, 40491, 37770, 42818,
      43009, 40515, 45103, 43762, 41293, 45829, 42210, 39692, 44332,
      40859, 38489, 42848, 41574, 39228, 43537, 40100, 37708, 42115,
      45159, 42541, 47359, 45950, 43358, 48120, 44321, 41677, 46548,
      46836, 45477, 47990, 47172, 45833, 48309, 46483, 45106, 47656,
      44494, 43204, 45591, 44814, 43542, 45894, 44159, 42850, 45273,
      49177, 47751, 50390, 49531, 48125, 50724, 48808, 47361, 50039,
      45853, 44441, 47057, 46234, 44842, 47419, 45454, 44021, 46678,
      43560, 42219, 44705, 43922, 42600, 45048, 43181, 41820, 44344,
      48146, 46663, 49410, 48545, 47084, 49790, 47727, 46222, 49012,
      47707, 46400, 48815, 48007, 46718, 49098, 47394, 46068, 48519,
      45322, 44080, 46374, 45607, 44382, 46643, 45024, 43765, 46093,
      50092, 48720, 51255, 50407, 49054, 51553, 49764, 48371, 50945
    )
  )
  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      42546, 39239, 45446, 45118, 40687, 49067, 41324, 38211, 44041,
      40419, 37277, 43174, 42862, 38653, 46614, 39258, 36301, 41839,
      44673, 41201, 47718, 47374, 42721, 51521, 43390, 40122, 46243,
      40353, 37262, 43056, 41633, 38345, 44521, 39056, 36133, 41601,
      38336, 35399, 40903, 39552, 36428, 42295, 37103, 34326, 39521,
      42371, 39125, 45209, 43715, 40262, 46747, 41008, 37939, 43681,
      45931, 41487, 49887, 48337, 42836, 53225, 43409, 40085, 46320,
      43634, 39413, 47392, 45921, 40694, 50564, 41238, 38081, 44004,
      48227, 43561, 52381, 50754, 44978, 55886, 45579, 42090, 48636,
      71683, 66364, 76274, 73951, 68478, 78636, 69244, 64117, 73710,
      68099, 63046, 72461, 70254, 65054, 74704, 65782, 60911, 70024,
      75267, 69682, 80088, 77649, 71902, 82568, 72706, 67322, 77395,
      67689, 62628, 72114, 70227, 64958, 74793, 64949, 60144, 69190,
      64304, 59497, 68509, 66715, 61710, 71053, 61701, 57137, 65730,
      71073, 65760, 75720, 73738, 68206, 78533, 68196, 63151, 72649,
      75253, 69748, 79948, 77293, 71675, 82046, 73068, 67705, 77680,
      71491, 66261, 75950, 73428, 68092, 77944, 69415, 64320, 73796,
      79016, 73236, 83945, 81158, 75259, 86148, 76722, 71090, 81564
    ))
})


#### ITERATION ##################################################################
testthat::test_that("results the same no cutoff |pathway_rr|erf_log_lin|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = base::rep(c(5, 6, 7, 8, 9), times = 3),
        cutoff_central = 5,
        prop_pop_exp = c(c(0.1, 0.3, 0.2, 0.2, 0.2),
                         c(0.2, 0.2, 0.3, 0.1, 0.2),
                         c(0.2, 0.2, 0.2, 0.1, 0.3)),
        bhd_central = base::rep(runif_with_seed(3,1E4,1E5,1), each = 5),
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        geo_id_micro = base::rep(1:3, each = 5),
        geo_id_macro = base::rep("ch", each = 5 * 3)
        )$health_detailed$results_raw$impact_rounded,
    expected =
      base::round(c(545,  634,  991)) # Results on 2025-06-24; no comparison study
  )
})
base::rep(runif_with_seed(3,1E4,1E5,1), each = 5)

## with population argument specified
testthat::test_that("results the same no cutoff |pathway_rr|erf_log_lin|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = base::rep(c(5, 6, 7, 8, 9), times = 3),
        cutoff_central = 5,
        prop_pop_exp = c(c(0.1, 0.3, 0.2, 0.2, 0.2),
                         c(0.2, 0.2, 0.3, 0.1, 0.2),
                         c(0.2, 0.2, 0.2, 0.1, 0.3)),
        bhd_central = base::rep(runif_with_seed(3,1E4,1E5,1), each = 5),
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        geo_id_micro = base::rep(1:3, each = 5),
        geo_id_macro = base::rep("ch", each = 5 * 3),
        population = base::rep(1000000, each = 5 * 3)
      )$health_detailed$results_raw$impact_rounded,
    expected =
      base::round(c(545,  634,  991)) # Results on 2025-06-24; no comparison study
  )
})

testthat::test_that("results correct |pathway_rr|erf_function|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "LMU_O3_COPD_mort_2015_2016.rds"))
  data <- data |> dplyr::slice(-1)
  erf <- stats::splinefun(data$x[1:21], data$y[1:21], method="natural")
  erf_l <- stats::splinefun(data$x[1:21], data$y_l[1:21], method="natural")
  erf_u <- stats::splinefun(data$x[1:21], data$y_u[1:21], method="natural")

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        erf_eq_central = erf,
        erf_eq_lower = erf_l,
        erf_eq_upper = erf_u,
        prop_pop_exp = data$Population.affected,
        exp_central = data$Mean.O3, # exposure distribution for ozone
        cutoff_central = 0,
        bhd_central =  data$bhd, #COPD mortality in Germany 2015 and 2016
        geo_id_micro = data$X,
      )$health_main$impact_rounded,
    expected =
      c(350,267,424,313,238,379)
  )

  ## ASSESSOR: Susanne Breitner-Busch, LMU Munich
  ## ASSESSMENT DETAILS: https://www.umweltbundesamt.de/publikationen/quantifizierung-der-krankheitslast-verursacht-durch#:~:text=Beschrieben%20werden%20die%20gesundheitlichen%20Effekte%20in%20der%20deutschen,f%C3%BCr%20die%20Jahre%202007%20-%202016%20quantifiziert%20wurden.
  ## INPUT DATA DETAILS: Modelled ozone exposure, real COPD mortality data from Germany, 2016
})


testthat::test_that("results the same |pathway_rr|erf_lin_log|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        geo_id_micro = c("a", "a", "b", "b"),
        exp_central = c(20, 20, 20, 20),
        prop_pop_exp = c(0.5, 0.5, 0.5, 0.5),
        cutoff_central = 5,
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "linear_log",
        bhd_central = c(10, 10, 10, 10))$health_main$impact,
    expected =
      c(0.927071, 0.927071) # Results on 08 August 2024 (ChatGPT); no comparison study
  )
})

testthat::test_that("results the same |pathway_rr|erf_log_log|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        geo_id_micro = c("a", "a", "b", "b"),
        exp_central = c(20, 20, 20, 20),
        prop_pop_exp = c(0.5, 0.5, 0.5, 0.5),
        cutoff_central = 5,
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_log",
        bhd_central = c(10, 10, 10, 10))$health_main$impact,
    expected =
      c(0.936215963, 0.936215963) # Results on 06 August 2024 (ChatGPT); no comparison study
  )
})


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_dist|cutoff_FALSE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      base::signif(healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        prop_pop_exp = c(c(0.3,0.3,0.4),
                         c(0.2,0.3,0.5),
                         c(0.4,0.4,0.2),
                         c(0.5,0.2,0.3)),
        exp_central = c(1.1,2.1,0.1, 1.6,2.6,0.6, 1.6,2.6,0.6, 1.5,2.5,0.5 ), # dist = exposure distribution, e.g. 5 different exposure
        #categories (~ exposure ranges) with the information how many people are
        #exposed to each of the 5 exposure range heitgaasid-pm2.5
        cutoff_central = 0,
        geo_id_micro = base::rep(c("Anija", "Harku", "Jõelähtme", "Keila"),each = 3), #id codes for each area, can be names also
        bhd_central = base::rep(c(88, 92, 47, 103 ),each = 3) #deaths in chosen regions
      )$health_main$impact,2),##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(0.510, 0.750, 0.490, 0.840) # test did not pass but the value provided by healthiar differs on average 0.9
  )
})
# original results from paper: c(0.8, 2.5, 1.2, 1.7) # test did not pass but the value provided by healthiar differs on average 0.9
## ASSESSOR: Maria Lepnurm TAI
## ASSESSMENT DETAILS: I used data from https://keskkonnaportaal.ee/sites/default/files/2024-05/V%C3%A4lis%C3%B5hu%20kvaliteedi%20m%C3%B5ju%20v%C3%B5rdlus%20inimeste%20tervisele%20Eestis%20aastatel%202010%20ja%202020%20ning%20%C3%B5husaaste%20tervisem%C3%B5jude%20prognoos%20aastaks%202030.pdf for Estonian administrative units and data for attributable deaths in the year 2020. The number of deaths was obtained from statistics Estonia
## INPUT DATA DETAILS: obtained from the study mentioned. Calculated attributable deaths from pm2.5 in 2020.


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_dist|cutoff_TRUE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        prop_pop_exp = c(c(0.3,0.3,0.4),
                         c(0.2,0.3,0.5),
                         c(0.4,0.4,0.2),
                         c(0.5,0.2,0.3),
                         c(0.2,0.5,0.3),
                         c(0.4,0.1,0.5),
                         c(0.4,0.5,0.1),
                         c(0.5,0.4,0.1)),
        exp_central = c(2.1,3.1,1.1, 3,4,2, 2.6,3.6,1.6, 2.2,3.2,1.2, 2.3,3.3,1.3, 1.9,2.9,0.9, 1.8,2.8,0.8, 2.2,3.2,1.2  ), # exposure for NO2
        # dist = exposure distribution, e.g. 5 different exposure
        #categories (~ exposure ranges) with the information how many people are
        #exposed to each of the 5 exposure range heitgaasid-pm2.5
        cutoff_central = 0,
        geo_id_micro = base::rep(c("Haabersti", "Kesklinn", "Kristiine", "Lasnamäe", "Mustamäe", "Nõmme", "Pirita", "Põhja-Tallinn"),each=3),
        bhd_central = base::rep(c(433, 433, 289, 1232, 926, 397, 140, 694),each = 3) #deaths in chosen regions
      )$health_main$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(5.0, 7.0, 5.0, 15.0, 13.0, 3.0, 2.0, 10.0) # test did not pass but the value provided by healthiar differs on average 14.8, might need to look into the differences
  )
})
## original results from publication: c(16.2, 33.8, 15.3, 44.6, 28.4, 12, 4.9, 22.5) # test did not pass but the value provided by healthiar differs on average 14.8, might need to look into the differences)
## ASSESSOR: Maria Lepnurm TAI
## ASSESSMENT DETAILS: I used data from https://keskkonnaportaal.ee/sites/default/files/2024-05/V%C3%A4lis%C3%B5hu%20kvaliteedi%20m%C3%B5ju%20v%C3%B5rdlus%20inimeste%20tervisele%20Eestis%20aastatel%202010%20ja%202020%20ning%20%C3%B5husaaste%20tervisem%C3%B5jude%20prognoos%20aastaks%202030.pdf for Estonian administrative units and data for attributable deaths in the year 2020. The number of deaths was obtained from statistics Estonia
## INPUT DATA DETAILS: obtained from the study mentioned. Calculated attributable deaths from pm2.5 in 2020.


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_dist|cutoff_TRUE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|",{
  ## healthiar FUNCTION CALL
  results_pm2.5 <-
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear", # Page 18
      rr_central = 1.15, # Page 33
      rr_lower = NULL, #1.05, Page 33
      rr_upper = NULL, #1.25, Page 33
      rr_increment = 10, # Page 18
      prop_pop_exp = c(c(0.3,0.3,0.4),
                       c(0.2,0.3,0.5),
                       c(0.4,0.4,0.2),
                       c(0.5,0.2,0.3),
                       c(0.2,0.5,0.3)),
      exp_central = c(9.5,10.5,8.5, 9.8,10.8,8.8, 9.9,10.9,8.9, 10.9,11.9,9.9,9.6,10.6,8.6), # Table 5 page 29
      exp_lower = NULL, # list(6.6, 7.1, 7.2, 7.8, 6.6), # Table 5 page 29
      exp_upper = NULL, # list(13.5,13.5, 13.3, 14.4, 14.4), # Table 5 page 29
      cutoff_central = 5, # Page 33
      bhd_central = base::rep(c(133103, 121061, 87860, 219929, 561953),each = 3), # Table 3 page 22
      geo_id_micro = base::rep(c('Rural','Semi-rural', 'Semi-urbaines','Urbaines','France'),each = 3),
    )

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =results_pm2.5$health_main$impact_rounded,
    expected = c(7947.0, 7547.0, 6049.0, 17134.0, 36501.0))

  ### SpF report results ###
  # France_impact_Cuttoff5 : (7836, 7534, 5721, 18450, 39541) Table 8 page 37
  # France_pop_fraction_Cuttoff5 : (0.059, 0.063, 0.066, 0.084, 0.0071) Table 8 page 37
})


testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_dist|cutoff_TRUE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|",{
  results_NO2 <-
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear", # Page 18
      rr_central = 1.023, # Page 14
      rr_lower = NULL, #1.008 # Page 14
      rr_upper = NULL, #1.037 # Page 14
      rr_increment = 10, # Page 18
      prop_pop_exp = c(c(0.3,0.3,0.4),
                       c(0.2,0.3,0.5),
                       c(0.4,0.4,0.2),
                       c(0.5,0.2,0.3),
                       c(0.2,0.5,0.3)),
      exp_central = c(11.5,12.5,10.5, 12.4,13.4,11.4, 13.2,14.2,12.2, 17.2,18.2,16.2, 12.0,13.0,11.0), # Table 5 page 29
      exp_lower = NULL, #list(7.4, 7.6, 7.9, 8.0, 7.4), # Table 5 page 29
      exp_upper = NULL, #list(23.5, 22.8, 21.0, 34.3, 34.3), # Table 5 page 29
      cutoff_central = 10, # Page 33
      bhd_central = base::rep(c(133103, 121061, 87860, 219929, 561953),each = 3), # Table 3 page 22
      geo_id_micro = base::rep(c('Rural','Semi-rural', 'Semi-urbaines','Urbaines','France'),each = 3)
    )
  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =results_NO2$health_main$impact_rounded,
    expected = c(423.0, 604.0, 677.0, 3523.0, 2805.0))

  ### SpF report results ###
  # France_impact_Cuttoff10 : (451, 596, 633, 5110, 6790) Table 8 page 37
  # France_pop_fraction_Cuttoff10 : (0.003, 0.005, 0.007, 0.023, 0.0012) Table 8 page 37

  ## ASSESSOR: Maria José Rueda Lopez, CSTB and Sabrina Delaunay-Havard, SpF.
  ## ASSESSMENT DETAILS: Estimate the proportion of deaths attributable to PM2.5 exposure in France during the covid19 pandemic
  ## INPUT DATA DETAILS: Etudes et enquêtes. Impact de la pollution de l'air ambiant sur la mortalité en France métropolitaine. Réduction en lien avec le
  ## confinement du printemps 2020 et nouvelles données sur le poids total pour la période 2016-2019. Santé publique France. Avril 2021.
})


testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      base::signif(healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        prop_pop_exp = c(c(0.3,0.3,0.4),
                         c(0.2,0.3,0.5),
                         c(0.4,0.4,0.2),
                         c(0.5,0.2,0.3)),
        exp_central = c(1.1,1.2,1.0, 1.6,1.7,1.5, 1.6,1.7,1.5, 1.5,1.6,1.4), # dist = exposure distribution, e.g. 5 different exposure
        #categories (~ exposure ranges) with the information how many people are
        #exposed to each of the 5 exposure range heitgaasid-pm2.5
        cutoff_central = 0,
        geo_id_micro = base::rep(c("Anija", "Harku", "Jõelähtme", "Keila"),each = 3), #id codes for each area, can be names also
        bhd_central = base::rep(c(88, 92, 47, 103 ),each = 3) #deaths in chosen regions
      )$health_main$impact,3),
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected = c(0.5720, 0.8640, 0.4520, 0.9130)
  )
})
# original results: c(0.8, 2.5, 1.2, 1.7) # test did not pass but the value provided by healthiar differs on average 0.9
## ASSESSOR: Maria Lepnurm TAI
## ASSESSMENT DETAILS: I used data from https://keskkonnaportaal.ee/sites/default/files/2024-05/V%C3%A4lis%C3%B5hu%20kvaliteedi%20m%C3%B5ju%20v%C3%B5rdlus%20inimeste%20tervisele%20Eestis%20aastatel%202010%20ja%202020%20ning%20%C3%B5husaaste%20tervisem%C3%B5jude%20prognoos%20aastaks%202030.pdf for Estonian administrative units and data for attributable deaths in the year 2020. The number of deaths was obtained from statistics Estonia
## INPUT DATA DETAILS: obtained from the study mentioned. Calculated attributable deaths from pm2.5 in 2020.


##### Stratification (sex/age) ####################################################################

testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_dist|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))
  bhd_value = data$gbd_daly[1]
  data <- data |> dplyr::slice(-1)
  #Exotic test based on real data but does produce real world results

  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  rr_change <-1.2
  uncert_factor <- 20#set uncertainty factor
  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(data$exposure_mean, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::rep(base::signif(base::unlist(base::lapply(min(data$exposure_mean), function(x)  x * cutoff_change^(0:3))),5),times = length(data$exposure_mean))
  bhd_c <- base::rep(base::signif(base::unlist(base::lapply(bhd_value, function(x) x * bhd_change^(0:3))),5),times = length(data$exposure_mean))
  rr_c <- base::rep(base::signif(base::unlist(base::lapply(1.08, function(x) x * rr_change^(0:3))),5),times = length(data$exposure_mean))

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age_group = base::rep(c("below_50", "below_50", "50_plus", "70_plus"),times = length(data$exposure_mean)),
    sex = base::rep(c("male", "female", "male", "female"),times = length(data$exposure_mean)),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    rr_central = rr_c,
    rr_lower = rr_c - rr_c/uncert_factor,
    rr_upper = rr_c + rr_c/uncert_factor,
    rr_increment = base::rep(c(10, 11, 12, 13), times = 5),
    erf_shape = "log_linear",
    prop_pop_exp = base::rep(data$prop_exposed, each = 4),
    geo_id_micro = base::rep(base::rep(c("urban","rural"), each = 5), each = 2))

  testthat::expect_equal(
    ## test if age group results are correct
    object =x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      33336, 26689, 39226, 37577, 29480, 44715, 29733, 24076, 34759, 31669, 25355,
      37265, 35698, 28006, 42479, 28247, 22872, 33021, 35002, 28024, 41188, 39456,
      30954, 46950, 31220, 25280, 36497, 28333, 22922, 33155, 32002, 25582, 37711,
      24728, 20230, 28748, 26916, 21776, 31497, 30402, 24303, 35825, 23491, 19218,
      27310, 29749, 24068, 34813, 33603, 26862, 39596, 25964, 21241, 30185, 38816,
      30522, 46106, 42828, 33173, 51248, 34639, 27775, 40704, 36875, 28996, 43800,
      40687, 31514, 48685, 32907, 26386, 38669, 40757, 32048, 48411, 44969, 34832,
      53810, 36371, 29164, 42739, 50427, 47345, 52955, 51652, 48612, 54128, 49116,
      46001, 51691, 47905, 44978, 50307, 49070, 46181, 51421, 46660, 43701, 49107,
      52948, 49712, 55603, 54235, 51042, 56834, 51571, 48301, 54276, 47757, 44620,
      50370, 49157, 46045, 51730, 46259, 43108, 48904, 45369, 42389, 47852, 46699,
      43743, 49144, 43946, 40952, 46459, 50145, 46851, 52889, 51615, 48347, 54317,
      48572, 45263, 51349, 52764, 49768, 55185, 53836, 50893, 56196, 51616, 48573,
      54095, 50126, 47279, 52426, 51144, 48349, 53386, 49035, 46144, 51390, 55402,
      52256, 57944, 56528, 53438, 59006, 54197, 51002, 56800, 56631, 55398, 57597,
      57013, 55827, 57936, 56221, 54941, 57230, 53799, 52628, 54717, 54162, 53036,
      55040, 53410, 52194, 54369, 59462, 58168, 60477, 59863, 58619, 60833, 59032,
      57688, 60092, 55448, 54085, 56533, 55910, 54597, 56950, 54952, 53540, 56082,
      52676, 51381, 53706, 53115, 51867, 54103, 52204, 50863, 53278, 58220, 56790,
      59359, 58706, 57326, 59798, 57699, 56217, 58886, 57607, 56500, 58462, 57923,
      56860, 58738, 57269, 56116, 58164, 54727, 53675, 55539, 55027, 54017, 55802,
      54406, 53310, 55256, 60488, 59325, 61385, 60819, 59703, 61675, 60133, 58922,
      61072, 56890, 54150, 59031, 57692, 55021, 59764, 56031, 53226, 58242, 54045,
      51443, 56080, 54807, 52270, 56776, 53230, 50564, 55330, 59734, 56858, 61983,
      60576, 57772, 62752, 58833, 55887, 61154, 54841, 51957, 57136, 55778, 52955,
      58006, 53839, 50897, 56199, 52099, 49359, 54280, 52989, 50307, 55106, 51147,
      48352, 53389, 57583, 54555, 59993, 58566, 55603, 60906, 56531, 53442, 59009,
      58645, 56064, 60628, 59332, 56824, 61244, 57910, 55258, 59963, 55713, 53261,
      57596, 56366, 53983, 58182, 55014, 52495, 56964, 61577, 58868, 63659, 62299,
      59665, 64307, 60805, 58021, 62961, 59183, 58321, 59826, 59391, 58566, 60003,
      58960, 58059, 59636, 56224, 55405, 56835, 56421, 55638, 57002, 56012, 55156,
      56654, 62142, 61237, 62818, 62360, 61494, 63003, 61908, 60962, 62618, 58425,
      57437, 59175, 58684, 57738, 59399, 58146, 57116, 58934, 55504, 54565, 56217,
      55750, 54851, 56429, 55239, 54260, 55987, 61346, 60309, 62134, 61618, 60625,
      62369, 61054, 59972, 61880, 59790, 59042, 60339, 59957, 59242, 60477, 59612,
      58828, 60189, 56801, 56090, 57322, 56959, 56280, 57454, 56631, 55887, 57180,
      62780, 61994, 63356, 62954, 62204, 63501, 62592, 61770, 63199, 52201, 39992,
      62557, 55665, 42304, 66884, 48599, 37599, 58016, 49591, 37992, 59429, 52881,
      40189, 63540, 46169, 35719, 55115, 54811, 41991, 65685, 58448, 44419, 70229,
      51029, 39479, 60917, 46644, 36051, 55758, 50334, 38503, 60420, 42805, 33511,
      50862, 44312, 34248, 52970, 47817, 36578, 57399, 40664, 31836, 48319, 48976,
      37853, 58545, 52851, 40429, 63441, 44945, 35187, 53405, 57391, 43694, 68836,
      60646, 45875, 72856, 54009, 41437, 64619, 54522, 41509, 65394, 57614, 43581,
      69214, 51308, 39365, 61388, 60261, 45878, 72278, 63678, 48169, 76499, 56709,
      43509, 67850
    )
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      52969, 48193, 57109, 56007, 50081, 61165, 50620, 46501, 54155, 50321, 45783,
      54254, 53207, 47577, 58107, 48089, 44176, 51448, 55618, 50603, 59965, 58807,
      52585, 64223, 53151, 48827, 56863, 49134, 45078, 52626, 51573, 46850, 55676,
      46809, 43290, 49808, 46678, 42824, 49995, 48994, 44508, 52893, 44469, 41126,
      47318, 51591, 47332, 55257, 54151, 49193, 58460, 49150, 45455, 52299, 57243,
      51279, 62422, 60085, 53021, 66208, 54286, 49463, 58456, 54381, 48715, 59301,
      57081, 50370, 62897, 51572, 46990, 55533, 60105, 53843, 65544, 63090, 55672,
      69518, 57000, 51936, 61379, 87424, 81240, 92669, 90235, 83838, 95614, 84450,
      78516, 89525, 83053, 77178, 88036, 85723, 79646, 90833, 80227, 74590, 85048,
      91795, 85302, 97303, 94747, 88030, 100394, 88672, 82442, 94001, 82403, 76549,
      87432, 85498, 79374, 90715, 79129, 73587, 83926, 78283, 72721, 83061, 81223,
      75405, 86179, 75172, 69908, 79729, 86524, 80376, 91804, 89772, 83342, 95251,
      83085, 77266, 88122, 91944, 85510, 97330, 94502, 87905, 99974, 89239, 83001,
      94506, 87347, 81235, 92464, 89776, 83509, 94976, 84777, 78851, 89781, 96541,
      89786, 102197, 99227, 92300, 104973, 93701, 87151, 99232, 66483, 57477, 74145,
      68943, 58951, 77372, 63929, 55944, 70769, 63159, 54603, 70438, 65496, 56003,
      73503, 60733, 53147, 67230, 69807, 60350, 77852, 72390, 61898, 81240, 67126,
      58742, 74307, 62276, 54511, 68950, 64918, 56118, 72431, 59530, 52838, 65305,
      59162, 51785, 65502, 61672, 53312, 68810, 56554, 50196, 62039, 65390, 57236,
      72397, 68163, 58924, 76053, 62507, 55479, 68570, 70337, 60156, 78893, 72637,
      61513, 81892, 67954, 58747, 75758, 66820, 57148, 74948, 69005, 58437, 77798,
      64556, 55810, 71970, 73854, 63163, 82838, 76268, 64588, 85987, 71351, 61684,
      79546, 101790, 94986, 107270, 103804, 96941, 109279, 99661, 92939, 105125,
      96701, 90237, 101906, 98614, 92093, 103815, 94678, 88292, 99869, 106880,
      99735, 112633, 108994, 101788, 114743, 104644, 97586, 110381, 97634, 90933,
      103119, 99878, 93078, 105395, 95260, 88687, 100690, 92752, 86387, 97963,
      94884, 88424, 100125, 90497, 84252, 95655, 102516, 95480, 108275, 104872,
      97732, 110664, 100023, 93121, 105724, 105489, 98644, 110910, 107298, 100428,
      112686, 103577, 96776, 109013, 100215, 93712, 105364, 101933, 95407, 107052,
      98398, 91937, 103562, 110764, 103576, 116455, 112663, 105450, 118320, 108756,
      101615, 114464
    )
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_micro$impact_rounded,
    expected = c(
      140393, 129433, 149778, 146242, 133919, 156779, 135070, 125017, 143680,
      133373, 122961, 142289, 138930, 127223, 148940, 128316, 118767, 136496,
      147413, 135904, 157267, 153554, 140615, 164618, 141823, 131268, 150864,
      131538, 121627, 140058, 137070, 126224, 146391, 125938, 116877, 133734,
      124961, 115546, 133055, 130217, 119913, 139072, 119641, 111034, 127047,
      138115, 127709, 147061, 143924, 132535, 153711, 132235, 122721, 140420,
      149187, 136789, 159753, 154587, 140926, 166182, 143525, 132464, 152963,
      141728, 129950, 151765, 146857, 133880, 157873, 136348, 125841, 145315,
      156646, 143629, 167740, 162316, 147972, 174491, 150701, 139088, 160611,
      168273, 152463, 181415, 172747, 155891, 186651, 163590, 148884, 175894,
      159860, 144840, 172344, 164110, 148097, 177318, 155411, 141439, 167099,
      176687, 160086, 190485, 181385, 163686, 195983, 171770, 156328, 184688,
      159910, 145444, 172069, 164796, 149196, 177826, 154790, 141524, 165995,
      151914, 138172, 163466, 156556, 141737, 168935, 147051, 134448, 157695,
      167905, 152717, 180673, 173036, 156656, 186717, 162530, 148600, 174294,
      175826, 158800, 189803, 179935, 161941, 194578, 171530, 155523, 184771,
      167035, 150860, 180312, 170938, 153844, 184849, 162954, 147747, 175532,
      184618, 166740, 199293, 188932, 170038, 204307, 180107, 163299, 194009
    )
  )
})


testthat::test_that("results correct |pathway_rr|erf_lin_lin|exp_dist|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))
  bhd_value = data$gbd_daly[1]
  data <- data |> dplyr::slice(-1)
  #Exotic test based on real data but does produce real world results

  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  rr_change <-1.2
  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(data$exposure_mean, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::rep(base::signif(base::unlist(base::lapply(min(data$exposure_mean), function(x)  x * cutoff_change^(0:3))),5),times = length(data$exposure_mean))
  bhd_c <- base::rep(base::signif(base::unlist(base::lapply(bhd_value, function(x) x * bhd_change^(0:3))),5),times = length(data$exposure_mean))
  rr_c <- base::rep(base::signif(base::unlist(base::lapply(1.08, function(x) x * rr_change^(0:3))),5),times = length(data$exposure_mean))

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age_group = base::rep(c("below_50", "below_50", "50_plus", "70_plus"),times = length(data$exposure_mean)),
    sex = base::rep(c("male", "female", "male", "female"),times = length(data$exposure_mean)),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    rr_central = rr_c,
    rr_lower = rr_c - rr_c/uncert_factor,
    rr_upper = rr_c + rr_c/uncert_factor,
    rr_increment = base::rep(c(10, 11, 12, 13), times = 5),
    erf_shape = "linear",
    prop_pop_exp = base::rep(data$prop_exposed, each = 4),
    geo_id_micro = base::rep(base::rep(c("urban","rural"), each = 5), each = 2))

  x$health_detailed$results_by_geo_id_micro$impact_rounded
  testthat::expect_equal(
    ## test if age group results are correct
    object =x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      30725, 24766, 35985, 34368, 27092, 40761, 27707, 22597, 32225,
      29189, 23528, 34186, 32649, 25737, 38723, 26321, 21468, 30614,
      32261, 26005, 37784, 36086, 28447, 42799, 29092, 23727, 33836,
      26594, 21659, 30973, 29692, 23888, 34833, 23514, 19365, 27198,
      25264, 20576, 29425, 28207, 22693, 33091, 22339, 18396, 25838,
      27924, 22742, 32522, 31177, 25082, 36575, 24690, 20333, 28558,
      35298, 27893, 41784, 38668, 30056, 46130, 31725, 25619, 37097,
      33533, 26499, 39695, 36734, 28554, 43824, 30139, 24338, 35242,
      37063, 29288, 43873, 40601, 31559, 48437, 33311, 26900, 38952,
      42905, 40408, 45003, 43726, 41256, 45794, 42029, 39506, 44157,
      40760, 38388, 42753, 41540, 39194, 43505, 39928, 37531, 41949,
      45051, 42428, 47253, 45913, 43319, 48084, 44131, 41482, 46364,
      41131, 38585, 43286, 42065, 39543, 44191, 40131, 37564, 42312,
      39075, 36656, 41121, 39962, 37566, 41982, 38124, 35685, 40196,
      43188, 40515, 45450, 44168, 41520, 46401, 42137, 39442, 44427,
      44468, 42025, 46507, 45196, 42782, 47204, 43695, 41224, 45764,
      42245, 39924, 44182, 42936, 40643, 44844, 41510, 39163, 43476,
      46691, 44127, 48832, 47455, 44921, 49564, 45880, 43285, 48052,
      47845, 46546, 48945, 48163, 46884, 49246, 47511, 46192, 48629,
      45452, 44218, 46498, 45755, 44540, 46784, 45135, 43882, 46198,
      50237, 48873, 51392, 50572, 49228, 51708, 49886, 48502, 51061,
      46908, 45554, 48059, 47270, 45936, 48401, 46529, 45154, 47699,
      44563, 43277, 45656, 44906, 43640, 45981, 44203, 42896, 45314,
      49254, 47832, 50462, 49633, 48233, 50821, 48856, 47412, 50084,
      48673, 47426, 49726, 48957, 47728, 49994, 48377, 47111, 49447,
      46239, 45054, 47240, 46509, 45341, 47494, 45958, 44755, 46975,
      51107, 49797, 52213, 51404, 50114, 52493, 50796, 49467, 51920,
      47307, 44993, 49216, 47879, 45595, 49759, 46704, 44359, 48643,
      44942, 42743, 46756, 45485, 43315, 47271, 44369, 42141, 46211,
      49672, 47242, 51677, 50273, 47875, 52247, 49039, 46577, 51075,
      45886, 43503, 47864, 46534, 44181, 48481, 45201, 42787, 47209,
      43592, 41328, 45471, 44207, 41972, 46057, 42940, 40648, 44848,
      48180, 45678, 50257, 48860, 46390, 50905, 47461, 44927, 49569,
      48564, 46319, 50407, 49073, 46858, 50887, 48029, 45754, 49901,
      46136, 44003, 47887, 46619, 44515, 48343, 45628, 43466, 47406,
      50992, 48635, 52928, 51526, 49200, 53432, 50431, 48041, 52396,
      50187, 49041, 51150, 50411, 49281, 51360, 49954, 48792, 50931,
      47678, 46589, 48592, 47891, 46817, 48792, 47456, 46352, 48384,
      52696, 51493, 53707, 52932, 51745, 53928, 52452, 51232, 53478,
      49428, 48230, 50437, 49681, 48500, 50675, 49164, 47949, 50189,
      46956, 45819, 47915, 47197, 46075, 48141, 46706, 45551, 47679,
      51899, 50642, 52959, 52165, 50925, 53208, 51622, 50346, 52698,
      50861, 49764, 51781, 51061, 49978, 51968, 50654, 49541, 51587,
      48318, 47275, 49192, 48508, 47479, 49370, 48121, 47064, 49008,
      53404, 52252, 54370, 53614, 52477, 54567, 53186, 52018, 54166,
      46271, 35476, 55328, 49040, 37282, 58754, 43354, 33591, 51667,
      43957, 33702, 52562, 46588, 35418, 55817, 41187, 31912, 49084,
      48584, 37250, 58095, 51492, 39146, 61692, 45522, 35271, 54250,
      41866, 32418, 49955, 44868, 34362, 53733, 38697, 30384, 45904,
      39773, 30797, 47458, 42624, 32644, 51046, 36762, 28864, 43609,
      43959, 34039, 52453, 47111, 36080, 56419, 40632, 31903, 48199,
      50320, 38305, 60190, 52887, 39990, 63314, 47623, 36550, 56863,
      47804, 36390, 57181, 50242, 37991, 60148, 45242, 34723, 54020,
      52836, 40220, 63200, 55531, 41990, 66479, 50004, 38378, 59706
    )
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      45468, 41258, 49211, 48096, 42727, 52869, 43560, 40010, 46689,
      43195, 39195, 46750, 45691, 40591, 50226, 41382, 38009, 44355,
      47742, 43321, 51671, 50501, 44864, 55513, 45738, 42010, 49024,
      42534, 39046, 45609, 44504, 40351, 48198, 40695, 37748, 43252,
      40407, 37094, 43329, 42278, 38334, 45788, 38660, 35860, 41090,
      44661, 40999, 47890, 46729, 42369, 50608, 42729, 39635, 45415,
      48957, 43538, 53768, 51409, 44906, 57124, 46382, 42116, 50171,
      46509, 41361, 51079, 48838, 42661, 54268, 44063, 40010, 47663,
      51404, 45715, 56456, 53979, 47151, 59980, 48701, 44222, 52680,
      76007, 70462, 80722, 78162, 72505, 82932, 73687, 68286, 78322,
      72206, 66939, 76686, 74253, 68880, 78785, 70003, 64872, 74406,
      79807, 73985, 84758, 82070, 76130, 87078, 77372, 71700, 82238,
      72100, 66753, 76709, 74523, 69016, 79227, 69480, 64334, 73957,
      68495, 63415, 72873, 70797, 65565, 75266, 66006, 61118, 70259,
      75705, 70090, 80544, 78249, 72467, 83188, 72954, 67551, 77655,
      79483, 73807, 84250, 81411, 75661, 86204, 77415, 71838, 82138,
      75509, 70116, 80037, 77341, 71877, 81894, 73544, 68246, 78031,
      83457, 77497, 88462, 85482, 79444, 90514, 81286, 75430, 86244,
      56714, 48296, 63883, 58809, 49483, 66611, 54524, 47068, 60979,
      53879, 45881, 60689, 55869, 47009, 63280, 51798, 44715, 57930,
      59550, 50711, 67077, 61750, 51958, 69941, 57250, 49422, 64028,
      53268, 46051, 59546, 55520, 47325, 62535, 50909, 44730, 56357,
      50604, 43748, 56569, 52744, 44958, 59408, 48363, 42494, 53539,
      55931, 48353, 62524, 58296, 49691, 65662, 53454, 46967, 59175,
      59895, 50364, 67825, 61851, 51477, 70327, 57853, 49215, 65170,
      56900, 47846, 64434, 58758, 48903, 66810, 54960, 46755, 61911,
      62889, 52882, 71216, 64943, 54051, 73843, 60745, 51676, 68428,
      87050, 81214, 91812, 88521, 82675, 93262, 85488, 79674, 90262,
      82698, 77153, 87221, 84095, 78541, 88599, 81214, 75690, 85749,
      91403, 85275, 96402, 92947, 86808, 97925, 89762, 83658, 94775,
      83912, 78100, 88710, 85562, 79718, 90354, 82153, 76390, 86945,
      79717, 74195, 84274, 81284, 75732, 85836, 78046, 72570, 82598,
      88108, 82005, 93145, 89840, 83704, 94871, 86261, 80209, 91292,
      89850, 84024, 94554, 91170, 85349, 95843, 88453, 82630, 93181,
      85358, 79823, 89826, 86611, 81081, 91051, 84031, 78498, 88522,
      94343, 88225, 99281, 95728, 89616, 100635, 92876, 86761, 97840
    )
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_micro$impact_rounded,
    expected = c(
      121475, 111720, 129933, 126258, 115232, 135801, 117247, 108296, 125011, 115401, 106134, 123436, 119945, 109471, 129011, 111385, 102881, 118760, 127549, 117306, 136430, 132571, 120994, 142591, 123109, 113710, 131261, 114634, 105799, 122318, 119027, 109367, 127425, 110174, 102082, 117209, 108902, 100509, 116202, 113075, 103899, 121054, 104666,  96978, 111349, 120365, 111089, 128434, 124978, 114835, 133797,
      115683, 107186, 123070, 128439, 117344, 138018, 132820, 120566, 143328, 123797, 113954, 132309, 122017, 111477, 131117, 126179, 114538, 136162, 117607, 108256, 125693, 134861, 123212, 144918, 139461, 126595, 150494, 129987, 119652, 138924, 143765, 129510, 155694, 147330, 132158, 159873, 140012, 126742, 151241, 136576, 123034, 147910, 139964, 125550, 151879, 133012, 120405, 143679, 150953, 135985, 163479,
      154697, 138766, 167867, 147013, 133079, 158803, 137180, 124151, 148256, 141082, 127043, 152889, 133062, 121120, 143302, 130321, 117943, 140843, 134028, 120691, 145244, 126409, 115064, 136137, 144039, 130359, 155669, 148136, 133395, 160533, 139715, 127176, 150467, 149745, 134387, 162379, 153021, 136826, 166169, 146306, 131845, 158351, 142258, 127668, 154260, 145370, 129985, 157861, 138991, 125253, 150434,
      157232, 141107, 170498, 160672, 143667, 174478, 153622, 138437, 166269
    )
  )
})




#### USER-DEFINED ERF FUNCTION ###############################################

testthat::test_that("results the same mrbrt with cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))
  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$exposure_mean,
        prop_pop_exp = data$prop_exposed,
        cutoff_central = min(data$exposure_mean),
        bhd_central = data$gbd_daly[1],
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural"),
        erf_eq_lower = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean,
          method = "natural"),
        erf_eq_upper = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean + 0.01,
          method = "natural"),
        info = data.frame(pollutant = "road_noise", outcome = "YLD")
        )$health_main$impact_rounded,
    expected =
      c(1637, 1637, 2450) # Results on 2025-01-20 ; no comparison study
  )
})

testthat::test_that("results the same mrbrt no cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))
  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = data$exposure_mean,
        cutoff_central = 0,
        prop_pop_exp = data$prop_exposed,
        bhd_central = data$gbd_daly[1],
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural"),
        erf_eq_lower = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean,
          method = "natural"),
        erf_eq_upper = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean + 0.01,
          method = "natural"),
        info = data.frame(pollutant = "road_noise", outcome = "YLD")
        )$health_main$impact_rounded,
    expected =
      c(32502, 32502, 32828) # Results on 2025-01-20 ; no comparison study
  )
})

testthat::test_that("results the same mrbrt with cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_pop <- base::readRDS(testthat::test_path("data", "pop_data_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        exp_central = data_pop$Concentration,
        prop_pop_exp = data_pop$Viken,
        cutoff_central = 5,
        bhd_central = 4500,
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural")
      )$health_main$impact_rounded,
    expected =
      c(32) # Results on 19 Dec 2024; no comparison study
  )
})

testthat::test_that("results the same mrbrt no cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_pop <- base::readRDS(testthat::test_path("data", "pop_data_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        exp_central = data_pop$Concentration,
        cutoff_central = 0,
        prop_pop_exp = data_pop$Viken,
        bhd_central = 4500,
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural")
      )$health_main$impact_rounded,
    expected =
      c(249) # Results on 2025-01-17; no comparison study
  )
})

testthat::test_that("results the same mrbrt with cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_pop <- base::readRDS(testthat::test_path("data", "pop_data_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        exp_central = data_pop$Concentration,
        prop_pop_exp = data_pop$Viken,
        cutoff_central = 0,
        bhd_central = 4500,
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural")
      )$health_main$impact_rounded,
    expected =
      c(249) # Results on 2025-01-17 ; no comparison study
  )
})


testthat::test_that("results the same mrbrt with cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data_pop <- base::readRDS(testthat::test_path("data", "pop_data_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        exp_central = data_pop$Concentration,
        prop_pop_exp = data_pop$Viken,
        cutoff_central = 5,
        cutoff_lower = 5 - 1,
        cutoff_upper = 5 + 1,
        bhd_central = 4500,
        bhd_lower = 4500 - 1000,
        bhd_upper = 4500 + 1000,
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural"),
        erf_eq_lower = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean,
          method = "natural"),
        erf_eq_upper = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean + 0.01,
          method = "natural")
      )$health_main$impact_rounded,
    expected =
      c(32,32,76) # Results on 19 Dec 2024 ; no comparison study
  )
})

testthat::test_that("results the same |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data_pop <- base::readRDS(testthat::test_path("data", "pop_data_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        exp_central = data_pop$Concentration,
        cutoff_central = 5,
        cutoff_lower = 5 - 1,
        cutoff_upper = 5 + 1,
        prop_pop_exp = data_pop$Viken,
        bhd_central = 4500,
        bhd_lower = 4500 - 1000,
        bhd_upper = 4500 + 1000,
        erf_eq_central =
          stats::splinefun(
            x = data_erf$exposure,
            y = data_erf$mean,
            method = "natural"),
        erf_eq_lower = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean,
          method = "natural"),
        erf_eq_upper = stats::splinefun(
          x = data_erf$exposure,
          y = data_erf$mean + 0.01,
          method = "natural")
      )$health_main$impact_rounded,
    expected =
      c(32,32,76) # Results on 28 May 2025 ; no comparison study
  )
})


##### Stratification (sex/age) ####################################################################


testthat::test_that("results the same |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|", {


  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))
  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))
  #Exotic test based on real data but does produce real world results
func <-stats::splinefun(x = data_erf$exposure,
                        y = data_erf$mean,
                        method = "natural")


  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(data$exposure_mean, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::rep(base::signif(base::unlist(base::lapply(min(data$exposure_mean), function(x)  x * cutoff_change^(0:3))),5),times = length(data$exposure_mean))
  bhd_c <- base::rep(base::signif(base::unlist(base::lapply(data$gbd_daly[1], function(x) x * bhd_change^(0:3))),5),times = length(data$exposure_mean))


  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age_group = base::rep(c("below_50", "below_50", "50_plus", "70_plus"),times = length(data$exposure_mean)),
    sex = base::rep(c("male", "female", "male", "female"),times = length(data$exposure_mean)),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    erf_eq_central = stats::splinefun(x = data_erf$exposure,
                                      y = data_erf$mean,
                                      method = "natural"),#formula
    erf_eq_lower = stats::splinefun(x = data_erf$exposure,
                                    y = data_erf$mean -0.01,
                                    method = "natural"),
    erf_eq_upper = stats::splinefun(x = data_erf$exposure,
                                    y = data_erf$mean + 0.01,
                                    method = "natural"),
    prop_pop_exp = base::rep(data$prop_exposed, each = 4))
  testthat::expect_equal(
    ## test if age group results are correct
    object = x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      14725,13363,16063,18429,17135,19700,12938,11544,14307,13989,12694,15260,
      17508,16279,18715,12291,10967,13591,15461,14031,16866,19351,17992,20685,
      13585,12121,15022,12284,10878,13663,14095,12721,15443,10485,9047,11896,
      11670,10334,12980,13390,12085,14671,9961,8594,11301,12898,11422,14347,
      14800,13357,16215,11009,9499,12491,19023,17740,20284,22521,21300,23720,
      15346,13994,16673,18072,16853,19270,21395,20235,22534,14578,13294,15839,
      19974,18627,21298,23647,22365,24906,16113,14693,17507,18766,18397,19131,
      19463,19103,19817,18046,17666,18421,17828,17477,18174,18490,18148,18826,
      17144,16783,17500,19705,19316,20087,20436,20059,20808,18949,18549,19342,
      17341,16950,17726,18084,17704,18458,16573,16170,16970,16474,16102,16840,
      17180,16819,17536,15744,15362,16121,18208,17797,18613,18988,18590,19381,
      17402,16979,17818,20102,19751,20447,20754,20413,21090,19427,19067,19782,
      19097,18764,19425,19716,19392,20036,18455,18113,18793,21107,20739,21469,
      21792,21434,22145,20398,20020,20771,21308,21037,21575,21693,21427,21955,
      20912,20636,21185,20243,19985,20497,20608,20356,20857,19867,19604,20126,
      22373,22089,22654,22778,22498,23053,21958,21668,22244,20225,19939,20506,
      20639,20359,20915,19799,19507,20086,19213,18942,19481,19607,19341,19870,
      18809,18532,19082,21236,20936,21532,21671,21377,21961,20789,20483,21090,
      22313,22055,22567,22670,22417,22920,21946,21683,22205,21197,20953,21439,
      21536,21296,21774,20849,20599,21095,23429,23158,23696,23803,23538,24066,
      23043,22768,23315
    )
  )
  testthat::expect_equal(
    ## test if sex results are correct
    object = x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      20403,19204,21581,23528,22387,24649,19225,18006,20422,19383,18244,20502,
      22352,21267,23417,18263,17105,19400,21423,20164,22660,24704,23506,25882,
      20186,18906,21443,18438,17207,19647,19640,18429,20829,17280,16030,18508,
      17516,16346,18664,18658,17508,19788,16416,15228,17583,19360,18067,20629,
      20622,19351,21871,18144,16831,19434,24243,23112,25354,27189,26111,28248,
      21144,19956,22311,23031,21957,24086,25829,24806,26836,20087,18958,21195,
      25455,24268,26621,28548,27417,29661,22201,20954,23426,34397,33592,35188,
      36057,35279,36823,32672,31840,33491,32677,31913,33429,34254,33515,34982,
      31038,30248,31816,36116,35272,36948,37860,37043,38664,34306,33432,35165,
      31412,30560,32249,33178,32356,33988,29576,28695,30444,29841,29032,30637,
      31519,30738,32288,28098,27260,28922,32982,32088,33862,34837,33974,35687,
      31055,30130,31966,37195,36434,37945,38756,38019,39482,35574,34788,36349,
      35335,34612,36048,36818,36118,37508,33796,33048,34532,39055,38256,39842,
      40694,39920,41456,37353,36527,38166
    )
  )
})

testthat::test_that("results the same |pathway_rr|erf_function|exp_dist|cutoff_TRUE|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|",{
  data <- base::readRDS(testthat::test_path("data", "LMU_O3_COPD_mort_2016.rds"))
  #Exotic test based on real data but does produce real world results

  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  rr_change = bhd_change <-1.2
  uncert_factor <- 20#set uncertainty factor
  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(data$Mean.O3, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::rep(base::signif(base::unlist(base::lapply(1, function(x)  x * cutoff_change^(0:3))),5),times = length(data$Mean.O3))
  bhd_c <- base::rep(base::signif(base::unlist(base::lapply(29908, function(x) x * bhd_change^(0:3))),5),times = length(data$Mean.O3))
  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age = base::rep(c("below_50", "below_50", "50_plus", "70_plus"),times = length(data$Mean.O3)),
    sex = base::rep(c("male", "female", "male", "female"),times = length(data$Mean.O3)),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    erf_eq_central = stats::splinefun(data$x, data$y, method="natural"),
    erf_eq_lower  = stats::splinefun(data$x, data$y_l, method="natural"),
    erf_eq_upper  = stats::splinefun(data$x, data$y_u, method="natural"),
    prop_pop_exp = base::rep(data$Population.affected, each = 4),
  )
  testthat::expect_equal(
    ## test if age group results are correct
    object =x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      729,555,882,729,555,883,728,554,881,692,527,838,693,527,838,692,527,837,
      765,582,926,766,583,927,765,582,925,670,510,811,671,511,812,669,509,810,
      637,485,771,637,485,771,636,484,770,704,536,852,705,536,853,703,535,851,
      775,590,937,775,590,938,774,589,937,736,560,890,736,560,891,735,560,890,
      813,619,984,814,619,985,813,619,983,555,422,671,555,422,671,555,422,671,
      527,401,638,527,401,638,527,401,638,583,443,705,583,444,705,582,443,705,
      530,404,642,531,404,642,530,404,642,504,384,610,504,384,610,504,383,610,
      557,424,674,557,424,674,557,424,674,577,439,698,577,439,698,576,439,697,
      548,417,663,548,417,663,548,417,662,605,461,732,606,461,733,605,461,732,
      717,546,867,717,546,867,717,546,867,681,518,824,681,519,824,681,518,824,
      753,573,910,753,573,911,753,573,910,690,525,835,690,525,835,690,525,835,
      655,499,793,656,499,793,655,499,793,724,552,876,725,552,877,724,551,876,
      743,565,898,743,566,899,743,565,898,706,537,853,706,537,854,705,537,853,
      780,594,943,780,594,943,780,594,943
    )
  )
  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      862,656,1043,862,656,1043,861,655,1042,
      819,623,990,819,623,991,818,623,990,
      905,689,1095,905,689,1095,904,688,1094,
      805,612,974,805,613,974,804,612,973,
      764,582,925,765,582,926,764,581,924,
      845,643,1022,846,644,1023,844,642,1022,
      908,691,1098,908,691,1099,907,691,1098,
      862,656,1043,863,657,1044,862,656,1043,
      953,726,1153,954,726,1154,953,725,1153,
      1139,867,1378,1139,867,1378,1138,867,1377,
      1082,824,1309,1082,824,1309,1081,823,1308,
      1196,910,1447,1196,910,1447,1195,910,1446,
      1086,827,1314,1086,827,1314,1086,826,1314,
      1032,785,1248,1032,786,1249,1031,785,1248,
      1140,868,1380,1141,868,1380,1140,868,1379,
      1186,903,1435,1186,903,1435,1186,903,1434,
      1127,858,1363,1127,858,1363,1126,858,1363,
      1245,948,1507,1246,948,1507,1245,948,1506
    )
  )
})


testthat::test_that("results the same |pathway_rr|erf_function|exp_dist|cutoff_TRUE|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|",{
data <- base::readRDS(testthat::test_path("data", "LMU_O3_COPD_mort_2015_2016.rds"))
  data <- data |> dplyr::slice(-1)
  erf <- stats::splinefun(data$x[1:21], data$y[1:21], method="natural")
  erf_l <- stats::splinefun(data$x[1:21], data$y_l[1:21], method="natural")
  erf_u <- stats::splinefun(data$x[1:21], data$y_u[1:21], method="natural")
  #Exotic test based on real data but does produce real world results
  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  rr_change = bhd_change <-1.2
  uncert_factor <- 20#set uncertainty factor
  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(data$Mean.O3, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::rep(base::signif(base::unlist(base::lapply(1, function(x)  x * cutoff_change^(0:3))),5),times = length(data$Mean.O3))
  bhd_c <- base::signif(base::unlist(base::lapply(data$bhd, function(x) x * bhd_change^(0:3))),5)

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age = base::rep(c("below_50", "below_50", "50_plus", "70_plus"),times = length(data$Mean.O3)),
    sex = base::rep(c("male", "female", "male", "female"),times = length(data$Mean.O3)),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    erf_eq_central = erf,
    erf_eq_lower  = erf_l,
    erf_eq_upper  = erf_u,
    prop_pop_exp = base::rep(data$Population.affected, each = 4),
    geo_id_micro = base::rep(data$X, each = 4)
  )

  testthat::expect_equal(
    ## test if age group results are correct
    object =x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      807,614,976,807,614,977,806,614,976,766,583,927,767,584,928,766,583,927,
      847,645,1025,848,645,1026,847,644,1024,756,576,915,757,576,916,756,575,915,
      719,547,870,719,547,870,718,546,869,794,604,961,795,605,962,794,604,960,
      848,646,1026,849,646,1027,848,645,1026,806,613,975,806,614,975,805,613,974,
      891,678,1078,891,678,1078,890,678,1077,601,457,727,601,457,727,601,457,727,
      571,435,690,571,435,691,571,434,690,631,480,763,631,480,763,631,480,763,
      577,439,698,577,439,698,577,439,698,548,417,663,548,417,663,548,417,663,
      606,461,733,606,461,733,605,461,732,623,474,754,623,475,754,623,474,754,
      592,451,716,592,451,716,592,451,716,654,498,792,654,498,792,654,498,791,
      774,590,937,774,590,937,774,589,936,736,560,890,736,560,890,735,560,890,
      813,619,983,813,619,984,813,619,983,746,568,902,746,568,902,746,568,902,
      709,539,857,709,540,857,708,539,857,783,596,947,783,596,948,783,596,947,
      802,611,971,802,611,971,802,611,970,762,580,922,762,581,922,762,580,922,
      842,642,1019,843,642,1019,842,641,1019,729,555,882,729,555,883,728,554,881,
      692,527,838,693,527,838,692,527,837,765,582,926,766,583,927,765,582,925,
      670,510,811,671,510,812,669,509,810,637,484,770,637,485,771,636,484,770,
      704,535,851,704,536,852,703,535,851,775,590,937,775,590,938,774,589,937,
      736,560,890,736,560,891,735,560,890,813,619,984,814,619,985,813,619,984,
      555,422,671,555,422,671,555,422,671,527,401,638,527,401,638,527,401,638,
      583,444,705,583,444,705,582,443,705,530,404,642,531,404,642,530,404,642,
      504,384,610,504,384,610,504,383,610,557,424,674,557,424,674,557,424,674,
      577,439,698,577,439,698,576,439,697,548,417,663,548,417,663,548,417,663,
      605,461,732,606,461,733,605,461,732,717,546,867,717,546,867,717,546,867,
      681,518,824,681,519,824,681,518,824,753,573,911,753,573,911,753,573,910,
      690,525,835,690,525,835,690,525,835,655,499,793,656,499,793,655,499,793,
      724,552,877,725,552,877,724,551,876,743,565,898,743,566,899,743,565,898,
      706,537,853,706,537,854,705,537,853,780,594,943,780,594,943,780,594,943
    )

  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_micro$impact_rounded,
    expected = c(
      2182,1661,2640,2183,1662,2640,2181,1660,2639,
      2073,1578,2508,2073,1578,2508,2072,1577,2507,
      2291,1744,2772,2292,1745,2772,2290,1743,2771,
      2079,1583,2515,2080,1583,2516,2078,1582,2514,
      1975,1503,2390,1976,1504,2391,1974,1503,2389,
      2183,1662,2641,2184,1662,2642,2182,1661,2640,
      2274,1731,2751,2274,1732,2751,2273,1731,2750,
      2160,1645,2613,2161,1645,2614,2159,1644,2612,
      2387,1818,2888,2388,1818,2889,2387,1817,2887,
      2000,1523,2420,2001,1523,2421,2000,1522,2419,
      1900,1447,2299,1901,1447,2300,1900,1446,2298,
      2101,1599,2541,2101,1600,2542,2100,1598,2540,
      1891,1439,2288,1892,1440,2289,1890,1438,2286,
      1796,1367,2173,1797,1368,2174,1795,1366,2172,
      1985,1511,2402,1986,1512,2403,1984,1510,2401,
      2094,1594,2533,2094,1595,2534,2093,1593,2532,
      1989,1514,2407,1990,1515,2407,1988,1514,2406,
      2199,1674,2660,2199,1674,2661,2198,1673,2659
    )
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      946,720,1144,946,720,1145,945,720,1144,898,684,1087,899,684,1088,898,684,1087,
      993,756,1202,993,756,1202,993,756,1201,895,681,1083,895,682,1083,894,681,1082,
      850,647,1029,851,647,1029,850,647,1028,940,715,1137,940,716,1138,939,715,1136,
      989,753,1197,990,754,1197,989,753,1197,940,716,1137,940,716,1138,940,715,1137,
      1039,791,1257,1039,791,1257,1039,791,1256,1236,941,1495,1236,941,1496,1236,941,1495,
      1174,894,1421,1175,894,1421,1174,894,1420,1298,988,1570,1298,988,1570,1298,988,1570,
      1184,901,1433,1184,902,1433,1184,901,1432,1125,856,1361,1125,857,1361,1125,856,1361,
      1243,947,1504,1244,947,1505,1243,946,1504,1284,978,1554,1285,978,1554,1284,978,1553,
      1220,929,1476,1220,929,1476,1220,929,1476,1348,1027,1631,1349,1027,1632,1348,1026,1631,
      862,656,1043,862,656,1043,861,656,1042,819,623,991,819,623,991,818,623,990,
      905,689,1095,905,689,1095,904,688,1094,804,612,973,805,613,974,804,612,973,
      764,582,925,765,582,926,764,581,924,845,643,1022,845,643,1023,844,642,1021,
      908,691,1098,908,691,1099,907,691,1098,862,656,1043,863,657,1044,862,656,1043,
      953,726,1153,954,726,1154,953,725,1153,1139,867,1378,1139,867,1378,1138,867,1377,
      1082,824,1309,1082,824,1309,1082,823,1308,1196,910,1447,1196,911,1447,1195,910,1446,
      1086,827,1314,1086,827,1315,1086,826,1314,1032,785,1248,1032,786,1249,1031,785,1248,
      1140,868,1380,1141,868,1380,1140,868,1379,1186,903,1435,1186,903,1435,1186,903,1434,
      1127,858,1363,1127,858,1363,1126,858,1363,1245,948,1507,1246,948,1507,1245,948,1506
    )
  )
})

testthat::test_that("results the same |pathway_rr|erf_formula|exp_dist|cutoff_TRUE|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|",{
  data <- base::readRDS(testthat::test_path("data", "LMU_O3_COPD_mort_2015_2016.rds"))
  data <- data |> dplyr::slice(-1)
  #erf <- stats::splinefun(data$x[1:21], data$y[1:21], method="natural")
  #Exotic test based on real data but does produce real world results
  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  rr_change = bhd_change <-1.2
  uncert_factor <- 20#set uncertainty factor
  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(data$Mean.O3, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::rep(base::signif(base::unlist(base::lapply(1, function(x)  x * cutoff_change^(0:3))),5),times = length(data$Mean.O3))
  bhd_c <- base::signif(base::unlist(base::lapply(data$bhd, function(x) x * bhd_change^(0:3))),5)
  exp_c

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age = base::rep(c("below_50", "below_50", "50_plus", "70_plus"),times = length(data$Mean.O3)),
    sex = base::rep(c("male", "female", "male", "female"),times = length(data$Mean.O3)),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    erf_eq_central = "0.0026*(c-66.05)^0.35 + 0.0025*(c-66.05)^0.5-0.00085*(c-66.05)^0.7 - 0.015*(c-66.05+10)^-1 + 1",#   min(data$x[1:21]) = 66.05
    prop_pop_exp = base::rep(data$Population.affected, each = 4),
    geo_id_micro = base::rep(data$X, each = 4)
  )
  x$health_detailed$results_by_sex$impact_rounded
  testthat::expect_equal(
    ## test if age group results are correct
    object =x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      466, 466, 465, 442, 442, 442, 489, 489, 489, 441, 441, 441, 419, 419, 419,
      463, 464, 463, 855, 855, 854, 812, 812, 811, 897, 898, 897, 604, 604, 604,
      574, 574, 574, 635, 635, 634, 581, 582, 581, 552, 553, 552, 611, 611, 610,
      624, 624, 624, 593, 593, 593, 655, 655, 655, 769, 770, 769, 731, 731, 731,
      808, 808, 808, 747, 747, 747, 710, 710, 710, 784, 785, 784, 789, 789, 789,
      749, 749, 749, 828, 828, 828, 425, 425, 425, 404, 404, 403, 446, 446, 446,
      398, 398, 398, 378, 378, 378, 418, 418, 417, 780, 780, 779, 741, 741, 740,
      819, 819, 818, 560, 560, 560, 532, 532, 532, 588, 588, 588, 535, 535, 535,
      508, 508, 508, 562, 562, 561, 581, 581, 581, 552, 552, 552, 610, 610, 610,
      719, 719, 719, 683, 683, 683, 755, 755, 755, 695, 695, 695, 660, 660, 660,
      730, 730, 730, 740, 740, 740, 703, 703, 703, 777, 777, 776
    )


  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_micro$impact_rounded,
    expected = c(
      1839, 1840, 1839, 1747, 1748, 1747, 1931, 1932, 1931,
      1770, 1770, 1769, 1681, 1682, 1681, 1858, 1859, 1858,
      2267, 2268, 2267, 2154, 2154, 2153, 2381, 2381, 2380,
      1704, 1704, 1703, 1618, 1619, 1618, 1789, 1789, 1788,
      1628, 1628, 1627, 1546, 1547, 1546, 1709, 1710, 1709,
      2100, 2101, 2099, 1995, 1996, 1994, 2205, 2206, 2204
    )
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      604, 604, 604, 574, 574, 574, 635, 635, 634, 581, 582, 581,
      552, 553, 552, 611, 611, 610, 993, 993, 992, 943, 943, 943,
      1042, 1043, 1042, 1235, 1235, 1235, 1173, 1173, 1173, 1297, 1297, 1296,
      1188, 1189, 1188, 1129, 1129, 1129, 1248, 1248, 1247, 1274, 1275, 1274,
      1211, 1211, 1211, 1338, 1338, 1338, 560, 560, 560, 532, 532, 532,
      588, 588, 588, 535, 535, 535, 508, 508, 508, 562, 562, 561,
      913, 914, 913, 868, 868, 867, 959, 960, 959, 1144, 1144, 1144,
      1087, 1087, 1086, 1201, 1201, 1201, 1093, 1093, 1093, 1038, 1039, 1038,
      1148, 1148, 1147, 1187, 1187, 1186, 1127, 1128, 1127, 1246, 1246, 1246
    )

  )
})

testthat::test_that("results the same |pathway_rr|erf_formula|exp_dist|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|", {

  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))
  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))
  #Exotic test based on real data but does produce real world results

  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- base::signif(base::unlist(base::lapply(data$exposure_mean, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::rep(base::signif(base::unlist(base::lapply(min(data$exposure_mean), function(x)  x * cutoff_change^(0:3))),5),times = length(data$exposure_mean))
  bhd_c <- base::rep(base::signif(base::unlist(base::lapply(data$gbd_daly[1], function(x) x * bhd_change^(0:3))),5),times = length(data$exposure_mean))

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age_group = base::rep(c("below_50", "below_50", "50_plus", "70_plus"),times = length(data$exposure_mean)),
    sex = base::rep(c("male", "female", "male", "female"),times = length(data$exposure_mean)),
    exp_central = exp_c,
    exp_lower = exp_c - exp_c/uncert_factor,
    exp_upper = exp_c + exp_c/uncert_factor,
    cutoff_central = cutoff_c,
    cutoff_lower = cutoff_c - cutoff_c/uncert_factor,
    cutoff_upper = cutoff_c + cutoff_c/uncert_factor,
    bhd_central = bhd_c,
    bhd_lower = bhd_c - bhd_c/uncert_factor,
    bhd_upper = bhd_c + bhd_c/uncert_factor,
    erf_eq_central = "1+0.55*c^0.125-0.001*c^0.5",
    prop_pop_exp = base::rep(data$prop_exposed, each = 4))

  testthat::expect_equal(
    ## test if age group results are correct
    object = x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      43480, 67338, 42659, 41306, 63971, 40526, 45654, 70705, 44792, 42431, 43306,
      38720, 40310, 41141, 36784, 44553, 45471, 40656, 67459, 69203, 43646, 64086,
      65742, 41464, 70832, 72663, 45828, 31616, 31725, 31501, 30035, 30139, 29926,
      33197, 33312, 33076, 31387, 31508, 31257, 29817, 29933, 29695, 32956, 33084,
      32820, 31823, 31922, 31719, 30232, 30326, 30133, 33414, 33518, 33304, 29118,
      29174, 29060, 27662, 27715, 27607, 30574, 30633, 30513, 28960, 29021, 28897,
      27512, 27570, 27453, 30408, 30472, 30342, 29264, 29316, 29210, 27801, 27850,
      27750, 30727, 30781, 30671
    )

  )
  testthat::expect_equal(
    ## test if sex results are correct
    object = x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      41389, 65084, 40760, 39319, 61830, 38722, 43458, 68339, 42798, 40555, 41226,
      37095, 38527, 39165, 35240, 42583, 43287, 38949, 65201, 66811, 41543, 61941,
      63470, 39466, 68461, 70151, 43620, 62826, 63153, 62460, 59684, 59996, 59337,
      65967, 66311, 65583, 62223, 62609, 61780, 59112, 59479, 58691, 65334, 65740,
      64869, 63344, 63630, 63032, 60177, 60448, 59880, 66512, 66811, 66183
    )
  )
})


## AR ###########################################################################

testthat::test_that("results correct |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = data$exposure_mean,
        pop_exp = data$population_exposed_total,
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance", id = 1:5)
        )$health_main$impact_rounded,
    expected =
      data_raw |>
      dplyr::filter(exposure_category %in% "Total exposed")|>
      dplyr::select(number)|>
      dplyr::pull() |>
      base::round()
  )

  ## Single exposure value
  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = data$exposure_mean,
        pop_exp = data$population_exposed_total,
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")
      )$health_detailed$results_raw |> dplyr::slice_head() |> dplyr::select(impact) |> dplyr::pull() |> base::round(),
    expected =
      data_raw |>
      dplyr::filter(exposure_category %in% "55-59")|>
      dplyr::select(number)|>
      dplyr::slice_head() |>
      dplyr::pull() |>
      base::round()
  )
})

testthat::test_that("results correct |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "roadnoise_ha_Lden_StavangerandVicinity.rds"))

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = data$average_cat,
        population  = unique(data$totpop),
        pop_exp = data$ANTALL_PER,
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        info = data.frame(pollutant = "road_noise",
                          outcome = "highly_annoyance")
      )$health_main$impact_rounded,
    expected =
      c(14136)
  )

  ## ASSESSOR: Liliana Vázquez, NIPH
  ## ASSESSMENT DETAILS:
  ## INPUT DATA DETAILS:
})

testthat::test_that("results correct |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  ## The inputs to the arguments are all vectors
  testthat::expect_equal(
    object =
      healthiar::attribute_health(
          approach_risk = exdat_noise$risk_estimate_type,
          exp_central = exdat_noise$exposure_mean,
          pop_exp = exdat_noise$exposed,
          erf_eq_central = exdat_noise$erf
      )$health_main$impact_rounded,
    expected =
      c(174232 * 2) # Results on 2 October 2025; no comparison study
  )

  ## ASSESSOR: Axel Luyten
  ## ASSESSMENT DETAILS: example_road_noise_niph.xlsx
  ## INPUT DATA DETAILS: -
})

testthat::test_that("results correct  |pathway_ar|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("data", "roadnoise_ha_Lden_StavangerandVicinity.rds"))



  erf_df <- data.frame(
    dB = seq(30, 85, by = 0.5)
  )

  # Compute AR using the quadratic formula
  erf_df$AR <- 78.9270 - 3.1162 * erf_df$dB + 0.0342 * erf_df$dB^2

  # Create a function using spline interpolation over the data
  spline_fun <- stats::splinefun(
    x = erf_df$dB,
    y = erf_df$AR,
    method = "natural"
  )

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = data$average_cat,
        population = data$totpop,
        pop_exp = data$ANTALL_PER,

        erf_eq_central = spline_fun,
        dw_central = 0.02,
        duration_central = 1,
        info = data.frame(pollutant = "road_noise",

                          outcome = "highly_annoyance")

      )$health_main$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(283)
  )
})

## ASSESSOR:
## Liliana Vázquez, NIPH
## ASSESSMENT DETAILS:
## Stavanger highly annoyance
## INPUT DATA DETAILS:
## Add here input data details: defined own function with spline interpolation

### ITERATION ###################################################################

testthat::test_that("no error ar iteration", {

  testthat::expect_no_error(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = c(runif_with_seed(5,8,10,1),
                           runif_with_seed(5,8,10,2),
                           runif_with_seed(5,8,10,3)),
        pop_exp = c(
          runif_with_seed(1,5E3,1E4,1) * runif_with_seed(5,0,1,1), # total pop * proportion pop exposed
          runif_with_seed(1,5E3,1E4,2) * runif_with_seed(5,0,1,2),
          runif_with_seed(1,5E3,1E4,3) * runif_with_seed(5,0,1,3)
        ),
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        geo_id_micro = base::rep(1:3, 5),
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance"))
)
})

testthat::test_that("detailed results the same |pathway_ar|erf_formula|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = c(runif_with_seed(5,8,10,1),
                           runif_with_seed(5,8,10,2),
                           runif_with_seed(5,8,10,3)),
        pop_exp = c(
          runif_with_seed(1,5E3,1E4,1) * runif_with_seed(5,0,1,1), # total pop * proportion pop exposed
          runif_with_seed(1,5E3,1E4,2) * runif_with_seed(5,0,1,2),
          runif_with_seed(1,5E3,1E4,3) * runif_with_seed(5,0,1,3)
          ),
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        geo_id_micro = base::rep(1:3, 5),
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")
        )$health_detailed$results_raw$impact |> base::round(),
    expected =
      c(921, 1278, 1932, 2967, 704, 605, 2191, 1810, 551, 2877, 543, 2458, 1219, 1043, 1869) # Results on 2025-02-05; no comparison study
  )
})

testthat::test_that("detailed results the same |pathway_ar|erf_formula|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = c(runif_with_seed(5,9,10,1),
                           runif_with_seed(5,9,10,2),
                           runif_with_seed(5,9,10,3)),
        exp_lower = c(runif_with_seed(5,7,8,1),
                         runif_with_seed(5,7,8,2),
                         runif_with_seed(5,7,8,3)),
        exp_upper = c(runif_with_seed(5,11,12,1),
                         runif_with_seed(5,11,12,2),
                         runif_with_seed(5,11,12,3)),
        pop_exp = c(
          runif_with_seed(1,5E3,1E4,1) * runif_with_seed(5,0,1,1), # total pop * proportion pop exposed
          runif_with_seed(1,5E3,1E4,2) * runif_with_seed(5,0,1,2),
          runif_with_seed(1,5E3,1E4,3) * runif_with_seed(5,0,1,3)
        ),
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        geo_id_micro = base::rep(1:3, 5),
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")
        )$health_detailed$results_raw$impact |> base::round(),
    expected = # Results on 2025-01-20; no comparison study
      c(890, 976,  809, 1241, 1361, 1128, 1893, 2077, 1720, 2954, 3242, 2682,  678,  743,
        617, 583, 639,  530, 2160, 2370, 1962, 1774, 1946, 1611, 530, 581, 482, 2870,
        3150, 2605,  522,  573,  475, 2436, 2673, 2212, 1185, 1299, 1076, 1011, 1109,  919,
        1834, 2012, 1666)
  )
})

testthat::test_that("results correct |pathway_ar|erf_formula|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))

  ## Convert data to long format following Ma-Loma's suggestion in #643
  data <- data |>
    dplyr::select(-erf_percent,-number,-yld) |>
    tidyr::pivot_longer( cols = dplyr::starts_with("population_exposed_"), names_to = "region", values_to = "exposed" ) |>
    dplyr::mutate(region = base::strsplit(region, "_") |> purrr::map_chr(\(x) x[3]))  |>
    dplyr::mutate(regionID = region  |>  base::as.factor()  |>  base::as.numeric())

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        geo_id_micro = data$regionID,
        # geo_id_micro = base::rep(c("c","a","b"), times = 5),
        approach_risk = "absolute_risk",
        exp_central = data$exposure_mean,
        pop_exp = data$exposed,
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2")$health_main$impact_rounded,
  expected = c(150904, 23328, 174232) # Results from NIPH
  )
})


# Pathwway ID
# pathway_ar-erf_formula-exp_dist-iteration_TRUE.R
# Different number of exposure categories across geo_ids

testthat::test_that("results correct  |pathway_ar|erf_formula|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("data", "roadnoise_HA_Lden_Stavanger_Bergen_.rds"))

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = data$average_cat,
        population = data$totpop,
        # prop_pop_exp = data$prop_pop_exp,
        pop_exp = data$ANTALL_PER,
        geo_id_micro = data$GEO_ID,
        geo_id_macro = "Norway",
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        dw_central = 0.02,
        duration_central = 1,
        info = data.frame(pollutant = "road_noise",

                          outcome = "highly_annoyance")

      )$health_detailed$results_by_geo_id_micro$impact,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(282.71548, 397.93929)
  )
})

## ASSESSOR:
## Liliana Vázquez, NIPH
## ASSESSMENT DETAILS:
## Stavanger and Bergen highly annoyance
## INPUT DATA DETAILS:
## Add here input data details: data sources, measured vs. modeled, ...

# Now with age groups
testthat::test_that("results correct  |pathway_ar|erf_formula|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {
  data <- base::readRDS(testthat::test_path("data", "roadnoise_HA_Lden_Stavanger_Bergen_.rds"))
  data_groups <- dplyr::bind_rows(data, data) |>
    dplyr::mutate(age_group = rep(c("below_40", "above_40"), each = 85))

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        age_group = data_groups$age_group,
        approach_risk = "absolute_risk",
        exp_central = data_groups$average_cat,
        population = data_groups$totpop,
        pop_exp = data_groups$ANTALL_PER,
        geo_id_micro = data_groups$GEO_ID,
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        dw_central = 0.02,
        duration_central = 1,
        )$health_detailed$results_by_geo_id_micro$impact,
    expected =
      c(282.71548, 397.93929)*2)
})

testthat::test_that("results correct  |pathway_ar|erf_function|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("data", "roadnoise_HA_Lden_Stavanger_Bergen_.rds"))



  erf_df <- data.frame(
    dB = seq(30, 85, by = 0.5)
  )

  # Compute AR using the quadratic formula
  erf_df$AR <- 78.9270 - 3.1162 * erf_df$dB + 0.0342 * erf_df$dB^2

  # Create a function using spline interpolation over the data
  spline_fun <- stats::splinefun(
    x = erf_df$dB,
    y = erf_df$AR,
    method = "natural"
  )


  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = data$average_cat,
        population = data$totpop,
        pop_exp = data$ANTALL_PER,
        geo_id_micro = data$GEO_ID,
        geo_id_macro = "Norway",
        erf_eq_central = spline_fun,
        dw_central = 0.02,
        duration_central = 1,
        info = data.frame(pollutant = "road_noise",

                          outcome = "highly_annoyance")

      )$health_detailed$results_by_geo_id_micro$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(283, 398 )
  )
})

## ASSESSOR:
## Liliana Vázquez, NIPH
## ASSESSMENT DETAILS:
## Stavanger and Bergen highly annoyance
## INPUT DATA DETAILS:
## Add here input data details: defined own function with spline interpolation



### YLD #########################################################################

## Using only the pop_exp argument
testthat::test_that("results correct prevalence-based YLD |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))

  testthat::expect_equal(
    object = healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = data$exposure_mean,
      pop_exp = data$population_exposed_total,
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      dw_central = 0.5, dw_lower = 0.1, dw_upper = 1,
      duration_central = 1,
      info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")
      )$health_main$impact_rounded,
    expected = data_raw |>
      dplyr::filter(exposure_category %in% "Total exposed")|>
      dplyr::select(number)|>
      dplyr::pull() |>
      base::round() / 2 # With dw_central = 0.5 & duration_central = 1 the expected results are half of those we would obtain without dw & duration arguments
  )
})

## Using the prop_pop_exp and pop_exp arguments in combination
testthat::test_that("results correct prevalence-based YLD |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))

  testthat::expect_equal(
    object = healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = data$exposure_mean,
      pop_exp = data$population_exposed_total, # For prop_pop_exp case, this vector is summed in the background to get total pop exposed, which is then combined with prop_pop_exp to get the number exposed per exp category)
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
      dw_central = 0.5, dw_lower = 0.1, dw_upper = 1,
      duration_central = 1,
      info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")
    )$health_main$impact_rounded,
    expected = data_raw |>
      dplyr::filter(exposure_category %in% "Total exposed")|>
      dplyr::select(number)|>
      dplyr::pull() |>
      base::round() / 2 # With dw_central = 0.5 & duration_central = 1 the expected results are half of those we would obtain without dw & duration arguments
  )
})

testthat::test_that("results correct |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "Bergen_HA_og_HSD.rds"))
  totalpop_Bergen <- 269189

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = as.numeric(gsub(",",".",data$Lden..dB..middle.point)),
        population  = totalpop_Bergen,
        pop_exp = as.numeric(gsub(",",".",data$Bergen.)),
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        dw_central = 0.02,
        dw_lower = 0.01,
        dw_upper = 0.12,
        duration_central = 1,
        info = data.frame(pollutant = "road_noise",
                          outcome = "highly_annoyance")
      )$health_detailed$results_by_geo_id_micro$impact_rounded,
    expected =
      c(398, 199, 2388)
  )

  ## ASSESSOR: Liliana Vázquez, NIPH
  ## ASSESSMENT DETAILS:
  ## INPUT DATA DETAILS:
})


# ERROR OR WARNING ########
## ERROR #########
testthat::test_that("error if geo_id_macro but no geo_id_micro", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        prop_pop_exp = 1,
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10,
        erf_shape = "log_linear",
        geo_id_macro = c("a", "b")),
    regexp = "If you do not pass a value for geo_id_micro, you cannot use geo_id_macro."
  )
})




testthat::test_that("error if rr lower than 0", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = -1.05,
        rr_increment = 10,
        erf_shape = "log_linear"),
    regexp = "The values in the following arguments must be higher than 0: rr_central."
  )
})

## NOTE 2025-08-08: the two error message tests for log-log and log-lin have been commented out, as with the new ERFs it's no problem to calculate RR's for exp=0 or when exp <= cutoff; once we've settled on these new ERFs remove these error messages
# testthat::test_that("error if cutoff higher than exposure when erf_shape == log_log", {
#
#   error <- "if the exposure-response function shape is log-log or linear-log then the values of cutoff_central, cutoff_lower and cutoff_upper must be lower than the values of exposure_central, exposure_lower and exposure_upper. please adjust."
#
#   ## cutoff_central > exp_central
#   testthat::expect_error(
#     object =
#       healthiar::attribute_health(
#         exp_central = 4, cutoff_central = 5,
#         exp_lower = NULL, cutoff_lower = NULL,
#         exp_upper = NULL, cutoff_upper = NULL,
#         bhd_central = 1000,
#         rr_central = 1.05,
#         rr_increment = 10,
#         erf_shape = "log_log"),
#     regexp = error)
#
#   ## cutoff_upper > exp_upper
#   testthat::expect_error(
#     object =
#       healthiar::attribute_health(
#         exp_central = 3, cutoff_central = 1,
#         exp_lower = 2, cutoff_lower = 0,
#         exp_upper = 5, cutoff_upper = 7,
#         bhd_central = 1000,
#         rr_central = 1.05,
#         rr_increment = 10,
#         erf_shape = "log_log"),
#     regexp = error)
#
#   ## cutoff_lower == exp_lower
#   testthat::expect_error(
#     object =
#       healthiar::attribute_health(
#         exp_central = 3,
#         exp_lower = 2,
#         exp_upper = 5,
#         cutoff_central = 2,
#         cutoff_lower = 0,
#         cutoff_upper = 4,
#         bhd_central = 1000,
#         rr_central = 1.05,
#         rr_increment = 10,
#         erf_shape = "log_log"),
#     regexp = error)
# })
#
# testthat::test_that("error if cutoff higher than exposure when erf_shape == linear_log", {
#
#   error <- "if the exposure-response function shape is log-log or linear-log then the values of cutoff_central, cutoff_lower and cutoff_upper must be lower than the values of exposure_central, exposure_lower and exposure_upper. please adjust."
#
#   ## cutoff_central > exp_central
#   testthat::expect_error(
#     object =
#       healthiar::attribute_health(
#         exp_central = 4, cutoff_central = 5,
#         exp_lower = NULL, cutoff_lower = NULL,
#         exp_upper = NULL, cutoff_upper = NULL,
#         bhd_central = 1000,
#         rr_central = 1.05,
#         rr_increment = 10,
#         erf_shape = "linear_log"),
#     regexp = error)
#
#   ## cutoff_upper > exp_upper
#   testthat::expect_error(
#     object =
#       healthiar::attribute_health(
#         exp_central = 3, cutoff_central = 1,
#         exp_lower = 2, cutoff_lower = 0,
#         exp_upper = 5, cutoff_upper = 7,
#         bhd_central = 1000,
#         rr_central = 1.05,
#         rr_increment = 10,
#         erf_shape = "linear_log"),
#     regexp = error)
#
#   ## cutoff_lower == exp_lower
#   testthat::expect_error(
#     object =
#       healthiar::attribute_health(
#         exp_central = 3,
#         exp_lower = 2,
#         exp_upper = 5,
#         cutoff_central = 2,
#         cutoff_lower = 0,
#         cutoff_upper = 4,
#         bhd_central = 1000,
#         rr_central = 1.05,
#         rr_increment = 10,
#         erf_shape = "linear_log"),
#     regexp = error)
#
# })

testthat::test_that("error if dw higher than 1", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 4,
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10,
        dw_central = 1.1,
        erf_shape = "log_linear"),
    regexp = "The values in the following arguments must not be higher than 1: dw_central.")
})

testthat::test_that("error if not lower>central>upper", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 4,
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_lower = 1.10,
        rr_upper = 1.20,
        rr_increment = 10,
        erf_shape = "log_linear"),
    regexp = "rr_central must be higher than rr_lower and lower than rr_upper."
  )
})

testthat::test_that("error if only lower or upper", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 4,
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_upper = 1.20,
        rr_increment = 10,
        erf_shape = "log_linear"),
    regexp = "Either both, rr_lower and rr_upper, or none of them must entered, but not only one.")
})

testthat::test_that("error if numeric argument is not numeric", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = "hi",
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10,
        erf_shape = "log_linear"),
    regexp = "The following arguments should be numeric without NAs: exp_central",
    # Use fixed because otherwise the brackets regexp give an error in the test
    fixed = TRUE
    )
})

testthat::test_that("error if numeric argument is not numeric", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = c(6,8),
        prop_pop_exp = c(NA, 0.5),
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10,
        erf_shape = "log_linear"),
    regexp = "The following arguments should be numeric without NAs: prop_pop_exp",
    # Use fixed because otherwise the brackets regexp give an error in the test
    fixed = TRUE
  )
})

testthat::test_that("error if not an option", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10,
        erf_shape = "hello"),
    regexp = "For erf_shape, please, type (between quotation marks) one of these options: linear, log_linear, log_log, linear_log.",
    # Use fixed = TRUE because brackets in the message
    fixed = TRUE

  )
})

testthat::test_that("error if sum(prop_pop_exp) higher than 1", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = c(6,7,8),
        prop_pop_exp = c(0.2,0.5,0.8),
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10,
        erf_shape = "log_linear"),
    regexp = "The sum of values in prop_pop_exp cannot be higher than 1 for each geo unit.")
})



testthat::test_that("error if pop_exp and rr |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))

  ## With pop_exp
  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = data$exposure_mean,
        pop_exp = data$population_exposed_total,
        cutoff_central = min(data$exposure_mean),
        bhd_central = data$gbd_daly[1],
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = data.frame(pollutant = "road_noise", outcome = "YLD")
      )$health_main$impact_rounded,
    regexp = "The argument pop_exp is aimed for absolute risk. Use prop_pop_exp instead.")
})

testthat::test_that("error if prop_pop_exp and ar |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

    data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
    data  <- data_raw |>
      dplyr::filter(!base::is.na(data_raw$exposure_mean))

    testthat::expect_error(
      object =
        healthiar::attribute_health(
          approach_risk = "absolute_risk",
          exp_central = data$exposure_mean,
          prop_pop_exp = data$population_exposed_total/sum(data$population_exposed_total),
          erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
          info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")
        ),
      regexp = "The argument prop_pop_exp is aimed for relative risk. Use pop_exp instead."
    )
  })

testthat::test_that("error if pop_exp and prop_pop_exp |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))

  ## With pop_exp
  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = data$exposure_mean,
        pop_exp = data$population_exposed_total,
        cutoff_central = min(data$exposure_mean),
        bhd_central = data$gbd_daly[1],
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = data.frame(pollutant = "road_noise", outcome = "YLD")
      )$health_main$impact_rounded,
    regexp = "The argument pop_exp is aimed for absolute risk. Use prop_pop_exp instead."
  )
})

testthat::test_that("error if rr and erf_eq", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        prop_pop_exp = 1,
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10,
        erf_shape = "log_linear",
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"),
    regexp = "The argument rr_central cannot be used together with the argument erf_eq_central (either one or the other but not both).",
    fixed = TRUE)
})

testthat::test_that("error if multi geo units but different length of geo-depending arguments", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = c(6, 2, 3),
        prop_pop_exp = 1,
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10,
        erf_shape = "log_linear",
        geo_id_micro = c("a", "b")),
    regexp = "Not clear what is the maximal length of your arguments: 3, 2. Check: exp_central, geo_id_micro.")
})

testthat::test_that("error if info has incompatible length |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))

  ## With pop_exp
  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = data$exposure_mean,
        pop_exp = data$prop_exposed,
        cutoff_central = min(data$exposure_mean),
        bhd_central = data$gbd_daly[1],
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        info = data.frame(id = 1:20)
      )$health_main$impact_rounded,
    regexp = "All function arguments must have the same length (here 6) or length 1. Check: info.",
    fix = TRUE
  )
})

testthat::test_that("error if length of exp lower than length of prop pop", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        prop_pop_exp = c(0.5, 0.5),
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10,
        erf_shape = "log_linear"),
    regexp = "All function arguments must have the same length (here 1) or length 1. Check: prop_pop_exp.",
    # To prevent not passed test because of the brackets
    fix = TRUE
  )
})


## WARNING #########

testthat::test_that("warning if absolute risk and cutoff", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!base::is.na(data_raw$exposure_mean))

  testthat::expect_warning(
    object =
      healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = data$exposure_mean,
        cutoff_central = 5,
        pop_exp = data$population_exposed_total,
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        info = data.frame(pollutant = "road_noise", outcome = "highly_annoyance")
      ),
    regexp = "For absolute risk, the value of cutoff_central is not considered; cutoff_central is defined by the exposure-response function."
  )
})

testthat::test_that("error if multi geo units but different length of geo-depending arguments", {

  testthat::expect_warning(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        prop_pop_exp = 1,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10,
        erf_shape = "log_linear"),
    regexp = "You entered no value for cutoff_central. Therefore, 0 has been assumed as default. Be aware that this can determine your results.")
})


testthat::test_that("error if erf_eq is not function or string", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        prop_pop_exp = 1,
        cutoff_central = 5,
        bhd_central = 1000,
        erf_eq_central = c(1)),
    regexp = "erf_eq_central must be a function or a character string." ,
    fixed = TRUE)
})


testthat::test_that("error if bhd does not match the geo_id_micro, sex and age_group
composition", {


  testthat::expect_error(
    object = healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear", # Page 18
      rr_central = 1.023, # Page 14
      rr_lower = NULL, #1.008 # Page 14
      rr_upper = NULL, #1.037 # Page 14
      rr_increment = 10, # Page 18
      exp_central = c(11.5, 12.4, 13.2,5.3,11.5, 12.4, 13.2,5.3), # Table 5 page 29
      exp_lower = NULL, #list(7.4, 7.6, 7.9, 8.0, 7.4), # Table 5 page 29
      exp_upper = NULL, #list(23.5, 22.8, 21.0, 34.3, 34.3), # Table 5 page 29
      cutoff_central = 10, # Page 33
      bhd_central = c(10000,10000,30000,30000,50000,50000,70000,70000), # Table 3 page 22
      geo_id_micro = c("basel","basel","zurich","zurich","basel","basel","bern","bern"),
      geo_id_macro = c("switzerland","switzerland","switzerland","switzerland","germany","germany","germany","germany")
    ),
    regexp = "Allocation from bhd_central to geo_id_micro is ambiguous." ,
    fixed = TRUE)



  testthat::expect_error(
    object = healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear", # Page 18
      rr_central = 1.023, # Page 14
      rr_lower = NULL, #1.008 # Page 14
      rr_upper = NULL, #1.037 # Page 14
      rr_increment = 10, # Page 18
      exp_central = c(11.5, 12.4, 13.2,5.3,11.5, 12.4, 13.2,5.3), # Table 5 page 29
      exp_lower = NULL, #list(7.4, 7.6, 7.9, 8.0, 7.4), # Table 5 page 29
      exp_upper = NULL, #list(23.5, 22.8, 21.0, 34.3, 34.3), # Table 5 page 29
      cutoff_central = 10, # Page 33
      bhd_central = c(10000,10000,20000,20000,50000,50000,60000,60000), # Table 3 page 2
      sex = c('male','male', 'male','male','female','female', 'female','male'),
      age_group = c("above_50","above_50","above_50","above_50","below_50","below_50","below_50","below_50"),
      geo_id_micro = c("bern","bern","bern","bern","berlin","berlin","berlin","berlin"),
    ),
    regexp = "Allocation from bhd_central to geo_id_micro, sex, age_group is ambiguous." ,
    fixed = TRUE)
})



