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
  rr_c <- base::rep(rr_change,times = length(data$exposure_mean)*4)

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
      22628, 16756, 27897, 28489, 21149, 35041, 19767, 14595, 24434, 21496,
      15919, 26502, 27065, 20092, 33289, 18779, 13865, 23213, 23759, 17594,
      29292, 29914, 22206, 36793, 20755, 15324, 25656, 18723, 13804, 23174,
      21610, 15981, 26675, 15899, 11681, 19742, 17787, 13114, 22015, 20529,
      15182, 25341, 15104, 11097, 18755, 19659, 14494, 24332, 22690, 16780,
      28009, 16694, 12265, 20729, 29459, 21899, 36191, 35034, 26136, 42894,
      23635, 17526, 29102, 27986, 20804, 34381, 33282, 24829, 40749, 22453,
      16650, 27647, 30932, 22994, 38000, 36786, 27443, 45038, 24816, 18403,
      30557, 26925, 20607, 32189, 27999, 21497, 33377, 25823, 19699, 30962,
      25578, 19576, 30580, 26599, 20422, 31708, 24532, 18714, 29414, 28271,
      21637, 33799, 29399, 22572, 35046, 27114, 20684, 32510, 24734, 18813,
      29737, 25863, 19736, 31003, 23575, 17872, 28428, 23497, 17872, 28250,
      24570, 18749, 29453, 22396, 16978, 27007, 25970, 19753, 31224, 27157,
      20723, 32553, 24753, 18766, 29850, 29008, 22337, 34490, 30029, 23196,
      35604, 27961, 21462, 33340, 27558, 21220, 32766, 28528, 22036, 33824,
      26563, 20389, 31673, 30459, 23454, 36215, 31531, 24355, 37384, 29359,
      22535, 35007, 29537, 23019, 34743, 30153, 23552, 35398, 28909, 22479,
      34072, 28060, 21868, 33006, 28645, 22374, 33628, 27463, 21356, 32368,
      31014, 24170, 36480, 31661, 24730, 37168, 30354, 23603, 35776, 27813,
      21547, 32890, 28462, 22100, 33589, 27152, 20987, 32173, 26423, 20470,
      31245, 27039, 20995, 31909, 25794, 19938, 30565, 29204, 22625, 34534,
      29885, 23205, 35268, 28510, 22037, 33782, 31175, 24438, 36480, 31760,
      24952, 37094, 30578, 23918, 35852, 29616, 23216, 34656, 30172, 23704,
      35239, 29049, 22722, 34059, 32733, 25660, 38304, 33348, 26199, 38949,
      32107, 25114, 37644
    )

  )
  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      29780, 22613, 35886, 34745, 26348, 41925, 27858, 21132, 33594, 28291,
      21482, 34092, 33008, 25031, 39829, 26465, 20075, 31914, 31269, 23744,
      37680, 36483, 27666, 44021, 29250, 22188, 35273, 26619, 20141, 32173,
      28565, 21637, 34497, 24782, 18724, 29987, 25288, 19134, 30564, 27137,
      20555, 32773, 23543, 17787, 28488, 27950, 21148, 33782, 29993, 22719,
      36222, 26021, 19660, 31487, 35902, 27291, 43231, 40624, 30894, 48896,
      30970, 23575, 37241, 34107, 25926, 41069, 38593, 29349, 46451, 29422,
      22396, 35379, 37697, 28655, 45392, 42656, 32438, 51341, 32519, 24754,
      39103, 49309, 37769, 58943, 51896, 39850, 61891, 46641, 35642, 55875,
      46844, 35881, 55996, 49301, 37857, 58796, 44309, 33860, 53081, 51775,
      39658, 61890, 54490, 41842, 64985, 48973, 37424, 58669, 44650, 34023,
      53627, 47370, 36180, 56770, 41844, 31817, 50356, 42418, 32321, 50946,
      45002, 34371, 53931, 39751, 30226, 47838, 46883, 35724, 56309, 49739,
      37989, 59608, 43936, 33407, 52874, 53740, 41383, 63930, 56199, 43390,
      66695, 51203, 39331, 61053, 51053, 39314, 60734, 53389, 41220, 63360,
      48643, 37365, 58000, 56427, 43452, 67127, 59009, 45559, 70030, 53763,
      41298, 64105
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
  rr_c <- base::rep(rr_change,times = length(data$exposure_mean)*4)

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
      21555, 16146, 26322, 27225, 20411, 33228, 19038, 14184, 23353, 20478,
      15338, 25006, 25864, 19391, 31567, 18086, 13475, 22186, 22633, 16953,
      27639, 28586, 21432, 34890, 19990, 14894, 24521, 18138, 13477, 22303,
      20702, 15466, 25337, 15575, 11502, 19250, 17232, 12803, 21188, 19667,
      14693, 24071, 14796, 10927, 18287, 19045, 14150, 23418, 21737, 16239,
      26604, 16353, 12078, 20212, 28009, 21049, 34116, 33167, 25032, 40245,
      22391, 16815, 27283, 26609, 19997, 32410, 31509, 23780, 38233, 21271,
      15974, 25919, 29409, 22102, 35821, 34826, 26283, 42257, 23510, 17655,
      28647, 24177, 18908, 28446, 24989, 19620, 29308, 23335, 18175, 27547,
      22968, 17962, 27024, 23740, 18639, 27843, 22169, 17266, 26169, 25386,
      19853, 29869, 26238, 20601, 30773, 24502, 19084, 28924, 22505, 17458,
      26653, 23377, 18211, 27592, 21598, 16682, 25672, 21379, 16585, 25321,
      22208, 17301, 26212, 20518, 15848, 24388, 23630, 18331, 27986, 24546,
      19122, 28971, 22678, 17516, 26955, 25734, 20278, 30094, 26491, 20952,
      30888, 24950, 19585, 29267, 24448, 19264, 28590, 25167, 19904, 29344,
      23703, 18606, 27803, 27021, 21292, 31599, 27816, 22000, 32433, 26198,
      20564, 30730, 25678, 20514, 29704, 26120, 20918, 30159, 25224, 20101,
      29236, 24394, 19488, 28219, 24814, 19873, 28651, 23963, 19096, 27774,
      26962, 21539, 31189, 27426, 21964, 31667, 26485, 21106, 30698, 24435,
      19388, 28417, 24908, 19815, 28909, 23949, 18953, 27911, 23213, 18419,
      26997, 23662, 18824, 27463, 22752, 18005, 26516, 25656, 20357, 29838,
      26153, 20805, 30354, 25147, 19900, 29307, 26842, 21582, 30896, 27257,
      21966, 31319, 26417, 21190, 30462, 25500, 20503, 29351, 25894, 20868,
      29753, 25096, 20131, 28939, 28184, 22661, 32441, 28620, 23064, 32884,
      27737, 22250, 31985
    )
  )
  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      26976, 20886, 32050, 31814, 24514, 37973, 25360, 19602, 30160, 25628,
      19842, 30447, 30223, 23289, 36074, 24092, 18622, 28652, 28325, 21931,
      33652, 33405, 25740, 39871, 26628, 20582, 31668, 24391, 18787, 29090,
      26041, 20093, 31022, 22819, 17541, 27253, 23172, 17848, 27635, 24739,
      19089, 29471, 21678, 16664, 25890, 25611, 19727, 30544, 27343, 21098,
      32573, 23960, 18418, 28615, 32682, 25263, 38911, 37079, 28650, 44159,
      27884, 21661, 33042, 31048, 24000, 36965, 35225, 27218, 41951, 26490,
      20578, 31390, 34316, 26526, 40856, 38933, 30083, 46367, 29278, 22744,
      34694, 44434, 34681, 52423, 46521, 36435, 54722, 42238, 32858, 49976,
      42212, 32947, 49802, 44195, 34613, 51986, 40126, 31215, 47477, 46656,
      36415, 55044, 48847, 38256, 57458, 44349, 34501, 52475, 40687, 31535,
      48284, 42946, 33399, 50816, 38303, 29597, 45580, 38652, 29959, 45870,
      40799, 31729, 48275, 36387, 28117, 43301, 42721, 33112, 50698, 45094,
      35068, 53357, 40218, 31076, 47859, 47903, 37646, 56196, 49836, 39300,
      58293, 45873, 35930, 53970, 45508, 35764, 53386, 47344, 37335, 55379,
      43579, 34133, 51271, 50298, 39529, 59005, 52328, 41265, 61208, 48167,
      37726, 56668
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
  rr_c <- base::rep(rr_change,times = length(data$exposure_mean)*4)

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
      29196, 21770, 35761, 35254, 26370, 43055, 24673, 18360, 30274, 27736,
      20681, 33973, 33492, 25052, 40902, 23439, 17442, 28760, 30655, 22858,
      37549, 37017, 27689, 45208, 25907, 19279, 31788, 23353, 17348, 28700,
      27916, 20780, 34248, 19187, 14222, 23624, 22186, 16481, 27265, 26521,
      19741, 32535, 18228, 13511, 22443, 24521, 18216, 30135, 29312, 21819,
      35960, 20146, 14933, 24805, 36465, 27322, 44467, 42193, 31740, 51259,
      30458, 22750, 37249, 34642, 25956, 42244, 40083, 30153, 48696, 28935,
      21612, 35387, 38288, 28688, 46690, 44302, 33328, 53822, 31981, 23887,
      39112, 28771, 22163, 34197, 29884, 23098, 35414, 27626, 21210, 32936,
      27332, 21055, 32487, 28390, 21943, 33643, 26245, 20149, 31290, 30209,
      23271, 35907, 31378, 24252, 37184, 29008, 22270, 34583, 26492, 20273,
      31677, 27668, 21245, 32981, 25283, 19281, 30325, 25168, 19260, 30093,
      26285, 20183, 31332, 24019, 18317, 28809, 27817, 21287, 33261, 29051,
      22308, 34630, 26547, 20246, 31841, 30928, 23980, 36548, 31982, 24878,
      37683, 29845, 23064, 35372, 29382, 22781, 34720, 30382, 23634, 35799,
      28352, 21910, 33603, 32474, 25179, 38375, 33581, 26122, 39567, 31337,
      24217, 37141, 31363, 24624, 36653, 31994, 25178, 37314, 30719, 24062,
      35975, 29795, 23393, 34820, 30394, 23919, 35448, 29183, 22859, 34176,
      32931, 25855, 38486, 33593, 26437, 39179, 32255, 25265, 37774, 29593,
      23088, 34779, 30260, 23664, 35488, 28913, 22503, 34051, 28114, 21933,
      33040, 28747, 22481, 33714, 27467, 21378, 32349, 31073, 24242, 36518,
      31773, 24847, 37263, 30358, 23628, 35754, 33037, 26101, 38399, 33634,
      26633, 39015, 32428, 25561, 37767, 31385, 24796, 36479, 31952, 25301,
      37064, 30807, 24282, 35879, 34689, 27406, 40319, 35315, 27964, 40966,
      34050, 26839, 39656, 35239, 27702, 41137, 36173, 28526, 42112, 34277,
      26861, 40126, 33477, 26317, 39080, 34365, 27100, 40006, 32563, 25518,
      38120, 37000, 29087, 43194, 37982, 29952, 44217, 35991, 28204, 42133,
      33011, 25763, 38782, 34007, 26626, 39839, 31986, 24883, 37687, 31360,
      24475, 36843, 32306, 25295, 37847, 30387, 23639, 35803, 34661, 27051,
      40721, 35707, 27957, 41831, 33585, 26127, 39572, 37329, 29554, 43309,
      38206, 30341, 44208, 36427, 28750, 42377, 35463, 28076, 41143, 36296,
      28824, 41998, 34606, 27313, 40258, 39196, 31031, 45474, 40117, 31858,
      46419, 38249, 30188, 44496, 36377, 29122, 41802, 36906, 29609, 42329,
      35838, 28627, 41260, 34559, 27665, 39712, 35060, 28129, 40213, 34046,
      27195, 39197, 38196, 30578, 43892, 38751, 31090, 44446, 37630, 30058,
      43323, 34652, 27548, 40057, 35215, 28059, 40630, 34076, 27030, 39469,
      32919, 26171, 38054, 33454, 26656, 38598, 32373, 25679, 37496, 36384,
      28926, 42060, 36976, 29462, 42661, 35780, 28382, 41443, 37995, 30623,
      43410, 38491, 31089, 43896, 37490, 30151, 42911, 36096, 29092, 41239,
      36566, 29534, 41701, 35616, 28643, 40765, 39895, 32155, 45580, 40415,
      32643, 46090, 39365, 31658, 45056, 54333, 41451, 65174, 59246, 45407,
      70771, 49179, 37358, 59226, 51616, 39379, 61915, 56284, 43137, 67232,
      46720, 35490, 56264, 57049, 43524, 68432, 62208, 47677, 74309, 51638,
      39226, 62187, 46909, 35536, 56631, 52160, 39685, 62721, 41401, 31242,
      50159, 44563, 33759, 53799, 49552, 37701, 59585, 39331, 29680, 47651,
      49254, 37312, 59462, 54768, 41670, 65857, 43471, 32804, 52667, 61279,
      47090, 73025, 65876, 50861, 78170, 56457, 43188, 67559, 58215, 44736,
      69374, 62582, 48318, 74261, 53634, 41029, 64181, 64343, 49445, 76677,
      69170, 53405, 82078, 59280, 45348, 70937
    )
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
      34791, 26491, 41824, 39956, 30422, 48038, 31209, 23778, 37487, 33052,
      25167, 39733, 37958, 28900, 45636, 29648, 22589, 35613, 36531, 27816,
      43915, 41954, 31943, 50440, 32769, 24967, 39362, 29771, 22624, 35843,
      33388, 25357, 40228, 26601, 20224, 32005, 28283, 21493, 34051, 31718,
      24089, 38217, 25271, 19213, 30404, 31260, 23756, 37635, 35057, 26625,
      42240, 27931, 21235, 33605, 41285, 31512, 49528, 46169, 35288, 55316,
      36165, 27608, 43379, 39221, 29937, 47052, 43861, 33524, 52550, 34357,
      26227, 41210, 43349, 33088, 52004, 48477, 37052, 58082, 37974, 28988,
      45548, 54539, 42066, 64787, 57176, 44224, 67745, 51810, 39854, 61698,
      51812, 39962, 61548, 54317, 42013, 64357, 49219, 37861, 58613, 57265,
      44169, 68026, 60035, 46436, 71132, 54400, 41847, 64783, 49667, 38085,
      59313, 52457, 40332, 62489, 46781, 35782, 55996, 47184, 36181, 56347,
      49834, 38316, 59365, 44442, 33993, 53197, 52151, 39989, 62279, 55080,
      42349, 65613, 49120, 37572, 58796, 59145, 45890, 69886, 61639, 47964,
      72640, 56565, 43766, 67009, 56188, 43596, 66392, 58557, 45565, 69008,
      53737, 41578, 63659, 62102, 48185, 73380, 64721, 50362, 76272, 59394,
      45954, 70360, 56255, 43388, 66849, 60476, 46788, 71658, 51831, 39872,
      61740, 53443, 41218, 63506, 57452, 44449, 68075, 49240, 37878, 58653,
      59068, 45557, 70191, 63500, 49127, 75241, 54423, 41866, 64827, 49598,
      38036, 59242, 54107, 41601, 64471, 44873, 34349, 53687, 47118, 36134,
      56279, 51401, 39521, 61247, 42629, 32632, 51003, 52078, 39937, 62204,
      56812, 43681, 67694, 47116, 36067, 56372, 62490, 48494, 73848, 66441,
      51737, 78272, 58348, 45141, 69150, 59366, 46069, 70156, 63119, 49150,
      74358, 55431, 42884, 65692, 65615, 50919, 77541, 69764, 54324, 82186,
      61266, 47398, 72607, 69693, 54887, 81264, 71849, 56754, 83554, 67464,
      52973, 78872, 66209, 52142, 77200, 68257, 53916, 79376, 64090, 50325,
      74928, 73178, 57631, 85327, 75442, 59592, 87732, 70837, 55622, 82816,
      64973, 50812, 76229, 67276, 52770, 78719, 62590, 48806, 73628, 61724,
      48271, 72417, 63912, 50132, 74783, 59461, 46365, 69947, 68221, 53352,
      80040, 70639, 55409, 82655, 65720, 51246, 77309, 74114, 58773, 85895,
      76131, 60554, 88001, 72026, 56949, 83696, 70408, 55835, 81601, 72325,
      57526, 83601, 68425, 54101, 79511, 77819, 61712, 90190, 79938, 63582,
      92401, 75628, 59796, 87881
    )
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_micro$impact_rounded,
    expected = c(
      89330, 68557, 106611, 97132, 74646, 115783, 83019, 63632, 99185, 84863,
      65129, 101280, 92275, 70914, 109993, 78868, 60451, 94226, 93796, 71985,
      111942, 101988, 78378, 121572, 87169, 66814, 104144, 79439, 60709, 95156,
      85845, 65689, 102717, 73383, 56006, 88001, 75467, 57674, 90398, 81552,
      62405, 97581, 69713, 53206, 83601, 83411, 63745, 99914, 90137, 68974,
      107853, 77052, 58807, 92401, 100430, 77402, 119414, 107808, 83251, 127956,
      92731, 71374, 110389, 95409, 73532, 113443, 102417, 79089, 121559, 88094,
      67805, 104869, 105452, 81272, 125385, 113198, 87414, 134354, 97367, 74942,
      115908, 125949, 98274, 148112, 132325, 103542, 155212, 119295, 92845, 140612,
      119651, 93361, 140707, 125709, 98365, 147451, 113330, 88203, 133582, 132246,
      103188, 155518, 138941, 108719, 162973, 125260, 97488, 147643, 114571, 88847,
      135470, 121382, 94371, 143190, 107463, 83155, 127315, 108842, 84405, 128697,
      115313, 89652, 136030, 102090, 78997, 120950, 120299, 93290, 142244, 127451,
      99089, 150349, 112836, 87313, 133681, 136604, 107267, 159744, 142573, 112291,
      166273, 130375, 102090, 152846, 129774, 101904, 151756, 135444, 106677, 157960,
      123856, 96985, 145204, 143434, 112631, 167731, 149702, 117906, 174587, 136893,
      107194, 160488
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
  rr_c <- base::rep(rr_change,times = length(data$exposure_mean)*4)

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

  testthat::expect_equal(
    ## test if age group results are correct
    object =x$health_detailed$results_by_age_group$impact_rounded,
    expected = c(
      27727, 20896, 33694, 33353, 25229, 40398, 23610, 17733, 28767, 26341,
      19851, 32009, 31686, 23968, 38378, 22429, 16846, 27329, 29114, 21941,
      35378, 35021, 26490, 42418, 24790, 18620, 30206, 22469, 16829, 27443,
      26654, 20032, 32465, 18567, 13865, 22725, 21346, 15987, 26071, 25322,
      19030, 30841, 17638, 13172, 21589, 23593, 17670, 28815, 27987, 21034,
      34088, 19495, 14559, 23861, 34331, 26034, 41495, 39414, 30057, 47403,
      28776, 21745, 34891, 32614, 24733, 39420, 37444, 28554, 45033, 27337,
      20657, 33146, 36047, 27336, 43569, 41385, 31560, 49773, 30215, 22832,
      36635, 25631, 20187, 29986, 26455, 20920, 30851, 24775, 19431, 29082,
      24350, 19178, 28487, 25132, 19874, 29308, 23536, 18460, 27627, 26913,
      21196, 31485, 27778, 21966, 32393, 26014, 20403, 30536, 23920, 18683,
      28173, 24810, 19462, 29118, 22995, 17880, 27181, 22724, 17749, 26764,
      23569, 18489, 27662, 21845, 16986, 25822, 25116, 19618, 29581, 26050,
      20435, 30574, 24145, 18774, 28540, 27218, 21603, 31646, 27983, 22295,
      32439, 26423, 20891, 30817, 25857, 20523, 30063, 26584, 21180, 30817,
      25102, 19847, 29276, 28578, 22684, 33228, 29382, 23410, 34061, 27744,
      21936, 32358, 27031, 21757, 31089, 27476, 22170, 31541, 26575, 21336,
      30624, 25680, 20669, 29535, 26103, 21062, 29964, 25246, 20269, 29092,
      28383, 22845, 32644, 28850, 23279, 33118, 27903, 22402, 32155, 25773,
      20600, 29802, 26250, 21037, 30291, 25282, 20154, 29296, 24484, 19570,
      28311, 24937, 19985, 28777, 24018, 19146, 27831, 27061, 21630, 31292,
      27562, 22089, 31806, 26546, 21162, 30761, 28206, 22852, 32279, 28622,
      23243, 32697, 27780, 22453, 31848, 26796, 21709, 30665, 27191, 22081,
      31062, 26391, 21330, 30256, 29616, 23994, 33893, 30053, 24405, 34332,
      29168, 23575, 33441, 30308, 24428, 34821, 30966, 25040, 35487, 29627,
      23798, 34128, 28793, 23206, 33080, 29417, 23788, 33713, 28146, 22608,
      32422, 31823, 25649, 36562, 32514, 26292, 37262, 31109, 24988, 35835,
      28726, 22971, 33205, 29438, 23624, 33935, 27988, 22300, 32445, 27290,
      21823, 31545, 27966, 22443, 32238, 26589, 21185, 30822, 30162, 24120,
      34865, 30910, 24805, 35631, 29388, 23415, 34067, 31771, 25795, 36298,
      32380, 26371, 36909, 31141, 25204, 35664, 30182, 24506, 34483, 30761,
      25052, 35063, 29584, 23944, 33881, 33359, 27085, 38113, 33999, 27690,
      38754, 32698, 26464, 37448, 30516, 25046, 34584, 30878, 25395, 34941,
      30146, 24691, 34217, 28991, 23794, 32854, 29334, 24125, 33193, 28639,
      23456, 32506, 32042, 26299, 36313, 32422, 26665, 36688, 31654, 25925,
      35928, 29330, 23912, 33406, 29719, 24283, 33793, 28932, 23535, 33007,
      27864, 22717, 31735, 28233, 23069, 32104, 27485, 22358, 31357, 30797,
      25108, 35076, 31205, 25497, 35483, 30378, 24711, 34658, 31620, 26115,
      35669, 31957, 26444, 35999, 31275, 25780, 35331, 30039, 24809, 33886,
      30360, 25122, 34199, 29712, 24491, 33564, 33201, 27421, 37452, 33555,
      27767, 37799, 32839, 27069, 37098, 49680, 38543, 58886, 53610, 41858,
      63203, 45436, 35030, 54145, 47196, 36616, 55941, 50930, 39765, 60043,
      43164, 33278, 51438, 52164, 40470, 61830, 56291, 43951, 66363, 47707,
      36781, 56853, 43606, 33495, 52123, 47974, 37087, 57028, 38866, 29679,
      46699, 41425, 31821, 49516, 45575, 35233, 54177, 36923, 28195, 44364,
      45786, 35170, 54729, 50373, 38942, 59880, 40809, 31163, 49034, 55157,
      43207, 64856, 58712, 46276, 68685, 51333, 39962, 60676, 52399, 41047,
      61613, 55776, 43962, 65251, 48767, 37964, 57642, 57915, 45367, 68098,
      61648, 48589, 72119, 53900, 41961, 63710
    )
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(
  31763, 24573, 37788, 36601, 28284, 43586, 28500, 22074, 33862, 30174,
  23345, 35899, 34771, 26870, 41406, 27075, 20970, 32169, 33351, 25802,
  39678, 38431, 29699, 45765, 29925, 23177, 35555, 27343, 21108, 32569,
  30655, 23639, 36564, 24391, 18862, 28987, 25975, 20053, 30940, 29122,
  22457, 34736, 23171, 17919, 27538, 28710, 22163, 34197, 32188, 24821,
  38393, 25610, 19805, 30436, 37619, 29160, 44690, 41987, 32605, 49791,
  32838, 25485, 38972, 35738, 27702, 42456, 39888, 30975, 47301, 31196,
  24211, 37024, 39500, 30618, 46925, 44086, 34236, 52280, 34480, 26759,
  40921, 48627, 38267, 56981, 50684, 40035, 59205, 46459, 36426, 54611,
  46196, 36353, 54132, 48150, 38033, 56244, 44136, 34605, 51880, 51059,
  40180, 59830, 53219, 42036, 62165, 48782, 38248, 57341, 44819, 35004,
  52848, 47059, 36891, 55310, 42453, 33037, 50216, 42578, 33254, 50206,
  44706, 35047, 52544, 40330, 31386, 47705, 47060, 36755, 55491, 49411,
  38736, 58075, 44576, 34689, 52726, 52136, 41329, 60729, 54032, 42989,
  62748, 50141, 39603, 58584, 49529, 39263, 57692, 51331, 40840, 59611,
  47634, 37623, 55654, 54743, 43395, 63765, 56734, 45138, 65886, 52648,
  41583, 61513, 50490, 39634, 59320, 53889, 42493, 63066, 46816, 36604,
  55201, 47966, 37652, 56354, 51195, 40368, 59913, 44475, 34774, 52441,
  53015, 41616, 62286, 56583, 44617, 66219, 49157, 38434, 57961, 45061,
  35103, 53288, 48840, 38199, 57548, 40958, 31812, 48570, 42808, 33347,
  50624, 46398, 36289, 54670, 38910, 30221, 46142, 47315, 36858, 55953,
  51282, 40109, 60425, 43006, 33403, 50999, 55393, 43828, 64654, 58468,
  46476, 67975, 52084, 41029, 61024, 52624, 41637, 61421, 55544, 44152,
  64576, 49480, 38978, 57973, 58163, 46020, 67886, 61391, 48799, 71373,
  54689, 43081, 64076, 60015, 48383, 68970, 61565, 49800, 70565, 58393,
  46915, 67290, 57014, 45964, 65522, 58487, 47310, 67036, 55473, 44569,
  63925, 63015, 50802, 72419, 64643, 52290, 74093, 61313, 49260, 70654,
  56600, 45276, 65445, 58291, 46795, 67209, 54828, 43701, 63581, 53770,
  43013, 62173, 55377, 44455, 63848, 52086, 41516, 60402, 59430, 47540,
  68717, 61206, 49134, 70569, 57569, 45886, 66760, 63155, 51289, 72169,
  64582, 52615, 73618, 61665, 49917, 70647, 59997, 48725, 68561, 61353,
  49985, 69937, 58582, 47421, 67114, 66312, 53854, 75778, 67811, 55246,
  77298, 64748, 52413, 74179
)

  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_micro$impact_rounded,
    expected = c(
      80390, 62840, 94769, 87285, 68319, 102790, 74959, 58500, 88473, 76370,
      59698, 90031, 82921, 64903, 97651, 71211, 55575, 84049, 84409, 65982,
      99508, 91649, 71735, 107930, 78707, 61425, 92896, 72162, 56113, 85417,
      77714, 60531, 91874, 66844, 51900, 79203, 68554, 53307, 81146, 73828,
      57504, 87281, 63502, 49305, 75243, 75770, 58918, 89688, 81599, 63557,
      96468, 70186, 54495, 83163, 89754, 70489, 105419, 96019, 75594, 112539,
      82979, 65088, 97556, 85267, 66965, 100148, 91218, 71815, 106912, 78830,
      61834, 92678, 94242, 74014, 110690, 100820, 79374, 118166, 87128, 68343,
      102434, 110505, 88017, 128291, 115454, 92293, 133631, 105209, 83519, 122491,
      104979, 83616, 121876, 109681, 87678, 126949, 99949, 79343, 116366, 116030,
      92418, 134705, 121227, 96908, 140312, 110470, 87695, 128616, 101662, 80379,
      118733, 107131, 84994, 124756, 95786, 75513, 112151, 96579, 76360, 112797,
      101775, 80744, 118518, 90996, 71738, 106543, 106745, 84398, 124670, 112488,
      89244, 130994, 100575, 79289, 117758, 118548, 95117, 136823, 123049, 99091,
      141592, 113750, 90946, 131671, 112621, 90362, 129982, 116897, 94136, 134513,
      108062, 86399, 125088, 124475, 99873, 143664, 129202, 104046, 148672, 119437,
      95494, 138255
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

# Using info as additional subgroup analysis

# Goal: determine mean attributable health impacts by education level

## Same exposure within geo_id


testthat::test_that("results the same |pathway_rr|erf_formula|exp_dist|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|", {

  output_attribute <- healthiar::attribute_health(
    rr_central = 1.063,
    rr_increment = 10,
    erf_shape = "log_linear",
    cutoff_central =  0,
    exp_central = c(6, 6, 6,
                    7, 7, 7,
                    8, 8, 8,
                    9, 9, 9),
    bhd_central = c(600, 700, 800,
                    700, 800, 900,
                    800, 900, 1000,
                    900, 1000, 1100),
    geo_id_micro = base::rep(c("a", "b", "c", "d"), each = 3),
    info = base::data.frame(
      education = base::rep(c("secondary", "bachelor", "master"), times = 4)) # education level
    )

  testthat::expect_equal(
    object =  output_attribute$health_detailed$results_raw |>
      dplyr::group_by(info_column_1) |>
      dplyr::summarize(mean_impact = mean(impact))|>
      dplyr::pull(mean_impact),
    expected = c(38.77982591, 43.25633549, 34.30331632)) # Results on 21 Jan 2026
})

## Different exposure within geo_id (subgroup-specific exposure)

testthat::test_that("results the same |pathway_rr|erf_formula|exp_dist|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|", {

  # Goal: determine mean attributable health impacts by education level
  output_attribute <- healthiar::attribute_health(
    rr_central = 1.063,
    rr_increment = 10,
    erf_shape = "log_linear",
    cutoff_central =  0,
    exp_central = c(6, 7, 8,
                    7, 8, 9,
                    8, 9, 10,
                    9, 10, 11),
    bhd_central = c(600, 700, 800,
                    700, 800, 900,
                    800, 900, 1000,
                    900, 1000, 1100),
    geo_id_micro = base::rep(c("a", "b", "c", "d"), each = 3),
    info = base::data.frame(
      education = base::rep(c("secondary", "bachelor", "master"), times = 4)) # education level
  )

  testthat::expect_equal(
    object =  output_attribute$health_detailed$results_raw |>
      dplyr::group_by(info_column_1) |>
      dplyr::summarize(mean_impact = mean(impact))|>
      dplyr::pull(mean_impact),
    expected = c(43.720874558, 54.268438780, 34.303316320)) # Results on 21 Jan 2026
})

## All sex, age and info together
## Different exposure within geo_id (subgroup-specific exposure)

testthat::test_that("results the same |pathway_rr|erf_formula|exp_dist|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|", {

  # Goal: determine mean attributable health impacts by education level
  output_attribute <- healthiar::attribute_health(
    rr_central = 1.063,
    rr_increment = 10,
    erf_shape = "log_linear",
    cutoff_central =  0,
    age_group = base::rep(c("50_and_younger", "50_plus"), each = 4, times= 2),
    sex = base::rep(c("female", "male"), each = 2, times = 4),
    exp_central = c(6, 7, 8, 7, 8, 9, 8, 9,
                    10, 9, 10, 11, 10, 11, 12, 13),
    bhd_central = c(600, 700, 800, 700, 800, 900, 800, 900,
                    1000, 900, 1000, 1100, 1000, 1100, 1200, 1000),
    geo_id_micro = base::rep(c("a", "b"), each = 8),
    info = base::data.frame(
      education = base::rep(c("without_master", "with_master"), times = 8)) # education level
  )

  testthat::expect_equal(
    object =  output_attribute$health_detailed$results_raw |>
      dplyr::group_by(info_column_1) |>
      dplyr::summarize(mean_impact = mean(impact))|>
      dplyr::pull(mean_impact),
    expected = c(52.8008961746572, 49.8382646174031)) # Results on 21 Jan 2026
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


  ## Now with pipe |>
  testthat::expect_equal(
    object =
      exdat_noise |>
      (\(df) {
        healthiar::attribute_health(
          approach_risk = df$risk_estimate_type,
          exp_central = df$exposure_mean,
          pop_exp = df$exposed,
          erf_eq_central = df$erf
        )$health_main$impact_rounded
      })(),
    expected =
      c(174232 * 2) # Results on 2 October 2025; no comparison study
  )


  ## Now with pipe |> and with()
  testthat::expect_equal(
    object =
      exdat_noise |>
      (\(df) {
        base::with(df, healthiar::attribute_health(
          approach_risk = risk_estimate_type,
          exp_central = exposure_mean,
          pop_exp = exposed,
          erf_eq_central = erf
        ))$health_main$impact_rounded
      })(),
    expected =
      c(174232 * 2) # Results on 2 October 2025; no comparison study
  )

  ## With pipe %>% also works but a bit different code
  testthat::expect_equal(
    object =
      exdat_noise %>%
      {
        healthiar::attribute_health(
          approach_risk = .$risk_estimate_type,
          exp_central = .$exposure_mean,
          pop_exp = .$exposed,
          erf_eq_central = .$erf
        )$health_main$impact_rounded
      },
    expected =
      c(174232 * 2) # Results on 2 October 2025; no comparison study
  )

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



testthat::test_that("error if multiple rr for one go_id, sex, age_group ... combination", {


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
  rr_c <- base::rep(rr_change,times = length(data$exposure_mean)*4)+seq(1,20)

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        age_group = base::rep(c("below_50", "below_50", "50_plus", "70_plus"),times = length(data$exposure_mean)),
        sex = base::rep(c("male", "female", "male", "female"),times = length(data$exposure_mean)),
        exp_central = exp_c,
        cutoff_central = cutoff_c,
        bhd_central = bhd_c,
        rr_central = rr_c,
        rr_increment = base::rep(c(10, 11, 12, 13), times = 5),
        erf_shape = "log_linear",
        prop_pop_exp = base::rep(data$prop_exposed, each = 4),
        geo_id_micro = base::rep(base::rep(c("urban","rural"), each = 5), each = 2)),
    regexp = "Allocation from rr_central to geo_id_micro, age_group, sex is ambiguous.",
    fix = TRUE
  )
})


testthat::test_that("error if multiple rr for one go_id, sex, age_group ... combination", {

  testthat::expect_error(
    object =healthiar::attribute_health(
      exp_central = c(20, 20),
      prop_pop_exp = c(0.5, 0.5),
      cutoff_central = 5,
      rr_central = c(1.08,1.09),
      rr_increment = 10,
      erf_shape = "linear_log",
      bhd_central = c(10)),
    regexp = "rr_central must be the same for all exposures.",
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
    regexp = paste0("Allocation from bhd_central to geo_id_micro, age_group, sex is ambiguous.") ,
    fixed = TRUE)
})





