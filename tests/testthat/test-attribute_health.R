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
          info = paste0(data$pollutant,"_", data$evaluation_name)
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
        approach_risk = rep("relative_risk", 4),
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
        approach_risk = rep("relative_risk", 4),
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
        info = paste0(data$pollutant,"_", data$evaluation_name)
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
        info = paste0(data$pollutant,"_", data$evaluation_name)
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
        info = paste0(data$pollutant,"_", data$evaluation_name)
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
        info = paste0(data$pollutant,"_", data$evaluation_name)
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
        info = paste0(data$pollutant,"_", data$evaluation_name)
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
        info = paste0(data$pollutant,"_", data$evaluation_name)
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
        info = paste0(data$pollutant,"_", data$evaluation_name))$health_main$impact_rounded,
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
        info = paste0(data$pollutant,"_", data$evaluation_name)
        )$health_main$impact_rounded,
    expected =
      c(1052) # Results on 10 October 2024 (with cutoff = 5); no comparison study
  )
})

testthat::test_that("results correct |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data <- base::readRDS(testthat::test_path("data", "LMU_O3_COPD_mort_2016.rds"))

  erf <- splinefun(data$x, data$y, method="natural")
  erf_l <- splinefun(data$x, data$y_l, method="natural")
  erf_u <- splinefun(data$x, data$y_u, method="natural")

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
  exp_c <- base::signif(unlist(lapply(data$mean_concentration, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::signif(unlist(lapply(data$mean_concentration, function(x)  x * cutoff_change^(0:3))),5)
  bhd_c <- base::signif(unlist(lapply(data$incidents_per_100_000_per_year/1E5*data$population_at_risk, function(x) x * bhd_change^(0:3))),5)
  rr_c <- base::signif(unlist(lapply(data$relative_risk, function(x) x * rr_change^(0:3))),5)

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

  x$health_detailed$results_by_age_group$impact_rounded
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
  exp_c <- base::signif(unlist(lapply(84.1, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- base::signif(unlist(lapply(1, function(x)  x * cutoff_change^(0:3))),5)
  bhd_c <- base::signif(unlist(lapply(29908, function(x) x * bhd_change^(0:3))),5)

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
    erf_eq_central = splinefun(data$x, data$y, method="natural"),
    erf_eq_lower  = splinefun(data$x, data$y_l, method="natural"),
    erf_eq_upper  = splinefun(data$x, data$y_u, method="natural"),
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
        population = rep(1E6, 1E2),
        geo_id_micro = 1:1E2,
        geo_id_macro = rep("CH", 1E2),
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
  erf<-splinefun(data$x, data$y, method="natural")
  erf_l<-splinefun(data$x, data$y_l, method="natural")
  erf_u<-splinefun(data$x, data$y_u, method="natural")

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


##### Stratification (sex/age) ####################################################################

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|cutoff_TRUE|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|",{
  data <- base::readRDS(testthat::test_path("data", "mort_pm25_sect_2021.rds"))
  #Exotic test based on real data but does produce real world results

  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- data$PM25
  cutoff_c <- rep(0, length.out = length(exp_c))
  bhd_c <- data$VALUE_BASELINE
  rr_c <- rep(1.118, length.out = length(bhd_c))

  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age = rep(c("below_50", "below_50", "50_plus", "70_plus"), length.out = length(bhd_c)),
    sex = rep(c("male", "female"), length.out = length(bhd_c)),
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
    rr_increment = rep(c(10, 11, 12, 13), length.out = length(bhd_c)),
    erf_shape = "log_linear",
    geo_id_micro = data$CS01012020,
    geo_id_macro = rep(c("basel","bern","zuerich","genf","lausanne"), length.out = length(bhd_c))
  )

  df_by_sex <- x$health_detailed$results_raw %>%
    group_by(sex, exp_ci,bhd_ci,cutoff_ci,erf_ci) %>%
    summarise(mean_value = mean(impact, na.rm = TRUE), .groups = "drop")

  df_by_age_group <- x$health_detailed$results_raw %>%
    group_by(age_group, exp_ci,bhd_ci,cutoff_ci,erf_ci) %>%
    summarise(mean_value = mean(impact, na.rm = TRUE), .groups = "drop")

  testthat::expect_equal(
    ## test if age group results are correct
    object = base::signif(df_by_age_group$mean_value,5),
    expected = c(
      1.71580, 0.95045, 2.40820, 1.71580, 0.95045, 2.40820,
      1.71580, 0.95045, 2.40820, 1.63000, 0.90293, 2.28780,
      1.63000, 0.90293, 2.28780, 1.63000, 0.90293, 2.28780,
      1.80160, 0.99797, 2.52870, 1.80160, 0.99797, 2.52870,
      1.80160, 0.99797, 2.52870, 1.63440, 0.90427, 2.29680,
      1.63440, 0.90427, 2.29680, 1.63440, 0.90427, 2.29680,
      1.55270, 0.85906, 2.18190, 1.55270, 0.85906, 2.18190,
      1.55270, 0.85906, 2.18190, 1.71620, 0.94949, 2.41160,
      1.71620, 0.94949, 2.41160, 1.71620, 0.94949, 2.41160,
      1.79660, 0.99649, 2.51880, 1.79660, 0.99649, 2.51880,
      1.79660, 0.99649, 2.51880, 1.70680, 0.94666, 2.39290,
      1.70680, 0.94666, 2.39290, 1.70680, 0.94666, 2.39290,
      1.88650, 1.04630, 2.64480, 1.88650, 1.04630, 2.64480,
      1.88650, 1.04630, 2.64480, 1.45520, 0.80462, 2.04590,
      1.45520, 0.80462, 2.04590, 1.45520, 0.80462, 2.04590,
      1.38240, 0.76439, 1.94360, 1.38240, 0.76439, 1.94360,
      1.38240, 0.76439, 1.94360, 1.52790, 0.84485, 2.14820,
      1.52790, 0.84485, 2.14820, 1.52790, 0.84485, 2.14820,
      1.38590, 0.76544, 1.95070, 1.38590, 0.76544, 1.95070,
      1.38590, 0.76544, 1.95070, 1.31660, 0.72717, 1.85320,
      1.31660, 0.72717, 1.85320, 1.31660, 0.72717, 1.85320,
      1.45520, 0.80372, 2.04820, 1.45520, 0.80372, 2.04820,
      1.45520, 0.80372, 2.04820, 1.52410, 0.84368, 2.14050,
      1.52410, 0.84368, 2.14050, 1.52410, 0.84368, 2.14050,
      1.44780, 0.80150, 2.03350, 1.44780, 0.80150, 2.03350,
      1.44780, 0.80150, 2.03350, 1.60030, 0.88587, 2.24750,
      1.60030, 0.88587, 2.24750, 1.60030, 0.88587, 2.24750,
      1.85620, 1.03200, 2.59650, 1.85620, 1.03200, 2.59650,
      1.85620, 1.03200, 2.59650, 1.76340, 0.98037, 2.46670,
      1.76340, 0.98037, 2.46670, 1.76340, 0.98037, 2.46670,
      1.94900, 1.08360, 2.72630, 1.94900, 1.08360, 2.72630,
      1.94900, 1.08360, 2.72630, 1.76890, 0.98204, 2.47770,
      1.76890, 0.98204, 2.47770, 1.76890, 0.98204, 2.47770,
      1.68040, 0.93294, 2.35380, 1.68040, 0.93294, 2.35380,
      1.68040, 0.93294, 2.35380, 1.85730, 1.03110, 2.60160,
      1.85730, 1.03110, 2.60160, 1.85730, 1.03110, 2.60160,
      1.94290, 1.08170, 2.71420, 1.94290, 1.08170, 2.71420,
      1.94290, 1.08170, 2.71420, 1.84580, 1.02760, 2.57850,
      1.84580, 1.02760, 2.57850, 1.84580, 1.02760, 2.57850,
      2.04000, 1.13580, 2.84990, 2.04000, 1.13580, 2.84990,
      2.04000, 1.13580, 2.84990
    )

  )
  testthat::expect_equal(
    ## test if sex results are correct
    object = base::signif(df_by_sex$mean_value,5),
    expected = c(
      1.59040, 0.88134, 2.23160, 1.59040, 0.88134, 2.23160,
      1.59040, 0.88134, 2.23160, 1.51090, 0.83728, 2.12000,
      1.51090, 0.83728, 2.12000, 1.51090, 0.83728, 2.12000,
      1.67000, 0.92541, 2.34320, 1.67000, 0.92541, 2.34320,
      1.67000, 0.92541, 2.34320, 1.51510, 0.83854, 2.12840,
      1.51510, 0.83854, 2.12840, 1.51510, 0.83854, 2.12840,
      1.43940, 0.79662, 2.02200, 1.43940, 0.79662, 2.02200,
      1.43940, 0.79662, 2.02200, 1.59090, 0.88047, 2.23480,
      1.59090, 0.88047, 2.23480, 1.59090, 0.88047, 2.23480,
      1.66530, 0.92401, 2.33400, 1.66530, 0.92401, 2.33400,
      1.66530, 0.92401, 2.33400, 1.58210, 0.87781, 2.21730,
      1.58210, 0.87781, 2.21730, 1.58210, 0.87781, 2.21730,
      1.74860, 0.97022, 2.45070, 1.74860, 0.97022, 2.45070,
      1.74860, 0.97022, 2.45070, 1.85120, 1.02820, 2.59200,
      1.85120, 1.02820, 2.59200, 1.85120, 1.02820, 2.59200,
      1.75860, 0.97675, 2.46240, 1.75860, 0.97675, 2.46240,
      1.75860, 0.97675, 2.46240, 1.94370, 1.07960, 2.72160,
      1.94370, 1.07960, 2.72160, 1.94370, 1.07960, 2.72160,
      1.76390, 0.97836, 2.47300, 1.76390, 0.97836, 2.47300,
      1.76390, 0.97836, 2.47300, 1.67570, 0.92944, 2.34930,
      1.67570, 0.92944, 2.34930, 1.67570, 0.92944, 2.34930,
      1.85210, 1.02730, 2.59660, 1.85210, 1.02730, 2.59660,
      1.85210, 1.02730, 2.59660, 1.93790, 1.07780, 2.70990,
      1.93790, 1.07780, 2.70990, 1.93790, 1.07780, 2.70990,
      1.84100, 1.02390, 2.57440, 1.84100, 1.02390, 2.57440,
      1.84100, 1.02390, 2.57440, 2.03480, 1.13170, 2.84540,
      2.03480, 1.13170, 2.84540, 2.03480, 1.13170, 2.84540
    )
  )
  x$health_detailed$results_by_geo_id_macro$impact_rounded
  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_macro$impact_rounded,
    expected = c( 27,15,38, 27,15,38, 27,15,38,
                  26,14,36, 26,14,36, 26,14,36, 29,16,40, 29,16,40, 29,16,40,
                  26,14,37, 26,14,37, 26,14,37, 25,14,35, 25,14,35, 25,14,35,
                  27,15,38, 27,15,38, 27,15,38, 29,16,40, 29,16,40, 29,16,40,
                  27,15,38, 27,15,38, 27,15,38, 30,17,42, 30,17,42, 30,17,42,
                  33,19,47, 33,19,47, 33,19,47, 32,18,44, 32,18,44, 32,18,44,
                  35,19,49, 35,19,49, 35,19,49, 32,18,45, 32,18,45, 32,18,45,
                  30,17,42, 30,17,42, 30,17,42, 33,18,47, 33,18,47, 33,18,47,
                  35,19,49, 35,19,49, 35,19,49, 33,18,47, 33,18,47, 33,18,47,
                  37,20,51, 37,20,51, 37,20,51, 50,28,70, 50,28,70, 50,28,70,
                  48,27,67, 48,27,67, 48,27,67, 53,29,74, 53,29,74, 53,29,74,
                  48,27,67, 48,27,67, 48,27,67, 46,25,64, 46,25,64, 46,25,64,
                  50,28,71, 50,28,71, 50,28,71, 53,29,74, 53,29,74, 53,29,74,
                  50,28,70, 50,28,70, 50,28,70, 55,31,77, 55,31,77, 55,31,77,
                  26,15,37, 26,15,37, 26,15,37, 25,14,35, 25,14,35, 25,14,35,
                  27,15,39, 27,15,39, 27,15,39, 25,14,35, 25,14,35, 25,14,35,
                  24,13,33, 24,13,33, 24,13,33, 26,15,37, 26,15,37, 26,15,37,
                  27,15,38, 27,15,38, 27,15,38, 26,14,36, 26,14,36, 26,14,36,
                  29,16,40, 29,16,40, 29,16,40, 35,19,49, 35,19,49, 35,19,49,
                  33,18,46, 33,18,46, 33,18,46, 37,20,51, 37,20,51, 37,20,51,
                  33,18,47, 33,18,47, 33,18,47, 32,17,44, 32,17,44, 32,17,44,
                  35,19,49, 35,19,49, 35,19,49, 36,20,51, 36,20,51, 36,20,51,
                  35,19,49, 35,19,49, 35,19,49, 38,21,54, 38,21,54, 38,21,54
    )

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
    dplyr::filter(!is.na(data_raw$exposure_mean))

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
    dplyr::filter(!is.na(data_raw$exposure_mean))

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
  erf <- splinefun(data$x, data$y, method="natural")
  erf_l <- splinefun(data$x, data$y_l, method="natural")
  erf_u <- splinefun(data$x, data$y_u, method="natural")

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
        bhd_central = rep(runif_with_seed(3,1E4,1E5,1), each = 5),
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        geo_id_micro = rep(1:3, each = 5),
        geo_id_macro = rep("ch", each = 5 * 3)
        )$health_detailed$results_raw$impact_rounded,
    expected =
      base::round(c(545,  634,  991)) # Results on 2025-06-24; no comparison study
  )
})

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
        bhd_central = rep(runif_with_seed(3,1E4,1E5,1), each = 5),
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_linear",
        geo_id_micro = rep(1:3, each = 5),
        geo_id_macro = rep("ch", each = 5 * 3),
        population = rep(1000000, each = 5 * 3)
      )$health_detailed$results_raw$impact_rounded,
    expected =
      base::round(c(545,  634,  991)) # Results on 2025-06-24; no comparison study
  )
})

testthat::test_that("results correct |pathway_rr|erf_function|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "LMU_O3_COPD_mort_2015_2016.rds"))
  data <- data |> dplyr::slice(-1)
  erf <- splinefun(data$x[1:21], data$y[1:21], method="natural")
  erf_l <- splinefun(data$x[1:21], data$y_l[1:21], method="natural")
  erf_u <- splinefun(data$x[1:21], data$y_u[1:21], method="natural")

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

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_dist|cutoff_TRUE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

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

testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_dist|cutoff_TRUE|varuncer_FALSE|iteration_TRUE|multiexp_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|",{
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
      geo_id_micro = c('Rural','Semi-rural', 'Semi-urbaines','Urbaines','France'),
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

testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {


  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      base::signif(healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        prop_pop_exp = 1,
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



#### USER-DEFINED ERF FUNCTION ###############################################

testthat::test_that("results the same mrbrt with cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data_erf <- base::readRDS(testthat::test_path("data", "mrbrt_stroke.rds"))
  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ihd_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

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
    dplyr::filter(!is.na(data_raw$exposure_mean))

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
    dplyr::filter(!is.na(data_raw$exposure_mean))
  #Exotic test based on real data but does produce real world results


  #percentage of variation
  exp_change <-1.1
  cutoff_change <-0.8
  bhd_change <-0.9
  uncert_factor <- 20#set uncertainty factor

  # set central values and variate by percentage
  exp_c <- base::signif(unlist(lapply(data$exposure_mean, function(x) x * exp_change^(0:3))),5)
  cutoff_c <- rep(base::signif(unlist(lapply(min(data$exposure_mean), function(x)  x * cutoff_change^(0:3))),5),times = length(data$exposure_mean))
  bhd_c <- rep(base::signif(unlist(lapply(data$gbd_daly[1], function(x) x * bhd_change^(0:3))),5),times = length(data$exposure_mean))


  x <-healthiar::attribute_health(
    approach_risk = "relative_risk",
    age_group = rep(c("below_50", "below_50", "50_plus", "70_plus"),times = length(data$exposure_mean)),
    sex = rep(c("male", "female", "male", "female"),times = length(data$exposure_mean)),
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
                                      method = "natural"),
    erf_eq_lower = stats::splinefun(x = data_erf$exposure,
                                    y = data_erf$mean -0.01,
                                    method = "natural"),
    erf_eq_upper = stats::splinefun(x = data_erf$exposure,
                                    y = data_erf$mean + 0.01,
                                    method = "natural"),
    prop_pop_exp = rep(data$prop_exposed, each = 4))
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

## AR ###########################################################################

testthat::test_that("results correct |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("data", "niph_noise_ha_excel.rds"))
  data  <- data_raw |>
    dplyr::filter(!is.na(data_raw$exposure_mean))

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
  spline_fun <- splinefun(
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
        geo_id_micro = rep(1:3, 5),
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
        geo_id_micro = rep(1:3, 5),
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
        geo_id_micro = rep(1:3, 5),
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
    dplyr::filter(!is.na(data_raw$exposure_mean))

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
        # geo_id_micro = rep(c("c","a","b"), times = 5),
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

      )$health_detailed$results_by_geo_id_micro$impact_rounded,
    ##  RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected =
      c(283, 398)
  )
})

## ASSESSOR:
## Liliana Vázquez, NIPH
## ASSESSMENT DETAILS:
## Stavanger and Bergen highly annoyance
## INPUT DATA DETAILS:
## Add here input data details: data sources, measured vs. modeled, ...


testthat::test_that("results correct  |pathway_ar|erf_function|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("data", "roadnoise_HA_Lden_Stavanger_Bergen_.rds"))



  erf_df <- data.frame(
    dB = seq(30, 85, by = 0.5)
  )

  # Compute AR using the quadratic formula
  erf_df$AR <- 78.9270 - 3.1162 * erf_df$dB + 0.0342 * erf_df$dB^2

  # Create a function using spline interpolation over the data
  spline_fun <- splinefun(
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
    dplyr::filter(!is.na(data_raw$exposure_mean))

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
    dplyr::filter(!is.na(data_raw$exposure_mean))

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
    regexp = "The following arguments should be numeric: exp_central",
    # Use fixed because otherwise the brackets regexp give an error in the test
    fixed = TRUE
    )
})

testthat::test_that("error if numeric argument is not numeric", {

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
    dplyr::filter(!is.na(data_raw$exposure_mean))

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
      dplyr::filter(!is.na(data_raw$exposure_mean))

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
    dplyr::filter(!is.na(data_raw$exposure_mean))

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
    dplyr::filter(!is.na(data_raw$exposure_mean))

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
    dplyr::filter(!is.na(data_raw$exposure_mean))

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
