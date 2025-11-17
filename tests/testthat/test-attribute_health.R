# QUANTITATIVE TEST ############################################################

## RR ###########################################################################

### SINGLE EXPOSURE #############################################################

testthat::test_that("result correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

    data <- base::readRDS(testthat::test_path("data", "airqplus_pm_copd.rds"))

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

testthat::test_that("zero effect if exp lower than cutoff |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("result correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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
testthat::test_that("result correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

  testthat::test_that("result correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("detailed result the same |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("no error rr_no_error|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("number of rows in detailed results correct |meta_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results correct |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_lin_log|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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



testthat::test_that("results correct |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_log_log|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|", {
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
      signif(c(results_pm$health_main$impact_rounded,results_pm$health_main$pop_fraction),3),
    expected =
      signif(c(1742.00000000,1191.00000000,2265.00000000,0.06902931,0.04721232,0.08977441),3)
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


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|", {
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
      signif(c(results_pm$health_main$impact_rounded,results_pm$health_main$pop_fraction),3),
    expected =
      signif(c(639.0000, 437.0000,  830.0000, 0.0762, 0.0522, 0.0990),3)
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

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|", {
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
      signif(c(results_pm2.5$health_main$impact_rounded,results_pm2.5$health_main$pop_fraction),3),
    expected =
      signif(c(34991.0000, 12472.0000, 54821.0000, 0.0623, 0.0222, 0.0976),3)
  )

  ### SpF report results ###
  # France_impact_Cuttoff5 : 39541. Table 8 page 37
  # France_pop_fraction_Cuttoff5 : 0.071 Table 8 page 37


  ## ASSESSOR: Maria José Rueda Lopez, CSTB and Sabrina Delaunay-Havard, SpF.
  ## ASSESSMENT DETAILS: Estimate the proportion of deaths attributable to PM2.5 exposure in France during the covid19 pandemic
  ## INPUT DATA DETAILS: Etudes et enquêtes. Impact de la pollution de l'air ambiant sur la mortalité en France métropolitaine. Réduction en lien avec le
  ## confinement du printemps 2020 et nouvelles données sur le poids total pour la période 2016-2019. Santé publique France. Avril 2021.
})


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|", {


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

testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_single|cutoff_FALSE|varuncer_FALSE|iteration_FALSE|multiexp_FALSE|", {

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
    object = signif(results_NO2$health_main$pop_fraction,3),
    expected = c(0.146, 0.0785, 0.230))



  # Original results from Paper (close to what attribute health gets):
  # Naples_impact_rounded : 1337 (95% CI: 700 - 2188)
  # Naples_pop_fraction : 13.9 (95% CI: 7.3 - 22.7)

  ## ASSESSOR: Maria José Rueda Lopez, CSTB

  ## ASSESSMENT DETAILS: Estimate the attributable death for exposition to NO2 concentrations in 2013 in Naples, Italy.

  ## INPUT DATA DETAILS: Chianese and Riccio 2024. Long-term variation in exposure to NO2 concentrations in the city of Naples, Italy: Results of a citizen science project
  ## https://doi.org/10.1016/j.scitotenv.2024.172799
})

testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_single|iteration_FALSE|", {


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

#### ITERATION ##################################################################

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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
testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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
testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results correct |pathway_rr|erf_function|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_lin_log|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_log_log|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

#### YLD ########################################################################

testthat::test_that("results the same prevalence-based YLD (duration_central=1) |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same incidence-based YLD (duration_central > 1) |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same prevalence-based YLD |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("detailed results the same prevalence-based YLD |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results correct with cutoff |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same no cutoff |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results correct |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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


testthat::test_that("results the same |pathway_rr|erf_lin_log|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_log_log|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

#### ITERATION ##################################################################
testthat::test_that("results the same no cutoff |pathway_rr|erf_log_lin|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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
testthat::test_that("results the same no cutoff |pathway_rr|erf_log_lin|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results correct |pathway_rr|erf_function|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_lin_log|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_log_log|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

#### USER-DEFINED ERF FUNCTION ###############################################

testthat::test_that("results the same mrbrt with cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same mrbrt no cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same mrbrt with cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same mrbrt no cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same mrbrt with cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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


testthat::test_that("results the same mrbrt with cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

## AR ###########################################################################

testthat::test_that("results correct |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results correct |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results correct |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results correct  |pathway_ar|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("detailed results the same |pathway_ar|erf_formula|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("detailed results the same |pathway_ar|erf_formula|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results correct |pathway_ar|erf_formula|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results correct  |pathway_ar|erf_formula|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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


testthat::test_that("results correct  |pathway_ar|erf_function|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|", {

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
testthat::test_that("results correct prevalence-based YLD |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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
testthat::test_that("results correct prevalence-based YLD |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("results correct |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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



testthat::test_that("error if pop_exp and rr |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("error if prop_pop_exp and ar |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("error if pop_exp and prop_pop_exp |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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

testthat::test_that("error if info has incompatible length |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|", {

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
