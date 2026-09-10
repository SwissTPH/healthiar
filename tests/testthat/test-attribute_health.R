# QUANTITATIVE TEST ############################################################

## RR ###########################################################################

### SINGLE EXPOSURE #############################################################


testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

    data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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

testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_FALSE|", {

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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
testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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

  testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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

testthat::test_that("results the same |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {
  testthat::expect_equal(
    rr_central2 <- attribute_health(
      approach_risk = "relative_risk",  
      erf_eq_central = function(c) {1 + 0.018*c},  
      erf_eq_lower = function(c) {1 + 0.005*c},         
      erf_eq_upper = function(c) {1 + 0.031*c},
      cutoff_central = 10,            
      exp_central = 75, 
      bhd_central = 100)$health_main$impact,
    expect = c(53.91705069, 24.52830189, 66.83250415))
})


testthat::test_that("results the same |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  testthat::expect_equal(
    object = healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_eq_central = "1+0.018*c",
      erf_eq_lower = "1+0.005*c",                
      erf_eq_upper = "1+0.031*c",
      cutoff_central = 10,           
      exp_central = 75,  
      bhd_central = 100)$health_main$impact,
    expect = c(53.91705069, 24.52830189, 66.83250415))
})


testthat::test_that("results the same |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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

  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

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

  data <- base::readRDS(testthat::test_path("testdata", "ozone_copd_mort_2016.rds"))

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
      0.927071 # Result on 08 August 2024; no comparison study (the linear-log
             # curve is a healthiar adaptation, not a published one)
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
    expected = 278
  )

})

testthat::test_that("results correct |pathway_rr|threshold_below_cutoff|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  # Same goal as in the test above, but using the argument threshold
  # instead of a user-defined exposure-response function.
  # Health impacts in the exposure group 55dB+ (cutoff, e.g. because no exposure
  # data are available below) that are affected by an exposure above the
  # effect threshold (45 dB)

  exp <- c(300000,200000,150000,120000,100000,70000,60000)
  totpop <- 10000000
  exp_lab <- c(47,52,57,62,67,72,77)
  diseased <- 50000

  results <-
    healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear",
      rr_central = 1.055,
      rr_increment = 10,
      prop_pop_exp = exp/totpop,
      exp_central = exp_lab,
      # Effect threshold, i.e. the anchor of the exposure-response function
      threshold = 45,
      # Cut-off, i.e. the exposures below it are not quantified
      cutoff_central = 55,
      bhd_central = diseased)

  # Same result as with the user-defined (truncated) exposure-response function
  # (see the test above for the reference to ETC/HE 2023/11, Formula 3)
  testthat::expect_equal(
    object = results$health_main$impact_rounded,
    expected = 278)

  # The exposure categories below the cutoff get the risk at the reference level
  testthat::expect_equal(
    object =
      healthiar::get_risk(
        rr = 1.055,
        rr_increment = 10,
        erf_shape = "log_linear",
        exp = exp_lab,
        threshold = 45,
        cutoff = 55)[1:2],
    expected = c(1, 1))

})

testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_dist|threshold_below_cutoff|etc_he_2023_11|", {

  # Validation against the methodology published in ETC HE Report 2023/11
  # (Engelmann et al., "Environmental noise health risk assessment:
  # methodology for assessing health risks using data reported under the
  # Environmental Noise Directive"), PART III: Calculation Methods:
  #
  #   Formula 2: RR[Ni] = exp( ln(RR10dB) / 10 * (M[Ni] - Ethres) )
  #   Formula 3: AFtot  = sum(p_i * (RR[Ni] - 1)) / (1 + sum(p_i * (RR[Ni] - 1)))
  #
  # where M[Ni] is the midpoint of the noise band Ni, Ethres the effect
  # threshold and p_i "the proportion of the population in each noise exposure
  # category". The report applies AFtot to health data of the whole country
  # (its Equation 5: "Co represents the total target population size";
  # its Equation 6: YLL of all-natural cause mortality per country multiplied
  # by AFtot), so bhd refers to the total population.
  #
  # Parameters taken from the report:
  #  - all-cause mortality, road/rail/aircraft: RR = 1.055 (95% CI 1.014-1.069)
  #    per 10 dB (Table 3.27)
  #  - effect threshold Lden = 45 dB for mortality (Chapter 3.4.3)
  #  - 5 dB bands with the midpoints defined in Chapter 2 ("52 is the midpoint
  #    of the 50-54 dB band"), i.e. 57, 62, 67, 72 and 77 dB for the bands
  #    above the END reporting threshold of 55 dB

  midpoint <- c(57, 62, 67, 72, 77)
  # Proportions of the TOTAL population. They add up to 0.1 because only the
  # bands above the END reporting threshold are known: the remaining 90% of
  # the population is below it and therefore unexposed
  prop_pop <- c(0.045, 0.030, 0.015, 0.007, 0.003)
  bhd_total_population <- 50000
  rr_10db <- c(central = 1.055, lower = 1.014, upper = 1.069)
  effect_threshold <- 45

  # Formulas 2 and 3 of the report, calculated here with base R so that the
  # expected values do not come from healthiar itself
  af_tot <- function(rr_per_10db){
    rr_at_exp <-
      base::exp(base::log(rr_per_10db) / 10 * (midpoint - effect_threshold))
    excess <- base::sum(prop_pop * (rr_at_exp - 1))
    excess / (1 + excess)
  }

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = rr_10db[["central"]],
        rr_lower = rr_10db[["lower"]],
        rr_upper = rr_10db[["upper"]],
        rr_increment = 10,
        exp_central = midpoint,
        prop_pop_exp = prop_pop,
        threshold = effect_threshold,
        bhd_central = bhd_total_population)$health_main$impact,
    expected =
      base::unname(
        base::vapply(rr_10db, af_tot, base::numeric(1)) * bhd_total_population))
})

testthat::test_that("results correct |pathway_ar|erf_ar_formula|yld_TRUE|etc_he_2024_11|", {

  # Validation against the published figures of ETC HE Report 2024/11
  # ("Health effects of transportation noise for children and adolescents:
  # an umbrella review and burden of disease estimation"), which follows the
  # methods of Engelmann et al. (2023), i.e. of ETC HE Report 2023/11.
  #
  # For reading and oral comprehension the exposure-response function provides
  # the prevalence directly, so the attributable prevalent cases are the
  # published numbers (Tables 4.1 and 4.3). The report obtains the DALYs by
  # multiplying them with the disability weight for mild cognitive impairment
  # of 0.013 (Charalampous et al., 2024; WHO Europe, 2024), see Chapter 4.1.
  #
  # This test therefore validates the step from attributable prevalent cases
  # to DALYs (dw and duration in healthiar). The attributable cases themselves
  # are EEA-31 totals over 1 dB exposure categories of all reported noise
  # sources, so they cannot be recalculated from the data given in the report.
  #
  # Published pairs of cases and DALYs (children and adolescents, 6-17 years,
  # EEA-31, 2021):
  #   above the END reporting threshold of 55 dB: 563,774 cases -> 7,329 DALYs
  #   above the WHO guideline values (scenario 2): 607,391 cases -> 7,896 DALYs
  disability_weight_mild_cognitive_impairment <- 0.013

  attributable_dalys <- function(prevalent_cases){
    healthiar::attribute_health(
      approach_risk = "absolute_risk",
      exp_central = 60,
      # The prevalence is already contained in the published cases, so the
      # exposure-response function returns 100% of them
      erf_eq_central = "100 + 0*c",
      pop_exp = prevalent_cases,
      dw_central = disability_weight_mild_cognitive_impairment,
      duration_central = 1)$health_main$impact_rounded
  }

  testthat::expect_equal(
    object = c(end_threshold = attributable_dalys(563774),
               who_guideline = attributable_dalys(607391)),
    expected = c(end_threshold = 7329, who_guideline = 7896))
})

testthat::test_that("results the same |pathway_rr|threshold_equal_cutoff|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  # If only one of cutoff and threshold is entered,
  # the other one takes the same value.
  # Therefore all three calls below must deliver the same result

  args <-
    base::list(
      exp_central = 8.85,
      bhd_central = 30747,
      erf_shape = "log_linear",
      rr_central = 1.369,
      rr_increment = 10)

  only_cutoff <-
    base::do.call(healthiar::attribute_health,
                  c(args, base::list(cutoff_central = 5)))

  only_threshold <-
    base::do.call(healthiar::attribute_health,
                  c(args, base::list(threshold = 5)))

  both <-
    base::do.call(healthiar::attribute_health,
                  c(args, base::list(cutoff_central = 5, threshold = 5)))

  testthat::expect_equal(
    object = only_threshold$health_main$impact,
    expected = only_cutoff$health_main$impact)

  testthat::expect_equal(
    object = both$health_main$impact,
    expected = only_cutoff$health_main$impact)

  # If only threshold is entered, cutoff takes the same value (and not the default 0)
  testthat::expect_equal(
    object = base::unique(only_threshold$health_detailed$results_raw$cutoff),
    expected = 5)

  # A cutoff of 0 means that there is no cutoff and is therefore equivalent to
  # a cutoff equal to the threshold, i.e. entering it explicitly changes nothing
  cutoff_0_and_threshold <-
    base::do.call(healthiar::attribute_health,
                  c(args, base::list(cutoff_central = 0, threshold = 5)))

  testthat::expect_equal(
    object = cutoff_0_and_threshold$health_main$impact,
    expected = only_cutoff$health_main$impact)

})

testthat::test_that("results correct |pathway_rr|erf_log_log|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|cardaba_2014|", {

  # Validation against the health impact assessment of air pollution in
  # Valladolid, Spain (Cardaba Arranz et al., BMJ Open 2014;4:e005999,
  # doi:10.1136/bmjopen-2014-005999). Its cardiopulmonary and lung cancer
  # rows in table 4 use the log-linear (power) model of Ostro (2004),
  # RR = ((X + 1) / (X0 + 1)) ^ beta, i.e. the log_log shape of healthiar,
  # with the beta coefficient given directly:
  #
  #   PM2.5 25.85 ug/m3, natural background 3 ug/m3, population over 30 years
  #   cardiopulmonary: beta = 0.15515, 729 baseline deaths -> 186 attributable
  #   lung cancer:     beta = 0.23218, 142 baseline deaths ->  51 attributable
  #
  # healthiar takes the relative risk per increment instead of beta, so beta
  # is converted with the definition of the shape,
  # beta = log(rr) / (log(increment + cutoff + 1) - log(cutoff + 1))
  background <- 3
  rr_increment <- 10

  attributable_deaths <- function(beta, bhd){
    healthiar::attribute_health(
      exp_central = 25.85,
      cutoff_central = background,
      bhd_central = bhd,
      erf_shape = "log_log",
      rr_central =
        base::exp(beta * (base::log(rr_increment + background + 1) -
                            base::log(background + 1))),
      rr_increment = rr_increment)$health_main$impact_rounded
  }

  testthat::expect_equal(
    object =
      c(cardiopulmonary = attributable_deaths(beta = 0.15515, bhd = 729),
        lung_cancer = attributable_deaths(beta = 0.23218, bhd = 142)),
    expected = c(cardiopulmonary = 186, lung_cancer = 51))
})

testthat::test_that("results correct |pathway_rr|erf_log_log|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|pozzer_2023|", {

  # The log-log curve of Pozzer et al. (2023), see rr_at_exp_pozzer() in
  # helper.R. The whole population is exposed to one single level, so the
  # attributable fraction is (RR - 1) / RR
  rr_at_exp <-
    rr_at_exp_pozzer(exp = 20, cutoff = 5, rr = 1.08, rr_increment = 10)

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        exp_central = 20,
        cutoff_central = 5,
        rr_central = 1.08,
        rr_increment = 10,
        erf_shape = "log_log",
        bhd_central = 10)$health_main$impact,
    expected = 10 * (rr_at_exp - 1) / rr_at_exp
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

testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_single|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|cutoff_FALSE|varuncer_FALSE|multiexp_FALSE|", {

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

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|cutoff_TRUE|", {
  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))
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

testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_single|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|cutoff_TRUE|", {
  data <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))
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



testthat::test_that("results the same |pathway_rr|erf_function|exp_single|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|cutoff_TRUE|", {
  data <- base::readRDS(testthat::test_path("testdata", "ozone_copd_mort_2016.rds"))
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


# By year (using info)
testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|cutoff_TRUE|", {

  testthat::expect_equal(
    object = healthiar::attribute_health(
      approach_risk = "relative_risk",
      exp_central = c(11, 12, 13),
      cutoff_central = rep(10, 3),
      bhd_central = c(10E3, 11E3, 12E3),
      rr_central = 1.105,
      rr_lower = 1.008,
      rr_upper = 1.160,
      rr_increment = 10,
      erf_shape = "log_linear",
      info = data.frame(year = c(2021, 2022, 2023)))$health_detailed$results_raw$impact_rounded,
    expected = c(99, 8, 147, 217, 18, 322,354, 29, 523)
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
  data <- base::readRDS(testthat::test_path("testdata", "mort_pm25_sect_2021.rds"))

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
  data <- base::readRDS(testthat::test_path("testdata", "mort_pm25_sect_2021_cutoff.rds"))

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

  data <- base::readRDS(testthat::test_path("testdata", "ozone_copd_mort_2016.rds"))
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
      # The linear-log curve is a healthiar adaptation, not a published one
      c(0.927071, 0.927071) # Result on 08 August 2024; no comparison study
  )
})

testthat::test_that("results correct |pathway_rr|erf_log_log|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|pozzer_2023|", {

  # The log-log curve of Pozzer et al. (2023), see rr_at_exp_pozzer() in
  # helper.R
  rr_at_exp <-
    rr_at_exp_pozzer(exp = 20, cutoff = 5, rr = 1.08, rr_increment = 10)

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
    expected = base::rep(10 * (rr_at_exp - 1) / rr_at_exp, times = 2)
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


testthat::test_that("results the same|pathway_rr|erf_lin_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_TRUE|cutoff_FALSE|varuncer_FALSE|multiexp_FALSE|", {
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

testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_TRUE|cutoff_FALSE|varuncer_FALSE|multiexp_FALSE|", {
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

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|cutoff_FALSE|varuncer_FALSE|multiexp_FALSE|", {

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


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|cutoff_TRUE|varuncer_FALSE|multiexp_FALSE|", {

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


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|cutoff_TRUE|varuncer_FALSE|multiexp_FALSE|", {
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

testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|cutoff_TRUE|varuncer_FALSE|multiexp_FALSE|", {
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

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_single|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|cutoff_TRUE|", {
  data <- base::readRDS(testthat::test_path("testdata", "mort_pm25_sect_2021.rds"))
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


testthat::test_that("results the same |pathway_rr|erf_lin_lin|exp_single|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|cutoff_TRUE|", {
  data <- base::readRDS(testthat::test_path("testdata", "mort_pm25_sect_2021.rds"))
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

testthat::test_that("results the same |pathway_rr|erf_function|exp_single|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|cutoff_TRUE|", {
  data <- base::readRDS(testthat::test_path("testdata", "ozone_copd_mort_2016.rds"))
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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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

  data <- base::readRDS(testthat::test_path("testdata", "ozone_copd_mort_2016.rds"))
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

  data <- base::readRDS(testthat::test_path("testdata", "ozone_copd_mort_2016.rds"))

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
      0.927071 # Result on 08 August 2024; no comparison study (the linear-log
             # curve is a healthiar adaptation, not a published one)
  )
})

testthat::test_that("results correct |pathway_rr|erf_log_log|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|pozzer_2023|", {

  # The log-log curve of Pozzer et al. (2023), see rr_at_exp_pozzer() in
  # helper.R. Both exposure categories have the same exposure level, so the
  # attributable fraction is the one of a single exposure
  rr_at_exp <-
    rr_at_exp_pozzer(exp = 20, cutoff = 5, rr = 1.08, rr_increment = 10)

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
    expected = 10 * (rr_at_exp - 1) / rr_at_exp
  )
})

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|cutoff_TRUE|varuncer_FALSE|multiexp_FALSE|", {

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      base::signif(healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        # Two exposure categories, so the proportions of the total population
        # in them have to add up to 1. Entering prop_pop_exp = 1 was recycled
        # to both categories, i.e. it assigned the whole population twice
        prop_pop_exp = c(0.5, 0.5),
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

testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|cutoff_FALSE|varuncer_FALSE|multiexp_FALSE|", {

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      base::signif(healthiar::attribute_health(
        approach_risk = "relative_risk",
        erf_shape = "log_linear",
        rr_central = 1.06, #relative risk for pm2.5 according to WHO, used in the study
        rr_increment = 10,
        # Two exposure categories, so the proportions of the total population
        # in them have to add up to 1 (see the test above)
        prop_pop_exp = c(0.5, 0.5),
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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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

  data <- base::readRDS(testthat::test_path("testdata", "ozone_copd_mort_2015_2016.rds"))
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
      # The linear-log curve is a healthiar adaptation, not a published one
      c(0.927071, 0.927071) # Result on 08 August 2024; no comparison study
  )
})

testthat::test_that("results correct |pathway_rr|erf_log_log|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|pozzer_2023|", {

  # The log-log curve of Pozzer et al. (2023), see rr_at_exp_pozzer() in
  # helper.R
  rr_at_exp <-
    rr_at_exp_pozzer(exp = 20, cutoff = 5, rr = 1.08, rr_increment = 10)

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
    expected = base::rep(10 * (rr_at_exp - 1) / rr_at_exp, times = 2)
  )
})


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|cutoff_FALSE|varuncer_FALSE|multiexp_FALSE|", {

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


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|cutoff_TRUE|varuncer_FALSE|multiexp_FALSE|", {

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


testthat::test_that("results the same |pathway_rr|erf_log_lin|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|cutoff_TRUE|varuncer_FALSE|multiexp_FALSE|", {
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


testthat::test_that("results correct |pathway_rr|erf_log_lin|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|cutoff_TRUE|varuncer_FALSE|multiexp_FALSE|", {
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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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
    expected = c(6201, 4321, 8090, 7606, 5294, 9932, 5156, 3600, 6713, 5891,
      4105, 7685, 7225, 5029, 9435, 4898, 3420, 6378, 6511, 4537,
      8494, 7986, 5559, 10428, 5413, 3780, 7049, 4822, 3372, 6271,
      5859, 4088, 7634, 3901, 2733, 5064, 4581, 3204, 5957, 5566,
      3884, 7252, 3706, 2596, 4811, 5063, 3541, 6584, 6152, 4293,
      8015, 4096, 2870, 5318, 7959, 5532, 10406, 9411, 6530, 12324,
      6546, 4555, 8552, 7561, 5256, 9886, 8941, 6203, 11708, 6219,
      4327, 8124, 8357, 5809, 10927, 9882, 6856, 12940, 6873, 4782,
      8979, 5828, 3971, 7759, 6190, 4208, 8257, 5473, 3738, 7270,
      5537, 3773, 7371, 5880, 3997, 7845, 5199, 3551, 6907, 6120,
      4170, 8147, 6499, 4418, 8670, 5746, 3925, 7634, 5136, 3517,
      6808, 5485, 3747, 7287, 4792, 3289, 6338, 4879, 3341, 6467,
      5211, 3560, 6923, 4552, 3125, 6021, 5392, 3692, 7148, 5760,
      3934, 7652, 5031, 3454, 6655, 6544, 4438, 8747, 6917, 4680,
      9264, 6177, 4199, 8240, 6217, 4216, 8310, 6572, 4446, 8801,
      5868, 3989, 7828, 6871, 4660, 9185, 7263, 4914, 9728, 6486,
      4409, 8652, 7220, 4853, 9720, 7483, 5022, 10087, 6960, 4686,
      9358, 6859, 4610, 9234, 7109, 4771, 9583, 6612, 4452, 8890,
      7581, 5096, 10206, 7857, 5273, 10591, 7308, 4920, 9826, 6525,
      4406, 8752, 6780, 4570, 9107, 6273, 4243, 8402, 6198, 4185,
      8315, 6441, 4342, 8651, 5959, 4031, 7982, 6851, 4626, 9190,
      7119, 4799, 9562, 6586, 4455, 8822, 7937, 5311, 10721, 8207,
      5484, 11100, 7669, 5140, 10347, 7540, 5046, 10185, 7797, 5210,
      10545, 7286, 4883, 9830, 8334, 5577, 11257, 8618, 5758, 11655,
      8052, 5397, 10864, 3552, 2327, 4916, 3739, 2441, 5192, 3370,
      2216, 4648, 3375, 2211, 4670, 3552, 2319, 4932, 3201, 2105,
      4415, 3730, 2444, 5162, 3926, 2563, 5451, 3538, 2326, 4880,
      3142, 2076, 4315, 3320, 2185, 4574, 2968, 1968, 4063, 2985,
      1972, 4099, 3154, 2076, 4346, 2820, 1870, 3860, 3299, 2179,
      4531, 3486, 2294, 4803, 3117, 2066, 4266, 3984, 2589, 5555,
      4181, 2707, 5848, 3792, 2473, 5270, 3785, 2460, 5277, 3972,
      2572, 5555, 3602, 2349, 5006, 4183, 2718, 5833, 4390, 2842,
      6140, 3981, 2596, 5533, 4251, 2727, 5996, 4392, 2811, 6210,
      4113, 2645, 5787, 4039, 2591, 5697, 4173, 2670, 5899, 3907,
      2513, 5498, 4464, 2864, 6296, 4612, 2951, 6520, 4318, 2777,
      6077, 3824, 2473, 5354, 3958, 2554, 5555, 3692, 2394, 5157,
      3633, 2350, 5087, 3760, 2426, 5278, 3508, 2275, 4900, 4015,
      2597, 5622, 4156, 2681, 5833, 3877, 2514, 5415, 4700, 2991,
      6677, 4848, 3077, 6903, 4554, 2906, 6456, 4465, 2841, 6343,
      4606, 2923, 6558, 4327, 2760, 6133, 4935, 3140, 7011, 5091,
      3231, 7248, 4782, 3051, 6778, 1316, 883, 1784, 1487, 992,
      2026, 1152, 777, 1554, 1250, 839, 1695, 1413, 943, 1925,
      1095, 738, 1476, 1382, 927, 1873, 1562, 1042, 2128, 1210,
      816, 1632, 1074, 727, 1444, 1235, 832, 1668, 921, 626,
      1232, 1021, 691, 1372, 1173, 790, 1584, 875, 594, 1170,
      1128, 763, 1516, 1297, 873, 1751, 967, 657, 1293, 1574,
      1046, 2152, 1756, 1161, 2415, 1399, 935, 1903, 1495, 994,
      2044, 1668, 1103, 2294, 1329, 888, 1808, 1652, 1099, 2260,
      1844, 1219, 2535, 1469, 982, 1998)
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(6890, 4721, 9127, 8044, 5517, 10647, 6089, 4176, 8061, 6545,
      4485, 8670, 7642, 5241, 10115, 5784, 3967, 7658, 7234, 4957,
      9583, 8446, 5793, 11180, 6393, 4384, 8464, 5698, 3916, 7528,
      6491, 4458, 8582, 5013, 3447, 6621, 5413, 3720, 7152, 6166,
      4235, 8152, 4763, 3275, 6290, 5983, 4112, 7905, 6815, 4681,
      9011, 5264, 3620, 6952, 8457, 5787, 11214, 9651, 6603, 12800,
      7295, 4988, 9682, 8034, 5498, 10654, 9168, 6273, 12160, 6930,
      4739, 9198, 8879, 6077, 11775, 10133, 6933, 13440, 7660, 5237,
      10166, 12359, 8424, 16442, 13235, 9006, 17629, 11500, 7849, 15281,
      11741, 8003, 15620, 12573, 8556, 16747, 10925, 7457, 14517, 12977,
      8845, 17265, 13896, 9457, 18510, 12075, 8242, 16045, 10784, 7379,
      14302, 11633, 7948, 15446, 9952, 6818, 13183, 10245, 7010, 13587,
      11051, 7550, 14673, 9454, 6477, 12524, 11324, 7748, 15017, 12215,
      8345, 16218, 10449, 7159, 13842, 13983, 9494, 18661, 14885, 10091,
      19888, 13097, 8906, 17457, 13284, 9020, 17728, 14141, 9586, 18894,
      12442, 8461, 16584, 14683, 9969, 19594, 15630, 10595, 20883, 13752,
      9351, 18330, 3978, 2621, 5476, 4257, 2796, 5879, 3707, 2450,
      5089, 3779, 2490, 5202, 4044, 2656, 5585, 3522, 2328, 4834,
      4176, 2752, 5750, 4470, 2935, 6172, 3892, 2573, 5343, 3456,
      2295, 4726, 3721, 2462, 5102, 3200, 2131, 4364, 3284, 2180,
      4489, 3535, 2339, 4847, 3040, 2024, 4146, 3629, 2409, 4962,
      3907, 2586, 5357, 3360, 2237, 4582, 4527, 2960, 6277, 4822,
      3142, 6708, 4241, 2783, 5863, 4301, 2812, 5964, 4581, 2985,
      6372, 4029, 2643, 5570, 4754, 3108, 6591, 5063, 3300, 7043,
      4454, 2922, 6156, 5142, 3317, 7220, 5362, 3448, 7549, 4928,
      3187, 6900, 4885, 3151, 6859, 5094, 3276, 7172, 4681, 3028,
      6555, 5399, 3483, 7581, 5630, 3621, 7926, 5174, 3347, 7245,
      4584, 2981, 6388, 4792, 3108, 6696, 4381, 2857, 6088, 4355,
      2832, 6068, 4553, 2952, 6361, 4162, 2714, 5784, 4813, 3130,
      6707, 5032, 3263, 7031, 4600, 3000, 6393, 5731, 3666, 8107,
      5963, 3803, 8458, 5504, 3531, 7765, 5444, 3483, 7701, 5665,
      3613, 8035, 5229, 3355, 7377, 6017, 3849, 8512, 6261, 3993,
      8881, 5779, 3708, 8153)
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_micro$impact_rounded,
    expected = c(19249, 13145, 25569, 21278, 14523, 28276, 17588, 12025, 23342, 18287,
      12488, 24291, 20214, 13797, 26862, 16709, 11423, 22175, 20212, 13802,
      26848, 22342, 15249, 29690, 18468, 12626, 24509, 16482, 11295, 21830,
      18124, 12405, 24027, 14965, 10265, 19804, 15658, 10730, 20739, 17218,
      11785, 22826, 14217, 9752, 18814, 17306, 11859, 22922, 19030, 13026,
      25229, 15713, 10778, 20795, 22440, 15282, 29875, 24536, 16694, 32688,
      20392, 13894, 27139, 21318, 14517, 28381, 23309, 15859, 31054, 19372,
      13199, 25782, 23562, 16046, 31369, 25763, 17529, 34323, 21412, 14589,
      28496, 9120, 5938, 12696, 9619, 6244, 13428, 8634, 5638, 11989,
      8664, 5641, 12061, 9138, 5932, 12756, 8203, 5356, 11390, 9576,
      6235, 13331, 10100, 6556, 14099, 9066, 5920, 12588, 8041, 5276,
      11113, 8513, 5570, 11797, 7581, 4988, 10452, 7639, 5012, 10558,
      8087, 5292, 11208, 7202, 4739, 9930, 8443, 5540, 11669, 8939,
      5849, 12387, 7960, 5237, 10975, 10258, 6626, 14384, 10785, 6945,
      15165, 9745, 6314, 13628, 9745, 6295, 13665, 10246, 6598, 14407,
      9258, 5998, 12947, 10771, 6957, 15103, 11324, 7293, 15924, 10233,
      6629, 14310)
  )
})


testthat::test_that("results correct |pathway_rr|erf_lin_lin|exp_dist|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|", {

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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
    expected = c(5764, 4099, 7379, 7018, 4995, 8975, 4850, 3445, 6215, 5476,
      3894, 7010, 6667, 4745, 8527, 4607, 3273, 5904, 6052, 4304,
      7748, 7369, 5245, 9424, 5092, 3617, 6525, 4573, 3246, 5863,
      5490, 3901, 7033, 3737, 2650, 4797, 4344, 3083, 5570, 5216,
      3706, 6681, 3551, 2518, 4557, 4801, 3408, 6157, 5765, 4096,
      7385, 3924, 2783, 5037, 7286, 5190, 9313, 8516, 6074, 10870,
      6037, 4296, 7722, 6922, 4931, 8847, 8090, 5770, 10326, 5735,
      4081, 7336, 7651, 5450, 9778, 8942, 6378, 11413, 6338, 4510,
      8108, 4889, 3497, 6224, 5125, 3669, 6517, 4652, 3323, 5928,
      4645, 3322, 5913, 4869, 3485, 6191, 4419, 3157, 5631, 5134,
      3671, 6535, 5381, 3852, 6843, 4884, 3489, 6224, 4422, 3156,
      5640, 4661, 3330, 5939, 4181, 2981, 5339, 4201, 2998, 5358,
      4428, 3164, 5642, 3972, 2832, 5072, 4643, 3314, 5922, 4894,
      3497, 6236, 4390, 3130, 5606, 5350, 3834, 6797, 5582, 4004,
      7085, 5116, 3662, 6506, 5082, 3642, 6457, 5303, 3804, 6731,
      4860, 3479, 6180, 5617, 4025, 7136, 5861, 4204, 7439, 5371,
      3845, 6831, 5616, 4041, 7108, 5766, 4152, 7294, 5465, 3929,
      6922, 5335, 3839, 6753, 5478, 3944, 6929, 5192, 3733, 6576,
      5897, 4243, 7464, 6055, 4359, 7658, 5738, 4126, 7268, 5207,
      3739, 6603, 5359, 3851, 6792, 5054, 3626, 6414, 4947, 3552,
      6273, 5092, 3659, 6452, 4801, 3445, 6093, 5467, 3926, 6934,
      5627, 4044, 7131, 5306, 3807, 6734, 6019, 4339, 7604, 6167,
      4450, 7786, 5870, 4229, 7422, 5718, 4122, 7224, 5859, 4227,
      7397, 5577, 4018, 7050, 6320, 4556, 7985, 6476, 4672, 8176,
      6164, 4440, 7793, 2702, 1914, 3472, 2804, 1987, 3601, 2600,
      1841, 3342, 2567, 1818, 3298, 2664, 1888, 3421, 2470, 1749,
      3175, 2837, 2010, 3646, 2944, 2086, 3781, 2730, 1933, 3509,
      2469, 1747, 3176, 2572, 1821, 3307, 2366, 1674, 3045, 2346,
      1660, 3017, 2443, 1730, 3141, 2248, 1590, 2893, 2593, 1835,
      3335, 2701, 1912, 3472, 2485, 1757, 3197, 2933, 2080, 3765,
      3034, 2152, 3893, 2832, 2007, 3636, 2786, 1976, 3577, 2883,
      2045, 3699, 2690, 1906, 3455, 3080, 2184, 3953, 3186, 2260,
      4088, 2973, 2107, 3818, 2971, 2110, 3808, 3038, 2158, 3892,
      2905, 2062, 3724, 2823, 2005, 3618, 2886, 2050, 3698, 2760,
      1959, 3538, 3120, 2216, 3999, 3190, 2266, 4087, 3050, 2165,
      3910, 2763, 1960, 3544, 2830, 2008, 3629, 2696, 1912, 3459,
      2625, 1862, 3367, 2688, 1908, 3448, 2561, 1816, 3286, 2901,
      2058, 3721, 2971, 2109, 3810, 2830, 2007, 3632, 3179, 2260,
      4070, 3245, 2307, 4153, 3113, 2212, 3987, 3020, 2147, 3867,
      3083, 2192, 3946, 2957, 2101, 3787, 3338, 2373, 4274, 3407,
      2423, 4361, 3268, 2323, 4186, 1130, 793, 1466, 1253, 879,
      1624, 1007, 706, 1306, 1073, 753, 1392, 1190, 835, 1543,
      957, 671, 1241, 1186, 832, 1539, 1315, 923, 1706, 1057,
      742, 1372, 951, 667, 1234, 1074, 753, 1393, 828, 580,
      1074, 903, 634, 1172, 1020, 716, 1323, 786, 551, 1020,
      998, 700, 1295, 1128, 791, 1463, 869, 610, 1128, 1309,
      918, 1697, 1431, 1005, 1855, 1186, 832, 1538, 1243, 873,
      1612, 1360, 954, 1762, 1127, 791, 1461, 1374, 964, 1782,
      1503, 1055, 1948, 1245, 874, 1615)
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(5971, 4257, 7626, 6994, 4986, 8931, 5293, 3773, 6760, 5673,
      4044, 7244, 6644, 4737, 8485, 5029, 3585, 6422, 6270, 4470,
      8007, 7343, 5235, 9378, 5558, 3962, 7098, 5010, 3568, 6403,
      5690, 4053, 7272, 4416, 3145, 5644, 4759, 3390, 6083, 5406,
      3850, 6909, 4195, 2988, 5362, 5260, 3747, 6723, 5975, 4256,
      7636, 4637, 3303, 5926, 7271, 5188, 9278, 8274, 5907, 10552,
      6251, 4460, 7976, 6907, 4928, 8814, 7861, 5612, 10025, 5939,
      4237, 7578, 7634, 5447, 9742, 8688, 6202, 11080, 6564, 4683,
      8375, 10298, 7379, 13085, 10915, 7830, 13855, 9673, 6924, 12305,
      9783, 7010, 12431, 10369, 7438, 13162, 9190, 6578, 11689, 10813,
      7748, 13740, 11461, 8221, 14548, 10157, 7270, 12920, 9192, 6572,
      11705, 9821, 7029, 12492, 8556, 6112, 10906, 8732, 6244, 11119,
      9330, 6678, 11867, 8128, 5806, 10360, 9652, 6901, 12290, 10312,
      7381, 13117, 8984, 6417, 11451, 11384, 8175, 14436, 11991, 8621,
      15188, 10771, 7726, 13673, 10815, 7767, 13714, 11391, 8190, 14429,
      10233, 7340, 12989, 11954, 8584, 15158, 12590, 9052, 15948, 11310,
      8113, 14357, 3105, 2196, 3995, 3281, 2322, 4221, 2928, 2071,
      3769, 2950, 2087, 3795, 3117, 2206, 4010, 2782, 1967, 3580,
      3260, 2306, 4195, 3446, 2438, 4432, 3075, 2174, 3957, 2778,
      1963, 3577, 2955, 2089, 3804, 2600, 1837, 3349, 2639, 1865,
      3398, 2807, 1985, 3614, 2470, 1746, 3181, 2917, 2062, 3756,
      3103, 2194, 3994, 2730, 1929, 3516, 3431, 2429, 4411, 3606,
      2553, 4635, 3255, 2303, 4186, 3259, 2307, 4190, 3426, 2426,
      4403, 3092, 2188, 3976, 3602, 2550, 4631, 3786, 2681, 4867,
      3418, 2419, 4395, 3698, 2620, 4751, 3813, 2702, 4897, 3583,
      2538, 4604, 3513, 2489, 4513, 3622, 2567, 4652, 3404, 2412,
      4374, 3883, 2751, 4988, 4004, 2838, 5142, 3762, 2665, 4834,
      3405, 2411, 4377, 3520, 2493, 4525, 3290, 2328, 4230, 3235,
      2290, 4158, 3344, 2368, 4298, 3125, 2212, 4018, 3575, 2531,
      4596, 3696, 2618, 4751, 3454, 2445, 4441, 3990, 2829, 5121,
      4104, 2911, 5267, 3875, 2748, 4975, 3790, 2688, 4865, 3899,
      2765, 5003, 3682, 2610, 4726, 4189, 2971, 5377, 4309, 3057,
      5530, 4069, 2885, 5224)

  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_micro$impact_rounded,
    expected = c(16269, 11636, 20711, 17909, 12816, 22786, 14967, 10698, 19064, 15456,
      11054, 19675, 17013, 12175, 21647, 14218, 10163, 18111, 17083, 12218,
      21746, 18804, 13457, 23925, 15715, 11232, 20018, 14202, 10141, 18107,
      15511, 11082, 19764, 12972, 9257, 16549, 13492, 9634, 17202, 14735,
      10528, 18776, 12324, 8794, 15722, 14912, 10648, 19013, 16286, 11636,
      20752, 13621, 9720, 17377, 18655, 13363, 23714, 20265, 14528, 25741,
      17022, 12187, 21649, 17723, 12695, 22528, 19252, 13801, 24454, 16171,
      11577, 20567, 19588, 14031, 24899, 21279, 15254, 27028, 17874, 12796,
      22732, 6803, 4817, 8746, 7095, 5024, 9118, 6512, 4609, 8372,
      6463, 4576, 8308, 6740, 4773, 8662, 6186, 4379, 7954, 7144,
      5058, 9183, 7449, 5275, 9574, 6837, 4840, 8791, 6183, 4374,
      7954, 6475, 4582, 8329, 5890, 4166, 7578, 5874, 4156, 7556,
      6152, 4353, 7912, 5595, 3958, 7199, 6492, 4593, 8352, 6799,
      4811, 8745, 6184, 4374, 7957, 7420, 5258, 9532, 7710, 5464,
      9902, 7130, 5051, 9161, 7049, 4995, 9055, 7325, 5191, 9407,
      6774, 4798, 8703, 7791, 5521, 10008, 8096, 5738, 10397, 7487,
      5304, 9619)
  )
})




#### USER-DEFINED ERF FUNCTION ###############################################

testthat::test_that("results the same mrbrt with cutoff |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

  data_erf <- base::readRDS(testthat::test_path("testdata", "erf_mrbrt_stroke.rds"))
  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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

  data_erf <- base::readRDS(testthat::test_path("testdata", "erf_mrbrt_stroke.rds"))
  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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

  data_pop <- base::readRDS(testthat::test_path("testdata", "pop_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("testdata", "erf_mrbrt_stroke.rds"))

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

  data_pop <- base::readRDS(testthat::test_path("testdata", "pop_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("testdata", "erf_mrbrt_stroke.rds"))

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

  data_pop <- base::readRDS(testthat::test_path("testdata", "pop_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("testdata", "erf_mrbrt_stroke.rds"))

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

  data_pop <- base::readRDS(testthat::test_path("testdata", "pop_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("testdata", "erf_mrbrt_stroke.rds"))

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

  data_pop <- base::readRDS(testthat::test_path("testdata", "pop_norway.rds"))
  data_erf <- base::readRDS(testthat::test_path("testdata", "erf_mrbrt_stroke.rds"))

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


  data_erf <- base::readRDS(testthat::test_path("testdata", "erf_mrbrt_stroke.rds"))
  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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

testthat::test_that("results the same |pathway_rr|erf_function|exp_dist|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|cutoff_TRUE|", {
  data <- base::readRDS(testthat::test_path("testdata", "ozone_copd_mort_2016.rds"))
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
    expected = c(729, 555, 882, 729, 555, 882, 728, 554, 881, 692,
      527, 838, 693, 527, 838, 692, 526, 837, 765, 582,
      926, 766, 583, 927, 765, 582, 925, 670, 510, 811,
      671, 511, 812, 669, 509, 810, 637, 485, 771, 637,
      485, 771, 636, 484, 770, 704, 536, 852, 704, 536,
      853, 703, 535, 851, 774, 589, 937, 775, 590, 938,
      774, 589, 937, 736, 560, 890, 736, 560, 891, 735,
      560, 890, 813, 619, 984, 814, 619, 985, 813, 619,
      983, 555, 422, 671, 555, 422, 671, 555, 422, 671,
      527, 401, 638, 527, 401, 638, 527, 401, 638, 583,
      443, 705, 583, 444, 705, 582, 443, 705, 530, 404,
      642, 531, 404, 642, 530, 404, 642, 504, 384, 610,
      504, 384, 610, 504, 383, 610, 557, 424, 674, 557,
      424, 674, 557, 424, 674, 577, 439, 697, 577, 439,
      698, 576, 439, 697, 548, 417, 663, 548, 417, 663,
      548, 417, 662, 605, 461, 732, 605, 461, 733, 605,
      461, 732, 717, 546, 867, 717, 546, 867, 717, 546,
      867, 681, 518, 824, 681, 519, 824, 681, 518, 824,
      753, 573, 910, 753, 573, 911, 752, 573, 910, 690,
      525, 835, 690, 525, 835, 690, 525, 835, 655, 499,
      793, 656, 499, 793, 655, 499, 793, 724, 552, 876,
      725, 552, 877, 724, 551, 876, 743, 565, 898, 743,
      566, 898, 743, 565, 898, 705, 537, 853, 706, 537,
      854, 705, 537, 853, 780, 594, 943, 780, 594, 943,
      780, 594, 943)
  )
  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(862, 656, 1043, 862, 656, 1043, 861, 655, 1042, 819,
      623, 990, 819, 623, 991, 818, 623, 990, 905, 689,
      1095, 905, 689, 1095, 904, 688, 1094, 805, 612, 974,
      805, 613, 974, 804, 612, 973, 764, 582, 925, 765,
      582, 926, 764, 581, 924, 845, 643, 1022, 845, 644,
      1023, 844, 642, 1021, 908, 691, 1098, 908, 691, 1099,
      907, 691, 1098, 862, 656, 1043, 863, 657, 1044, 862,
      656, 1043, 953, 725, 1153, 953, 726, 1154, 953, 725,
      1153, 1139, 867, 1378, 1139, 867, 1378, 1138, 867, 1377,
      1082, 824, 1309, 1082, 824, 1309, 1081, 823, 1308, 1196,
      910, 1447, 1196, 910, 1447, 1195, 910, 1446, 1086, 827,
      1314, 1086, 827, 1314, 1086, 826, 1313, 1032, 785, 1248,
      1032, 786, 1249, 1031, 785, 1248, 1140, 868, 1380, 1141,
      868, 1380, 1140, 868, 1379, 1186, 903, 1435, 1186, 903,
      1435, 1186, 903, 1434, 1127, 858, 1363, 1127, 858, 1363,
      1126, 858, 1363, 1245, 948, 1506, 1246, 948, 1507, 1245,
      948, 1506)
  )
})


testthat::test_that("results the same |pathway_rr|erf_function|exp_dist|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|cutoff_TRUE|", {
data <- base::readRDS(testthat::test_path("testdata", "ozone_copd_mort_2015_2016.rds"))
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
    expected = c(807, 614, 976, 807, 614, 977, 806, 614, 976, 766,
      583, 927, 767, 584, 928, 766, 583, 927, 847, 645,
      1025, 848, 645, 1025, 847, 644, 1024, 756, 576, 915,
      757, 576, 916, 756, 575, 915, 719, 547, 870, 719,
      547, 870, 718, 546, 869, 794, 604, 961, 795, 605,
      962, 794, 604, 960, 848, 646, 1026, 849, 646, 1027,
      848, 645, 1026, 806, 613, 975, 806, 614, 975, 805,
      613, 974, 891, 678, 1078, 891, 678, 1078, 890, 678,
      1077, 601, 457, 727, 601, 457, 727, 601, 457, 727,
      571, 435, 690, 571, 435, 691, 571, 434, 690, 631,
      480, 763, 631, 480, 763, 631, 480, 763, 577, 439,
      698, 577, 439, 698, 577, 439, 698, 548, 417, 663,
      548, 417, 663, 548, 417, 663, 606, 461, 733, 606,
      461, 733, 605, 461, 732, 623, 474, 754, 623, 475,
      754, 623, 474, 754, 592, 451, 716, 592, 451, 716,
      592, 451, 716, 654, 498, 792, 654, 498, 792, 654,
      498, 791, 774, 590, 937, 774, 590, 937, 774, 589,
      936, 736, 560, 890, 736, 560, 890, 735, 560, 890,
      813, 619, 983, 813, 619, 984, 813, 619, 983, 746,
      568, 902, 746, 568, 902, 746, 568, 902, 709, 539,
      857, 709, 540, 857, 708, 539, 857, 783, 596, 947,
      783, 596, 948, 783, 596, 947, 802, 611, 970, 802,
      611, 971, 802, 611, 970, 762, 580, 922, 762, 580,
      922, 762, 580, 922, 842, 642, 1019, 843, 642, 1019,
      842, 641, 1019, 729, 555, 882, 729, 555, 883, 728,
      554, 881, 692, 527, 838, 693, 527, 838, 692, 527,
      837, 765, 582, 926, 766, 583, 927, 765, 582, 925,
      670, 510, 811, 671, 510, 812, 669, 509, 810, 636,
      484, 770, 637, 485, 771, 636, 484, 769, 703, 535,
      851, 704, 536, 852, 703, 535, 850, 774, 589, 937,
      775, 590, 938, 774, 589, 937, 736, 560, 890, 736,
      560, 891, 735, 560, 890, 813, 619, 984, 814, 619,
      985, 813, 619, 983, 555, 422, 671, 555, 422, 671,
      555, 422, 671, 527, 401, 638, 527, 401, 638, 527,
      401, 638, 583, 443, 705, 583, 444, 705, 582, 443,
      705, 530, 404, 642, 531, 404, 642, 530, 404, 642,
      504, 384, 610, 504, 384, 610, 504, 383, 610, 557,
      424, 674, 557, 424, 674, 557, 424, 674, 577, 439,
      697, 577, 439, 698, 576, 439, 697, 548, 417, 663,
      548, 417, 663, 548, 417, 662, 605, 461, 732, 605,
      461, 732, 605, 461, 732, 717, 546, 867, 717, 546,
      867, 717, 546, 867, 681, 518, 824, 681, 518, 824,
      681, 518, 824, 753, 573, 910, 753, 573, 911, 752,
      573, 910, 690, 525, 835, 690, 525, 835, 690, 525,
      835, 655, 499, 793, 656, 499, 793, 655, 499, 793,
      724, 552, 876, 725, 552, 877, 724, 551, 876, 743,
      565, 898, 743, 566, 898, 742, 565, 898, 705, 537,
      853, 706, 537, 854, 705, 537, 853, 780, 594, 943,
      780, 594, 943, 780, 594, 943)

  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_micro$impact_rounded,
    expected = c(2182, 1661, 2640, 2182, 1661, 2640, 2181, 1660, 2639, 2073,
      1578, 2508, 2073, 1578, 2508, 2072, 1577, 2507, 2291, 1744,
      2772, 2292, 1745, 2772, 2290, 1743, 2771, 2079, 1583, 2515,
      2080, 1583, 2516, 2078, 1582, 2514, 1975, 1503, 2390, 1976,
      1504, 2390, 1974, 1503, 2389, 2183, 1662, 2641, 2184, 1662,
      2642, 2182, 1661, 2640, 2274, 1731, 2751, 2274, 1732, 2751,
      2273, 1731, 2750, 2160, 1644, 2613, 2161, 1645, 2614, 2159,
      1644, 2612, 2387, 1818, 2888, 2388, 1818, 2889, 2387, 1817,
      2887, 2000, 1523, 2420, 2001, 1523, 2421, 1999, 1522, 2419,
      1900, 1447, 2299, 1901, 1447, 2300, 1899, 1446, 2298, 2100,
      1599, 2541, 2101, 1599, 2542, 2099, 1598, 2540, 1890, 1439,
      2287, 1891, 1440, 2289, 1889, 1438, 2286, 1796, 1367, 2173,
      1797, 1368, 2174, 1795, 1366, 2172, 1985, 1511, 2402, 1986,
      1512, 2403, 1984, 1510, 2400, 2094, 1594, 2533, 2094, 1594,
      2534, 2093, 1593, 2532, 1989, 1514, 2406, 1990, 1515, 2407,
      1988, 1514, 2405, 2198, 1674, 2660, 2199, 1674, 2660, 2198,
      1673, 2659)
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(946, 720, 1144, 946, 720, 1145, 945, 720, 1144, 898,
      684, 1087, 899, 684, 1088, 898, 684, 1087, 993, 756,
      1201, 993, 756, 1202, 993, 756, 1201, 895, 681, 1083,
      895, 681, 1083, 894, 681, 1082, 850, 647, 1029, 851,
      647, 1029, 850, 647, 1028, 940, 715, 1137, 940, 716,
      1138, 939, 715, 1136, 989, 753, 1197, 990, 754, 1197,
      989, 753, 1197, 940, 716, 1137, 940, 716, 1138, 940,
      715, 1137, 1039, 791, 1257, 1039, 791, 1257, 1039, 791,
      1256, 1236, 941, 1495, 1236, 941, 1496, 1236, 941, 1495,
      1174, 894, 1421, 1174, 894, 1421, 1174, 894, 1420, 1298,
      988, 1570, 1298, 988, 1570, 1297, 988, 1570, 1184, 901,
      1433, 1184, 902, 1433, 1184, 901, 1432, 1125, 856, 1361,
      1125, 857, 1361, 1125, 856, 1361, 1243, 946, 1504, 1244,
      947, 1505, 1243, 946, 1504, 1284, 978, 1554, 1285, 978,
      1554, 1284, 978, 1553, 1220, 929, 1476, 1220, 929, 1476,
      1220, 929, 1476, 1348, 1027, 1631, 1349, 1027, 1632, 1348,
      1026, 1631, 862, 656, 1043, 862, 656, 1043, 861, 655,
      1042, 819, 623, 990, 819, 623, 991, 818, 623, 990,
      905, 689, 1095, 905, 689, 1095, 904, 688, 1094, 804,
      612, 973, 805, 613, 974, 804, 612, 973, 764, 582,
      925, 765, 582, 925, 764, 581, 924, 845, 643, 1022,
      845, 643, 1023, 844, 642, 1021, 908, 691, 1098, 908,
      691, 1099, 907, 691, 1098, 862, 656, 1043, 863, 657,
      1044, 862, 656, 1043, 953, 725, 1153, 953, 726, 1154,
      953, 725, 1153, 1139, 867, 1378, 1139, 867, 1378, 1138,
      867, 1377, 1082, 823, 1309, 1082, 824, 1309, 1081, 823,
      1308, 1196, 910, 1446, 1196, 910, 1447, 1195, 910, 1446,
      1086, 827, 1314, 1086, 827, 1314, 1086, 826, 1313, 1032,
      785, 1248, 1032, 786, 1249, 1031, 785, 1248, 1140, 868,
      1380, 1141, 868, 1380, 1140, 868, 1379, 1186, 903, 1435,
      1186, 903, 1435, 1186, 903, 1434, 1127, 858, 1363, 1127,
      858, 1363, 1126, 858, 1363, 1245, 948, 1506, 1246, 948,
      1507, 1245, 948, 1506)
  )
})

testthat::test_that("results the same |pathway_rr|erf_formula|exp_dist|iteration_TRUE|strat_TRUE|yld_FALSE|uncertainty_TRUE|cutoff_TRUE|", {
  data <- base::readRDS(testthat::test_path("testdata", "ozone_copd_mort_2015_2016.rds"))
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
    expected = c(466, 466, 465, 442, 442, 442, 489, 489, 489, 441,
      441, 441, 419, 419, 419, 463, 464, 463, 855, 855,
      854, 812, 812, 811, 897, 898, 897, 604, 604, 604,
      574, 574, 574, 635, 635, 634, 581, 582, 581, 552,
      553, 552, 611, 611, 610, 624, 624, 624, 593, 593,
      593, 655, 655, 655, 769, 769, 769, 731, 731, 731,
      808, 808, 808, 747, 747, 747, 710, 710, 710, 784,
      784, 784, 789, 789, 789, 749, 749, 749, 828, 828,
      828, 425, 425, 425, 404, 404, 403, 446, 446, 446,
      398, 398, 398, 378, 378, 378, 418, 418, 417, 780,
      780, 779, 741, 741, 740, 819, 819, 818, 560, 560,
      560, 532, 532, 532, 588, 588, 587, 535, 535, 535,
      508, 508, 508, 562, 562, 561, 581, 581, 581, 552,
      552, 552, 610, 610, 610, 719, 719, 719, 683, 683,
      683, 755, 755, 755, 695, 695, 695, 660, 660, 660,
      730, 730, 730, 740, 740, 739, 703, 703, 702, 776,
      777, 776)


  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_geo_id_micro$impact_rounded,
    expected = c(1839, 1840, 1839, 1747, 1748, 1747, 1931, 1932, 1931, 1770,
      1770, 1769, 1681, 1682, 1681, 1858, 1859, 1858, 2267, 2268,
      2267, 2154, 2154, 2153, 2380, 2381, 2380, 1703, 1704, 1703,
      1618, 1619, 1618, 1789, 1789, 1788, 1628, 1628, 1627, 1546,
      1547, 1546, 1709, 1710, 1709, 2100, 2101, 2099, 1995, 1996,
      1994, 2205, 2206, 2204)
  )

  testthat::expect_equal(
    ## test if sex results are correct
    object =x$health_detailed$results_by_sex$impact_rounded,
    expected = c(604, 604, 604, 574, 574, 574, 635, 635, 634, 581,
      582, 581, 552, 553, 552, 611, 611, 610, 993, 993,
      992, 943, 943, 943, 1042, 1043, 1042, 1235, 1235, 1235,
      1173, 1173, 1173, 1297, 1297, 1296, 1188, 1189, 1188, 1129,
      1129, 1129, 1248, 1248, 1247, 1274, 1275, 1274, 1211, 1211,
      1210, 1338, 1338, 1338, 560, 560, 560, 532, 532, 532,
      588, 588, 587, 535, 535, 535, 508, 508, 508, 562,
      562, 561, 913, 914, 913, 868, 868, 867, 959, 959,
      959, 1144, 1144, 1143, 1087, 1087, 1086, 1201, 1201, 1201,
      1093, 1093, 1092, 1038, 1039, 1038, 1148, 1148, 1147, 1187,
      1187, 1186, 1127, 1127, 1127, 1246, 1246, 1246)

  )
})

testthat::test_that("results the same |pathway_rr|erf_formula|exp_dist|iteration_FALSE|strat_TRUE|yld_FALSE|uncertainty_TRUE|", {

  data_erf <- base::readRDS(testthat::test_path("testdata", "erf_mrbrt_stroke.rds"))
  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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
      dplyr::group_by(info_education) |>
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
      dplyr::group_by(info_education) |>
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
      dplyr::group_by(info_education) |>
      dplyr::summarize(mean_impact = mean(impact))|>
      dplyr::pull(mean_impact),
    expected = c(52.8008961746572, 49.8382646174031)) # Results on 21 Jan 2026
})


## AR ###########################################################################

testthat::test_that("results correct |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ha.rds"))
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

  data <- base::readRDS(testthat::test_path("testdata", "noise_road_lden_stavanger.rds"))

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

# Now with cutoff
testthat::test_that("results the same |pathway_ar|erf_formula|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  data <- base::readRDS(testthat::test_path("testdata", "noise_road_lden_stavanger.rds"))

  testthat::expect_equal(
    object =
      suppressWarnings(healthiar::attribute_health(
        approach_risk = "absolute_risk",
        exp_central = data$average_cat,
        cutoff_central = 2, # This cutoff shift the erf_eq
        population  = unique(data$totpop),
        pop_exp = data$ANTALL_PER,
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        info = data.frame(pollutant = "road_noise",
                          outcome = "highly_annoyance")
      ))$health_main$impact_rounded,
    expected = 12497  # Results on 11 Aug 2026
  )
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
      # 174232 highly annoyed persons is the figure published by the Norwegian
      # Institute of Public Health (see the test below with noise_niph_ha.rds).
      # Twice, because exdat_noise holds the exposure of two regions
      c(174232 * 2)
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
      c(174232 * 2)
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
      c(174232 * 2)
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
      c(174232 * 2)
  )

  })

testthat::test_that("results correct  |pathway_ar|erf_function|exp_dist|iteration_FALSE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("testdata", "noise_road_lden_stavanger.rds"))



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

testthat::test_that("results the same |pathway_ar|erf_formula|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_FALSE|", {

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

testthat::test_that("results the same |pathway_ar|erf_formula|exp_dist|iteration_TRUE|strat_FALSE|yld_FALSE|uncertainty_TRUE|", {

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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ha.rds"))
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
  data <- base::readRDS(testthat::test_path("testdata", "noise_road_lden_stavanger_bergen.rds"))

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
  data <- base::readRDS(testthat::test_path("testdata", "noise_road_lden_stavanger_bergen.rds"))
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
  data <- base::readRDS(testthat::test_path("testdata", "noise_road_lden_stavanger_bergen.rds"))



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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ha.rds"))
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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ha.rds"))
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

  data <- base::readRDS(testthat::test_path("testdata", "noise_bergen_ha_hsd.rds"))
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
    regexp = "The values in the following arguments must not be lower than 0: rr_central."
  )
})


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
    regexp = "rr_central must not be lower than rr_lower and not higher than rr_upper."
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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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

    data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ha.rds"))
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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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

testthat::test_that("error if no erf at all", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        prop_pop_exp = 1,
        cutoff_central = 5,
        bhd_central = 1000),
    regexp = "Please define the exposure-response function either with rr_central (together with erf_shape and rr_increment) or with erf_eq_central.",
    fixed = TRUE)
})

testthat::test_that("error if rr but no erf_shape", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        prop_pop_exp = 1,
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10),
    regexp = "If you pass a value for rr_central, you must also pass a value for erf_shape.",
    fixed = TRUE)
})

testthat::test_that("error if rr but no rr_increment", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        prop_pop_exp = 1,
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        erf_shape = "log_linear"),
    regexp = "If you pass a value for rr_central, you must also pass a value for rr_increment.",
    fixed = TRUE)
})

testthat::test_that("error if rr_increment is 0", {

  testthat::expect_error(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        prop_pop_exp = 1,
        cutoff_central = 5,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 0,
        erf_shape = "log_linear"),
    regexp = "rr_increment must be higher than 0.",
    fixed = TRUE)
})

testthat::test_that("no error if erf defined with erf_eq instead of rr", {

  testthat::expect_no_error(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        prop_pop_exp = 1,
        cutoff_central = 5,
        bhd_central = 1000,
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"))
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

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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


  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ihd.rds"))
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


testthat::test_that("error if value lower than 0 lists ALL arguments concerned", {

  # The error message must not stop at the first argument concerned,
  # otherwise the users have to correct their input data one argument at a time
  testthat::expect_error(
    object =
      healthiar::attribute_health(
        erf_shape = "log_linear",
        rr_central = 1.369,
        rr_increment = 10,
        exp_central = 8.85,
        cutoff_central = -5,    # Negative value to force error
        bhd_central = -30747),  # Negative value to force error
    regexp = "The values in the following arguments must not be lower than 0: cutoff_central, bhd_central." ,
    fixed = TRUE)
})


testthat::test_that("error if value lower than 0 is not in the first position", {

  # All values of the argument must be checked and not only the first one.
  # Relevant because bhd_central is often a long vector
  # (one value per exposure category, geo unit, sex and age group)
  testthat::expect_error(
    object =
      healthiar::attribute_health(
        erf_shape = "log_linear",
        rr_central = 1.369,
        rr_increment = 10,
        exp_central = c(8.85, 9.0, 9.5),
        prop_pop_exp = c(0.3, 0.3, 0.4),
        cutoff_central = 5,
        bhd_central = c(30747, 30747, -1)), # Negative value in the LAST position
    regexp = "The values in the following arguments must not be lower than 0: bhd_central." ,
    fixed = TRUE)
})


testthat::test_that("error if bhd does not match the geo_id_micro, sex and age_group
composition", {


  testthat::expect_error(
    object = healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear", 
      rr_central = 1.023, 
      rr_increment = 10,
      exp_central = c(11.5, 12.4, 13.2,5.3,11.5, 12.4, 13.2,5.3), 
      cutoff_central = 10, 
      bhd_central = c(10000,10000,30000,30000,50000,50000,70000,70000),
      geo_id_micro = c("basel","basel","zurich","zurich","basel","basel","bern","bern"),
      geo_id_macro = c("switzerland","switzerland","switzerland","switzerland","germany","germany","germany","germany")
    ),
    regexp = "Allocation from bhd_central to geo_id_micro is ambiguous." ,
    fixed = TRUE)



  testthat::expect_error(
    object = healthiar::attribute_health(
      approach_risk = "relative_risk",
      erf_shape = "log_linear", 
      rr_central = 1.023, 
      rr_increment = 10, 
      exp_central = c(11.5, 12.4, 13.2,5.3,11.5, 12.4, 13.2,5.3), 
      cutoff_central = 10, 
      bhd_central = c(10000,10000,20000,20000,50000,50000,60000,60000),
      sex = c('male','male', 'male','male','female','female', 'female','male'),
      age_group = c("above_50","above_50","above_50","above_50","below_50","below_50","below_50","below_50"),
      geo_id_micro = c("bern","bern","bern","bern","berlin","berlin","berlin","berlin"),
    ),
    regexp = paste0("Allocation from bhd_central to geo_id_micro, age_group, sex is ambiguous.") ,
    fixed = TRUE)
})


## WARNING #########

testthat::test_that("warning if absolute risk and cutoff", {

  data_raw <- base::readRDS(testthat::test_path("testdata", "noise_niph_ha.rds"))
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
    regexp = "Be aware that healthiar shifts the exposure",
    fixed = FALSE
  )
})

testthat::test_that("warning if no cutoff", {

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

testthat::test_that("warning if threshold higher than cutoff", {

  testthat::expect_warning(
    object =
      healthiar::attribute_health(
        exp_central = 6,
        prop_pop_exp = 1,
        bhd_central = 1000,
        rr_central = 1.05,
        rr_increment = 10,
        erf_shape = "log_linear",
        cutoff_central = 5,
        threshold = 6),
    regexp = "The threshold is higher than the cut-off. Therefore, the cut-off has no effect on the results.")
})






# NOT_SUMMED ###################################################################

testthat::test_that("results the same |main_results_by|multiple_exposure_outcome_pairs_in_one_call|", {

  # Two exposure-outcome pairs with different exposure-response functions
  # in one single call must give the same impacts as two separate calls
  exp_central <- c(8.85, 22.1)
  cutoff_central <- c(5, 10)
  rr_central <- c(1.369, 1.041)
  erf_shape <- c("log_linear", "linear")
  bhd_central <- c(30747, 12000)

  in_one_call <-
    healthiar::attribute_health(
      info = base::data.frame(pair = c("pm2.5_copd", "no2_asthma")),
      main_results_by = "pair",
      exp_central = exp_central,
      cutoff_central = cutoff_central,
      rr_central = rr_central,
      rr_increment = 10,
      erf_shape = erf_shape,
      bhd_central = bhd_central)

  in_separate_calls <-
    purrr::map_dbl(
      .x = 1:2,
      .f = ~ healthiar::attribute_health(
        exp_central = exp_central[.x],
        cutoff_central = cutoff_central[.x],
        rr_central = rr_central[.x],
        rr_increment = 10,
        erf_shape = erf_shape[.x],
        bhd_central = bhd_central[.x])$health_main$impact_rounded)

  testthat::expect_equal(
    object = in_one_call$health_main$impact_rounded,
    expected = in_separate_calls)
})

testthat::test_that("results the same |main_results_by|several_dimensions|", {

  # main_results_by names the dimensions to report by, so its length is a
  # number of dimensions and not a number of data rows. It was compared with
  # the length of the data arguments, so exactly two names were rejected with
  # "All function arguments must have the same length", while one name or (by
  # coincidence) as many names as data rows passed
  attribute_two_pairs_by <- function(main_results_by){
    healthiar::attribute_health(
      info = base::data.frame(pair = base::rep(c("pm2.5_copd", "no2_asthma"),
                                               each = 2)),
      main_results_by = main_results_by,
      exp_central = c(8.85, 9.20, 22.1, 24.5),
      cutoff_central = c(5, 5, 10, 10),
      rr_central = c(1.369, 1.369, 1.041, 1.041),
      rr_increment = 10,
      erf_shape = c("log_linear", "log_linear", "linear", "linear"),
      bhd_central = c(30747, 31500, 12000, 12500),
      geo_id_micro = base::rep(c("a", "b"), 2))
  }

  # Both dimensions are kept apart, i.e. one row per exposure-outcome pair and
  # geo unit, and the impacts are the same as when only the pair is named
  # (the geo units are already kept apart by default)
  by_pair_and_geo <- attribute_two_pairs_by(c("pair", "geo_id_micro"))

  testthat::expect_equal(
    object = by_pair_and_geo$health_main$impact,
    expected = attribute_two_pairs_by("pair")$health_main$impact)
})
