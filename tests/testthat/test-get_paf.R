# QUANTITATIVE TEST ############################################################

testthat::test_that("results correct |get_paf|exp_single|airqplus|", {

  # Validation against WHO AirQ+, which reports the population attributable
  # fraction of a single exposure level as "estimated attributable proportion".
  # The stored AirQ+ run is the COPD example of the Swiss GeLuft assessment:
  # PM2.5 annual mean of 8.85 ug/m3, cut-off 5 ug/m3, log-linear
  # exposure-response function with RR = 1.369 (95% CI 1.12-1.66) per
  # 10 ug/m3, giving an attributable proportion of 11.39% (4.40%-17.80%)
  data <- readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))

  paf_at_rr <- function(rr){
    healthiar::get_paf(
      rr_at_exp =
        healthiar::get_risk(
          rr = rr,
          rr_increment = 10,
          erf_shape = "log_linear",
          exp = data$mean_concentration,
          cutoff = data$cut_off_value),
      # The whole population is exposed to the mean concentration
      prop_pop_exp = 1)
  }

  testthat::expect_equal(
    object =
      round(
        purrr::map_dbl(
          c(central = data$relative_risk,
            lower = data$relative_risk_lower,
            upper = data$relative_risk_upper),
          paf_at_rr),
        digits = 4),
    expected =
      c(central = data$estimated_attributable_proportion_central,
        lower = data$estimated_attributable_proportion_lower,
        upper = data$estimated_attributable_proportion_upper))
})


testthat::test_that("results correct |get_paf|exp_dist|etc_he_2023_11|", {

  # Validation against ETC HE Report 2023/11 (Engelmann et al.,
  # "Environmental noise health risk assessment"), PART III: Calculation
  # Methods, Formula 3:
  #
  #   AFtot = sum(p_i * (RR[Ni] - 1)) / (1 + sum(p_i * (RR[Ni] - 1)))
  #
  # where p_i is "the proportion of the population in each noise exposure
  # category" and RR[Ni] the relative risk at the midpoint of noise band Ni.
  # The parameters are those of the report: all-cause mortality at
  # RR = 1.055 per 10 dB (Table 3.27), effect threshold Lden = 45 dB
  # (Chapter 3.4.3) and the 5 dB band midpoints above the END reporting
  # threshold of 55 dB (Chapter 2).
  midpoint <- c(57, 62, 67, 72, 77)
  # Proportions of the TOTAL population, so they add up to 0.1: the remaining
  # 90% is below the reporting threshold and therefore unexposed
  prop_pop <- c(0.045, 0.030, 0.015, 0.007, 0.003)

  rr_at_exp <-
    healthiar::get_risk(
      rr = 1.055,
      rr_increment = 10,
      erf_shape = "log_linear",
      exp = midpoint,
      threshold = 45)

  # Formula 3 of the report, written out in base R so that the expected value
  # does not come from healthiar itself
  excess <- sum(prop_pop * (rr_at_exp - 1))

  testthat::expect_equal(
    object =
      healthiar::get_paf(
        rr_at_exp = rr_at_exp,
        prop_pop_exp = prop_pop),
    expected = excess / (1 + excess))
})
