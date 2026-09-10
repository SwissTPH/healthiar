# QUANTITATIVE TEST ############################################################

# The three approaches to add up the attributable impacts of two correlated
# exposures are the ones identified by the BEST-COST methodological report
# (Strak, Houthuijs & Staatsen, 2024, "D1.2 Report on the methodology for
# assessing the burden of correlated exposures"), see the vignette chapter
# "Two correlated exposures":
#
#   additive        PAF = PAF_1 + PAF_2                     (Steenland & Armstrong, 2006)
#   multiplicative  PAF = (RR_1 * RR_2 - 1) / (RR_1 * RR_2) (Jerrett et al., 2013)
#   combined        PAF = 1 - (1 - PAF_1) * (1 - PAF_2)     (Steenland & Armstrong, 2006)
#
# The case study is the one of the BEST-COST task 1.4 report (RIVM) on
# natural-cause mortality attributable to PM2.5 and NO2: annual means of
# 8.1 ug/m3 PM2.5 and 10.9 ug/m3 NO2 with log-linear exposure-response
# functions of RR = 1.063 and RR = 1.031 per 10 ug/m3, each adjusted for the
# other pollutant. The report gives an attributable fraction of 8.1% with the
# additive and of 7.9% with the multiplicative approach.
#
# ATTENTION: neither report is publicly available. The D1.2 report is
# confidential and will not be published; the task 1.4 report is not published
# yet. The figures used here are therefore not verifiable from outside the
# project, but they do come from a real assessment and not from a previous run
# of healthiar.
#
# The baseline health data of 1,000 cases is a round number, so that the
# attributable cases are the attributable fraction expressed in per mille.
bhd <- 1000

# The relative risk of one exposure at its own exposure level, i.e. the
# log-linear rescaling from the increment of the epidemiological study
rr_at_exp_log_linear <- function(rr, exp) rr ^ (exp / 10)

# The equations of the D1.2 report, written out in base R so that the expected
# values do not come from healthiar itself. The whole population is exposed to
# one single level, so the attributable fraction of one exposure is (RR - 1) / RR
paf_multiexposure <- function(approach, rr_pm, rr_no2, exp_pm, exp_no2){

  rr <- c(rr_at_exp_log_linear(rr_pm, exp_pm), rr_at_exp_log_linear(rr_no2, exp_no2))
  paf <- (rr - 1) / rr

  base::switch(
    approach,
    additive = base::sum(paf),
    multiplicative = (base::prod(rr) - 1) / base::prod(rr),
    combined = 1 - base::prod(1 - paf))
}

# The central estimates of the case study, entered once here and reused by
# every test below
exp_pm <- c(central = 8.1)
exp_no2 <- c(central = 10.9)
rr_pm <- c(central = 1.063)
rr_no2 <- c(central = 1.031)

attribute_two_exposures <- function(exp_pm, exp_no2, rr_pm, rr_no2){

  # NULL for a bound that is not entered, i.e. for an assessment without
  # uncertainty around the exposure or the exposure-response function
  bound <- function(x, ci) if(ci %in% base::names(x)) x[[ci]] else NULL

  output_pm <-
    healthiar::attribute_health(
      exp_central = exp_pm[["central"]],
      exp_lower = bound(exp_pm, "lower"),
      exp_upper = bound(exp_pm, "upper"),
      cutoff_central = 0,
      bhd_central = bhd,
      rr_central = rr_pm[["central"]],
      rr_lower = bound(rr_pm, "lower"),
      rr_upper = bound(rr_pm, "upper"),
      rr_increment = 10,
      erf_shape = "log_linear")

  base::list(
    pm = output_pm,
    no2 =
      healthiar::attribute_mod(
        output_attribute = output_pm,
        exp_central = exp_no2[["central"]],
        exp_lower = bound(exp_no2, "lower"),
        exp_upper = bound(exp_no2, "upper"),
        rr_central = rr_no2[["central"]],
        rr_lower = bound(rr_no2, "lower"),
        rr_upper = bound(rr_no2, "upper")))
}

impact_multiexpose <- function(output, approach, results = "health_main"){

  results_multiexpose <-
    healthiar::multiexpose(
      output_attribute_exp_1 = output$pm,
      output_attribute_exp_2 = output$no2,
      exp_name_1 = "pm2.5",
      exp_name_2 = "no2",
      approach_multiexposure = approach)

  if(base::identical(results, "health_main")){
    results_multiexpose$health_main$impact
  } else {
    results_multiexpose$health_detailed$results_raw$impact
  }
}


## ADDITIVE APPROACH ###########################################################

testthat::test_that("results correct |pathway_multiexposure|approach_multiexposure_additive|bestcost|", {

  # 8.1%, i.e. the attributable fraction published in the BEST-COST task 1.4
  # report for the additive approach
  testthat::expect_equal(
    object =
      base::round(
        paf_multiexposure("additive", rr_pm, rr_no2, exp_pm, exp_no2),
        digits = 3),
    expected = 0.081)

  testthat::expect_equal(
    object =
      impact_multiexpose(
        attribute_two_exposures(exp_pm, exp_no2, rr_pm, rr_no2),
        approach = "additive"),
    expected =
      paf_multiexposure("additive", rr_pm, rr_no2, exp_pm, exp_no2) * bhd)
})


## MULTIPLICATIVE APPROACH #####################################################

testthat::test_that("results correct |pathway_multiexposure|approach_multiexposure_multiplicative|bestcost|", {

  # 7.9%, i.e. the attributable fraction published in the BEST-COST task 1.4
  # report for the multiplicative approach
  testthat::expect_equal(
    object =
      base::round(
        paf_multiexposure("multiplicative", rr_pm, rr_no2, exp_pm, exp_no2),
        digits = 3),
    expected = 0.079)

  testthat::expect_equal(
    object =
      impact_multiexpose(
        attribute_two_exposures(exp_pm, exp_no2, rr_pm, rr_no2),
        approach = "multiplicative"),
    expected =
      paf_multiexposure("multiplicative", rr_pm, rr_no2, exp_pm, exp_no2) * bhd)
})


## COMBINED APPROACH ###########################################################

testthat::test_that("results correct |pathway_multiexposure|approach_multiexposure_combined|bestcost|", {

  # With one single exposure category the combined equation reduces to the
  # multiplicative one, because 1 - (1 / RR_1) * (1 / RR_2) equals
  # (RR_1 * RR_2 - 1) / (RR_1 * RR_2)
  testthat::expect_equal(
    object = paf_multiexposure("combined", rr_pm, rr_no2, exp_pm, exp_no2),
    expected =
      paf_multiexposure("multiplicative", rr_pm, rr_no2, exp_pm, exp_no2))

  testthat::expect_equal(
    object =
      impact_multiexpose(
        attribute_two_exposures(exp_pm, exp_no2, rr_pm, rr_no2),
        approach = "combined"),
    expected =
      paf_multiexposure("combined", rr_pm, rr_no2, exp_pm, exp_no2) * bhd)
})


## UNCERTAINTY #################################################################

# The BEST-COST task 1.4 report does not give confidence intervals together
# with the central estimates above, so the bounds below are only chosen wide
# enough to keep the lower, central and upper combinations apart. What the
# tests validate is not their value but that the equations of the D1.2 report
# are applied to every combination of the bounds
exp_pm_ci <- c(central = 8.1, lower = 7, upper = 9)
exp_no2_ci <- c(central = 10.9, lower = 9, upper = 12)
rr_pm_ci <- c(central = 1.063, lower = 1.05, upper = 1.07)
rr_no2_ci <- c(central = 1.031, lower = 1.02, upper = 1.04)
ci <- c("central", "lower", "upper")

testthat::test_that("results correct |pathway_multiexposure|approach_multiexposure_additive|uncertainty|", {

  output <-
    attribute_two_exposures(exp_pm_ci, exp_no2_ci, rr_pm_ci, rr_no2_ci)

  # health_main holds the three exposure-response function bounds at the
  # central exposure
  testthat::expect_equal(
    object = impact_multiexpose(output, approach = "additive"),
    expected =
      purrr::map_dbl(
        ci,
        \(erf_ci)
        paf_multiexposure(
          approach = "additive",
          rr_pm = rr_pm_ci[[erf_ci]],
          rr_no2 = rr_no2_ci[[erf_ci]],
          exp_pm = exp_pm_ci[["central"]],
          exp_no2 = exp_no2_ci[["central"]]) * bhd))

  # The additive approach adds the two assessments up only at the very end, so
  # the raw results keep one row per exposure: first the nine combinations of
  # exposure and exposure-response function bounds of PM2.5, then those of NO2
  paf_one_exposure <- function(rr_ci, exp_ci){
    purrr::map(
      ci,
      \(exp_bound)
      purrr::map_dbl(
        ci,
        \(erf_bound){
          rr <- rr_at_exp_log_linear(rr_ci[[erf_bound]], exp_ci[[exp_bound]])
          (rr - 1) / rr * bhd})) |>
      base::unlist()
  }

  testthat::expect_equal(
    object = impact_multiexpose(output, "additive", results = "results_raw"),
    expected =
      c(paf_one_exposure(rr_pm_ci, exp_pm_ci),
        paf_one_exposure(rr_no2_ci, exp_no2_ci)))
})

testthat::test_that("results correct |pathway_multiexposure|approach_multiexposure_multiplicative|uncertainty|", {

  testthat::expect_equal(
    object =
      impact_multiexpose(
        attribute_two_exposures(exp_pm_ci, exp_no2_ci, rr_pm_ci, rr_no2_ci),
        approach = "multiplicative"),
    expected =
      purrr::map_dbl(
        ci,
        \(erf_ci)
        paf_multiexposure(
          approach = "multiplicative",
          rr_pm = rr_pm_ci[[erf_ci]],
          rr_no2 = rr_no2_ci[[erf_ci]],
          exp_pm = exp_pm_ci[["central"]],
          exp_no2 = exp_no2_ci[["central"]]) * bhd))
})

testthat::test_that("results correct |pathway_multiexposure|approach_multiexposure_combined|uncertainty|", {

  # The combined approach merges the two exposures, so its raw results hold
  # the nine combinations of exposure and exposure-response function bounds
  testthat::expect_equal(
    object =
      impact_multiexpose(
        attribute_two_exposures(exp_pm_ci, exp_no2_ci, rr_pm_ci, rr_no2_ci),
        approach = "combined",
        results = "results_raw"),
    expected =
      purrr::map(
        ci,
        \(exp_ci)
        purrr::map_dbl(
          ci,
          \(erf_ci)
          paf_multiexposure(
            approach = "combined",
            rr_pm = rr_pm_ci[[erf_ci]],
            rr_no2 = rr_no2_ci[[erf_ci]],
            exp_pm = exp_pm_ci[[exp_ci]],
            exp_no2 = exp_no2_ci[[exp_ci]]) * bhd)) |>
      base::unlist())
})


# ERROR OR WARNING ########
## ERROR #########

testthat::test_that("error if the two assessments assume a different life table approach", {

  # The life table projects one cohort, so it needs one single value of
  # approach_exposure. Two assessments with different values used to reach
  # get_impact_with_lifetable() and abort there with
  # "the condition has length > 1"
  data <- base::readRDS(testthat::test_path("testdata", "lifetable_male_ekv_2010.rds"))

  attribute_with_approach_exposure <- function(approach_exposure){
    healthiar::attribute_lifetable(
      health_outcome = "yll",
      exp_central = 10,
      cutoff_central = 0,
      rr_central = 1.045,
      rr_increment = 10,
      erf_shape = "log_linear",
      age_group = data$age,
      sex = base::rep("male", 106),
      population = data$population_male,
      bhd_central = base::as.numeric(data$deaths_natural_male),
      year_of_analysis = 2010,
      min_age = 20,
      approach_exposure = approach_exposure)
  }

  testthat::expect_error(
    object =
      healthiar::multiexpose(
        output_attribute_exp_1 = attribute_with_approach_exposure("single_year"),
        output_attribute_exp_2 = attribute_with_approach_exposure("constant"),
        exp_name_1 = "pm2.5",
        exp_name_2 = "no2",
        approach_multiexposure = "additive"),
    regexp = "needs one single value of approach_exposure")
})

testthat::test_that("error if exposure distribution and approach_multiexposure is not additive", {

  # The exposure categories of two exposures are not paired: category 1 of
  # pm2.5 has nothing to do with category 1 of no2. Before, the multiplicative
  # approach silently multiplied the relative risks of every category of both
  # exposures together
  attribute_one_exposure <- function(exp_central, rr_central){
    healthiar::attribute_health(
      exp_central = exp_central,
      prop_pop_exp = c(0.5, 0.5),
      cutoff_central = 0,
      rr_central = rr_central,
      rr_increment = 10,
      erf_shape = "log_linear",
      bhd_central = 1000)
  }

  testthat::expect_error(
    object =
      healthiar::multiexpose(
        output_attribute_exp_1 = attribute_one_exposure(c(10, 20), 1.10),
        output_attribute_exp_2 = attribute_one_exposure(c(20, 40), 1.05),
        exp_name_1 = "pm2.5",
        exp_name_2 = "no2",
        approach_multiexposure = "multiplicative"),
    regexp = "cannot merge exposure distributions")
})

## WARNING #########


## INFO PER EXPOSURE ###########################################################

testthat::test_that("results the same |multiexpose|info_per_exposure|", {

  # Each exposure can come from an attribute_health() call with its own info,
  # e.g. the name of the pollutant. That info identifies the exposures that are
  # being merged, just like exp_name, so it must not keep them apart:
  # otherwise the relative risks are not multiplied and the impacts are counted
  # once per exposure instead of once in total
  attribute_one_exposure <- function(info, exp_central, rr_central){
    healthiar::attribute_health(
      exp_central = exp_central,
      cutoff_central = 0,
      rr_central = rr_central,
      rr_increment = 10,
      erf_shape = "log_linear",
      bhd_central = 1000,
      info = info)
  }

  output_multiexpose <-
    healthiar::multiexpose(
      output_attribute_exp_1 = attribute_one_exposure("pm2.5", 10, 1.10),
      output_attribute_exp_2 = attribute_one_exposure("no2", 20, 1.05),
      exp_name_1 = "pm2.5",
      exp_name_2 = "no2",
      approach_multiexposure = "multiplicative")

  testthat::expect_equal(
    object = base::unique(output_multiexpose$health_detailed$results_raw$rr_at_exp),
    # 1.10 * 1.05^(20/10), i.e. the two relative risks multiplied
    expected = 1.10 * 1.05^2)
})


## SEVERAL GEO UNITS ###########################################################

testthat::test_that("results the same |multiexpose|approach_multiexposure_multiplicative|several_geo_units|", {

  # The relative risks must be multiplied across the exposures within one geo
  # unit, never across geo units (or sexes, age groups and info subgroups).
  # Grouping only by the _ci columns multiplied every row of the assessment
  # together, which gave both geo units the same (too high) relative risk
  attribute_one_exposure <- function(exp_central, rr_central){
    healthiar::attribute_health(
      exp_central = exp_central,
      geo_id_micro = c("a", "b"),
      cutoff_central = 0,
      rr_central = rr_central,
      rr_increment = 10,
      erf_shape = "log_linear",
      bhd_central = c(1000, 1000))
  }

  output_multiexpose <-
    healthiar::multiexpose(
      output_attribute_exp_1 = attribute_one_exposure(c(10, 20), 1.10),
      output_attribute_exp_2 = attribute_one_exposure(c(20, 40), 1.05),
      exp_name_1 = "pm2.5",
      exp_name_2 = "no2",
      approach_multiexposure = "multiplicative")

  testthat::expect_equal(
    object =
      output_multiexpose$health_main |>
      dplyr::arrange(geo_id_micro) |>
      dplyr::pull(rr_at_exp),
    # geo unit a: 1.10^(10/10) * 1.05^(20/10); geo unit b: 1.10^(20/10) * 1.05^(40/10)
    expected = c(1.10 * 1.05^2, 1.10^2 * 1.05^4))
})
