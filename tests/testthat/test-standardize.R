# QUANTITATIVE TEST ############################################################

## ONLY ATTRIBUTE ############################################################

### ONE GEO UNIT ############################################################



# Curtin & Klein (1995), "Direct Standardization (Age-Adjusted Death Rates)",
# Healthy People 2000 Statistical Notes No. 6 (revised), CDC/National Center
# for Health Statistics. Their tables A and B are a complete worked example of
# the direct method: two communities with the same total population (10,000)
# and the same three age groups, where community B has the higher age-specific
# death rate in EVERY age group but the lower crude rate, because community A
# is much older (60% aged 65+ against 10%).
#
#   Table A (crude rates)             Table B (age adjustment)
#   Age     Community A  Community B  Standard    A rate    B rate
#           deaths/pop   deaths/pop   population  per 1000  per 1000
#   0-34      20/ 1,000   180/ 6,000       3,000        20        30
#   35-64    120/ 3,000   150/ 3,000       3,000        40        50
#   65+      360/ 6,000    70/ 1,000       4,000        60        70
#   Total    500/10,000   400/10,000      10,000
#   Crude rate       50           40
#   Age-adjusted rate (table B)                          42        52
#
# The direct method standardizes RATES, so it is bhd_per_100k_inhab that has to
# reproduce the published age-adjusted death rate. Exposure and
# exposure-response function play no role in it and are entered only because
# attribute_health() requires them
standardize_curtin_klein <- function(population, deaths, ref_prop_pop = NULL){

  age_group <- c("0-34", "35-64", "65+")

  healthiar::attribute_health(
    age_group = age_group,
    exp_central = 10,
    cutoff_central = 0,
    rr_central = 1.05,
    rr_increment = 10,
    erf_shape = "log_linear",
    bhd_central = deaths,
    population = population) |>
    healthiar::standardize(
      age_group = age_group,
      ref_prop_pop = ref_prop_pop) |>
    # Per 100,000 inhabitants in healthiar, per 1,000 in the publication
    purrr::pluck("health_main", "bhd_per_100k_inhab") / 100
}

testthat::test_that("results correct |pathway_standardize|single_geo|curtin_klein_1995|", {

  #### WITH REF_PROP_POP ############################################################

  # Table B: age-adjusted death rates with the standard population
  # 3,000 / 3,000 / 4,000, i.e. the reference proportions 0.3 / 0.3 / 0.4
  testthat::expect_equal(
    object =
      c(community_a =
          standardize_curtin_klein(
            population = c(1000, 3000, 6000),
            deaths = c(20, 120, 360),
            ref_prop_pop = c(0.3, 0.3, 0.4)),
        community_b =
          standardize_curtin_klein(
            population = c(6000, 3000, 1000),
            deaths = c(180, 150, 70),
            ref_prop_pop = c(0.3, 0.3, 0.4))),
    expected = c(community_a = 42, community_b = 52))

  #### WITHOUT REF_PROP_POP ############################################################

  # Without ref_prop_pop the age distribution of the study population itself is
  # taken as reference, so the direct method gives back the crude rate of
  # table A. Note that it ranks the two communities the other way round
  testthat::expect_equal(
    object =
      c(community_a =
          standardize_curtin_klein(
            population = c(1000, 3000, 6000),
            deaths = c(20, 120, 360)),
        community_b =
          standardize_curtin_klein(
            population = c(6000, 3000, 1000),
            deaths = c(180, 150, 70))),
    expected = c(community_a = 50, community_b = 40))

})



### MULTIPLE GEO UNITS ############################################################


testthat::test_that("results correct |pathway_standardize|multi_geo|curtin_klein_1995|", {

  # The two communities of Curtin & Klein (1995) are two geographic units, so
  # the same published example must come out of one single call with
  # geo_id_micro (see the table in the section above)
  age_group <- c("0-34", "35-64", "65+")

  both_communities <-
    healthiar::attribute_health(
      geo_id_micro = rep(c("community_a", "community_b"), each = 3),
      age_group = rep(age_group, times = 2),
      exp_central = 10,
      cutoff_central = 0,
      rr_central = 1.05,
      rr_increment = 10,
      erf_shape = "log_linear",
      bhd_central = c(20, 120, 360, 180, 150, 70),
      population = c(1000, 3000, 6000, 6000, 3000, 1000))

  #### WITH REF_PROP_POP ############################################################

  testthat::expect_equal(
    object =
      healthiar::standardize(
        output_attribute = both_communities,
        age_group = age_group,
        ref_prop_pop = c(0.3, 0.3, 0.4))$health_main$bhd_per_100k_inhab / 100,
    expected = c(42, 52))

  #### WITHOUT REF_PROP_POP ############################################################

  # Without ref_prop_pop the reference is the age distribution of the whole
  # assessment, i.e. of the two communities pooled (7,000 / 6,000 / 7,000 or
  # 0.35 / 0.30 / 0.35), which is what Curtin & Klein call an internal
  # standard. Both geo units are therefore standardized to the same reference
  internal_standard <- c(0.35, 0.30, 0.35)

  testthat::expect_equal(
    object =
      healthiar::standardize(
        output_attribute = both_communities,
        age_group = age_group)$health_main$bhd_per_100k_inhab / 100,
    expected =
      c(sum(c(20, 40, 60) * internal_standard),
        sum(c(30, 50, 70) * internal_standard)))
})




## IN COMBINATION WITH COMPARE() ############################################################

testthat::test_that("results the same |pathway_standardize|single_geo|", {

  mortality_1 <-
    healthiar::attribute_health(
      age_group = c("below_40", "above_40"),
      exp_central = c(8.1, 10.9),
      cutoff_central =  0,
      bhd_central = c(1000, 4000),
      rr_central = 1.063,
      rr_increment = 10,
      erf_shape = "log_linear",
      population = c(1E5, 5E5))

  std_mortality_1 <-
    healthiar::standardize(
      output_attribute = mortality_1,
      age_group = c("below_40", "above_40"),
      ref_prop_pop = c(0.5, 0.5))

  mortality_2 <-
    healthiar::attribute_health(
      age_group = c("below_40", "above_40"),
      exp_central = c(6.1, 7.9),
      cutoff_central =  0,
      bhd_central = c(1000, 4000),
      rr_central = 1.063,
      rr_increment = 10,
      erf_shape = "log_linear",
      population = c(1E5, 5E5))

  std_mortality_2 <-
    healthiar::standardize(
      output_attribute = mortality_2,
      age_group = c("below_40", "above_40"),
      ref_prop_pop = c(0.5, 0.5))

  comparison <- healthiar::compare(std_mortality_1, std_mortality_2)

  testthat::expect_equal(
    object =
      comparison$health_main$impact,

    expected =  80.9237143
    # No study behind.
    # Fake numbers to check consistency of result overtime. Results on 2026-01-16
  )

})






## NOT SUMMED #################################################################

testthat::test_that("results the same |pathway_standardize|main_results_by|two_subgroups_in_one_call|", {

  # Two exposure-outcome pairs kept apart with main_results_by must give the same
  # age-standardized impacts as two separate assessments, i.e. standardize()
  # must be applied once per subgroup instead of pooling them
  in_one_call <-
    healthiar::attribute_health(
      info = data.frame(pair = rep(c("copd", "asthma"), each = 2)),
      main_results_by = "pair",
      age_group = rep(c("below_40", "above_40"), times = 2),
      exp_central = c(8.1, 10.9, 22.1, 24.5),
      cutoff_central = 0,
      bhd_central = c(1000, 4000, 800, 3000),
      rr_central = c(1.063, 1.063, 1.041, 1.041),
      rr_increment = 10,
      erf_shape = "log_linear",
      population = c(1E5, 5E5, 1E5, 5E5)) |>
    healthiar::standardize(
      age_group = c("below_40", "above_40"),
      ref_prop_pop = c(0.5, 0.5))

  in_separate_calls <-
    purrr::map_dbl(
      .x = 1:2,
      .f = ~ healthiar::attribute_health(
        age_group = c("below_40", "above_40"),
        exp_central = list(c(8.1, 10.9), c(22.1, 24.5))[[.x]],
        cutoff_central = 0,
        bhd_central = list(c(1000, 4000), c(800, 3000))[[.x]],
        rr_central = c(1.063, 1.041)[.x],
        rr_increment = 10,
        erf_shape = "log_linear",
        population = c(1E5, 5E5)) |>
        healthiar::standardize(
          age_group = c("below_40", "above_40"),
          ref_prop_pop = c(0.5, 0.5)) |>
        purrr::pluck("health_main", "impact_per_100k_inhab"))

  testthat::expect_equal(
    object = in_one_call$health_main$impact_per_100k_inhab,
    expected = in_separate_calls)
})

## EXPOSURE, ATTRIBUTABLE FRACTION AND AGE GROUP ORDER ########################

testthat::test_that("results the same |pathway_standardize|uncertainty|age_group_order|", {

  output_attribute <-
    healthiar::attribute_health(
      age_group = c("below_40", "above_40"),
      exp_central = c(8.1, 10.9),
      cutoff_central =  0,
      bhd_central = c(1000, 4000),
      rr_central = 1.063,
      rr_lower = 1.02, # Fake lower and upper bound to get three erf_ci rows
      rr_upper = 1.08,
      rr_increment = 10,
      erf_shape = "log_linear",
      population = c(1E5, 5E5))

  standardized <-
    healthiar::standardize(
      output_attribute = output_attribute,
      age_group = c("below_40", "above_40"))

  central <-
    standardized$health_main[standardized$health_main$erf_ci %in% "central", ]

  # Expected values derived from the arguments above and not from the output,
  # so that the test does not confirm whatever the function happens to return
  rr_at_exp <- exp(log(1.063) * c(8.1, 10.9) / 10)
  pop_fraction_by_age_group <- (rr_at_exp - 1) / rr_at_exp
  bhd <- c(1000, 4000)
  population <- c(1E5, 5E5)

  # exp is the population-weighted mean exposure (not divided once more by the
  # number of age groups) and pop_fraction the attributable cases divided by
  # the baseline cases (not the sum of the age group-specific fractions).
  # Neither may depend on the number of uncertainty combinations, i.e. on how
  # many _ci rows each age group appears in
  testthat::expect_equal(
    object =
      c(exp = central$exp,
        pop_fraction = central$pop_fraction),
    expected =
      c(exp = sum(c(8.1, 10.9) * population / sum(population)),
        pop_fraction = sum(pop_fraction_by_age_group * bhd) / sum(bhd)))

  # The reference proportions must stay attached to their own age group, so the
  # order in which the age groups are entered cannot change the result
  testthat::expect_equal(
    object =
      healthiar::standardize(
        output_attribute = output_attribute,
        age_group = c("above_40", "below_40"))$health_main$impact_per_100k_inhab,
    expected = standardized$health_main$impact_per_100k_inhab)
})



# ERROR OR WARNING ########
## ERROR #########