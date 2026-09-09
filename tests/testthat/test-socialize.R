# QUANTITATIVE TEST ############################################################

## WITH ATTRIBUTE_HEALTH() ###############################################
testthat::test_that("results the same |fake_socialize|input_is_attribute_output_TRUE|social_indicator_TRUE|ref_pop_TRUE|", {

  att_age <-
    healthiar::attribute_health(
      age_group = exdat_socialize$age_group,
      exp_central = exdat_socialize$pm25_mean,
      cutoff_central = 0,
      rr_central = exdat_socialize$rr,
      erf_shape = "log_linear",
      rr_increment = 10,
      bhd_central = exdat_socialize$mortality,
      population = exdat_socialize$population,
      geo_id_micro = exdat_socialize$geo_unit)

  testthat::expect_equal(
    object =
      healthiar::socialize(
        output_attribute = att_age,
        age_group = exdat_socialize$age_group, # They have to be the same in socialize() and in attribute_health()
        ref_prop_pop = exdat_socialize$ref_prop_pop,
        geo_id_micro = exdat_socialize$geo_unit,
        social_indicator = exdat_socialize$score,
        n_quantile = 10,
        increasing_deprivation = TRUE)$social_main$difference_value |> base::round(2),
    expect = c(11.470, 0.190, -0.830, -0.010) # Results on 25 June 2025
  )
})


testthat::test_that("results the same |fake_socialize|input_is_attribute_output_TRUE|social_indicator_TRUE|ref_pop_TRUE|", {

  data <- base::readRDS(testthat::test_path("testdata", "noise_road_lden_stavanger_bergen.rds"))
  data_groups <- dplyr::bind_rows(data, data) |>
    dplyr::mutate(age_group = rep(c("below_40", "above_40"), each = 85))

  att_age <-
    healthiar::attribute_health(
      age_group = data_groups$age_group,
      approach_risk = "absolute_risk",
      exp_central = data_groups$average_cat,
      population = data_groups$totpop,
      pop_exp = data_groups$ANTALL_PER,
      geo_id_micro = data_groups$GEO_ID,
      erf_eq_central = "78.9270-3.1162*c+0.0342*c^2")

  testthat::expect_equal(
    object =
      healthiar::socialize(
        age_group = c("below_40", "above_40"), # They have to be the same in socialize() and in attribute_health()
        ref_prop_pop = c(0.5, 0.5),
        output_attribute = att_age,
        geo_id_micro = unique(data_groups$GEO_ID),
        social_indicator = c(3.5, 7.0),
        n_quantile = 10,
        increasing_deprivation = TRUE)$social_main$difference_value |> base::round(2),
    expect = c(37.240, 0.280, 19.470, 0.130) # Results on 25 June 2025
  )
})

testthat::test_that("results correct |pathway_socialize|input_is_attribute_output_TRUE|social_indicator_TRUE|ref_pop_TRUE|", {

  pop_ref <- base::readRDS(testthat::test_path("testdata", "pop_ref.rds"))
  no2_mrt_mdi <- base::readRDS(testthat::test_path("testdata", "no2_mrt_mdi.rds"))

  data <- dplyr::left_join(
    no2_mrt_mdi,
    pop_ref,
    by = "AGE")


  testthat::expect_equal(
    object =
      healthiar::socialize(
        impact = data$ATT_MORT,
        geo_id_micro = data$SECTOR,
        social_quantile = data$MDI,
        increasing_deprivation = TRUE,
        age_group = data$AGE,
        population = data$POP,
        ref_prop_pop = data$REF
        )$social_main$difference_value[1:4] |> base::round(3),
    expect = base::round(c(42.4484118, 0.7791663, 23.92910057, 0.30518553), 3)
  )
})

## tests with age groups
testthat::test_that("results correct |pathway_socialize|input_is_attribute_output_TRUE|social_indicator_TRUE|ref_pop_TRUE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("testdata", "no2_bimd_age.rds"))

  attribute_result_age <-
    healthiar::attribute_health(
      approach_risk = 'relative_risk',
      age_group = data$AGE,
      exp_central = data$EXPOSURE,
      rr_central = 1.045,
      rr_increment = 10,
      cutoff_central = 0,
      erf_shape = 'log_linear',
      bhd_central = data$MORT,
      population = data$POP,
      geo_id_micro = data$SECTOR
      )

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::socialize(
        output_attribute = attribute_result_age,
        age_group = base::unique(data$AGE),
        geo_id_micro = base::unique(data$SECTOR), # geo IDs of the preparatory iteration call above and this function call must match!
        social_indicator = base::unique(data$SCORE),
        n_quantile = 10, # Specify number of quantiles, e.g. 10
        # population = data$POPULATION,
        ref_prop_pop = base::subset(data, SECTOR == '21001A00-')$REF
      ) |>
      purrr::pluck("social_main") |>
      # dplyr::filter(
      #   difference_type == "absolute" &
      #     difference_compared_with == "bottom_quantile")  |>
      dplyr::select(difference_value) |>
      base::unlist() |>
      base::as.numeric(),

    ## RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected = c(43.3985958, 0.7783631, 24.469600, 0.305009)
  )

  ## ASSESSOR: Arno Pauwels, SCI
  ## ASSESSMENT DETAILS: All-cause mortality attributable to NO2, by census tract (iteration)
  ## INPUT DATA DETAILS: Modelled exposure, real mortality data from Belgium, 2022 + BIMD2011

})

## WITH USER IMPACT ###############################################

#### WITH REF_PROP_POP #####################

testthat::test_that("results correct |pathway_socialize|input_is_attribute_output_FALSE|social_indicator_TRUE|ref_pop_TRUE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("testdata", "no2_bimd_age.rds"))

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::socialize(
        impact = data$IMPACT,
        geo_id_micro = data$SECTOR, # geo IDs of the preparatory iteration call above and this function call must match!
        social_indicator = data$SCORE,
        n_quantile = 10, # Specify number of quantiles, e.g. 10
        population = data$POP,
        age_group = data$AGE,
        ref_prop_pop = data$REF
      ) |>
      purrr::pluck("social_main") |>
      # dplyr::filter(
      #   difference_type == "absolute" &
      #     difference_compared_with == "bottom_quantile")  |>
      dplyr::select(difference_value) |>
      base::unlist() |>
      base::as.numeric(),

    ## RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected = c(43.3985958, 0.7783631, 24.469600, 0.305009)
  )

  ## ASSESSOR: Arno Pauwels, SCI
  ## ASSESSMENT DETAILS: All-cause mortality attributable to NO2, by census tract (iteration)
  ## INPUT DATA DETAILS: Modelled exposure, real mortality data from Belgium, 2022 + BIMD2011

})



testthat::test_that("results correct |pathway_socialize|input_is_attribute_output_FALSE|social_indicator_FALSE|ref_pop_TRUE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("testdata", "no2_bimd_age.rds"))

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::socialize(
        impact = data$IMPACT,
        geo_id_micro = data$SECTOR, # geo IDs of the preparatory iteration call above and this function call must match!
        # social_indicator = data$SCORE,
        social_quantile = base::as.numeric(base::gsub("D", "", data$DECILE)),
        # n_quantile = 10, # Specify number of quantiles, e.g. 10
        population = data$POP,
        age_group = data$AGE,
        ref_prop_pop = data$REF
      ) |>
      purrr::pluck("social_main") |>
      # dplyr::filter(
      #   difference_type == "absolute" &
      #     difference_compared_with == "bottom_quantile")  |>
      dplyr::select(difference_value) |>
      base::unlist() |>
      base::as.numeric(),

    ## RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
    expected = c(43.3985958, 0.7783631, 24.469600, 0.305009)
  )

  ## ASSESSOR: Arno Pauwels, SCI
  ## ASSESSMENT DETAILS: All-cause mortality attributable to NO2, by census tract (iteration)
  ## INPUT DATA DETAILS: Modelled exposure, real mortality data from Belgium, 2022 + BIMD2011


### WITHOUT REF_PROP_POP  ################################

  testthat::test_that("results correct |pathway_socialize|input_is_attribute_output_FALSE|social_indicator_TRUE|ref_pop_TRUE|", {

    ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
    data <- base::readRDS(testthat::test_path("testdata", "no2_bimd_age.rds"))

    testthat::expect_equal(
      ## healthiar FUNCTION CALL
      object =
        healthiar::socialize(
          impact = data$IMPACT,
          geo_id_micro = data$SECTOR, # geo IDs of the preparatory iteration call above and this function call must match!
          social_indicator = data$SCORE,
          n_quantile = 10, # Specify number of quantiles, e.g. 10
          population = data$POP,
          age_group = data$AGE,
          #ref_prop_pop = data$REF  # Deactivating ref_prop_pop
        ) |>
        purrr::pluck("social_main") |>
        dplyr::select(difference_value) |>
        base::unlist() |>
        base::as.numeric(),

      ## RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
      expected = c(31.472515930, 0.791263808, 17.647936608, 0.307332548)
    )


  })

})

# ERROR OR WARNING ########
## ERROR #########

testthat::test_that("error if non-numeric", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("testdata", "no2_bimd_age.rds"))

  testthat::expect_error(
    ## healthiar FUNCTION CALL
    object =
      healthiar::socialize(
        impact = base::as.character(data$IMPACT), #
        geo_id_micro = data$SECTOR,
        social_indicator = data$SCORE,
        n_quantile = 10,
        population = data$POP,
        age_group = data$AGE,
        ref_prop_pop = data$REF),
    regexp = "impact must contain numeric value(s).",
    fixed = TRUE
  )
})


testthat::test_that("error if non-numeric in numeric var", {

  data <- base::readRDS(testthat::test_path("testdata", "no2_bimd_age.rds"))

  testthat::expect_error(
    ## healthiar FUNCTION CALL
    object =
      healthiar::socialize(
        impact = base::as.character(data$IMPACT), # As character to force error
        geo_id_micro = data$SECTOR,
        social_indicator = data$SCORE,
        n_quantile = 10,
        population = data$POP,
        age_group = data$AGE,
        ref_prop_pop = data$REF),
    regexp = "impact must contain numeric value(s).",
    fixed = TRUE
  )
})

testthat::test_that("error if non-numeric in integer var", {

  data <- base::readRDS(testthat::test_path("testdata", "no2_bimd_age.rds"))

  testthat::expect_error(
    ## healthiar FUNCTION CALL
    object =
      healthiar::socialize(
        impact = data$IMPACT,
        geo_id_micro = data$SECTOR,
        social_indicator = data$SCORE,
        n_quantile = as.character(10), # As character to force error
        population = data$POP,
        age_group = data$AGE,
        ref_prop_pop = data$REF),
    regexp = "n_quantile must contain numeric value(s).",
    fixed = TRUE
  )

})

testthat::test_that("error if non-numeric in integer var", {

  data <- base::readRDS(testthat::test_path("testdata", "no2_bimd_age.rds"))

  testthat::expect_error(
    ## healthiar FUNCTION CALL
    object =
      healthiar::socialize(
        impact = data$IMPACT,
        geo_id_micro = data$SECTOR,
        social_indicator = data$SCORE,
        increasing_deprivation = 0.3, # Number instead of TRUE/FALSE to force error
        n_quantile = 10,
        population = data$POP,
        age_group = data$AGE,
        ref_prop_pop = data$REF),
    regexp = "increasing_deprivation must be TRUE or FALSE."
  )

})

testthat::test_that("error if not integer var", {

  data <- base::readRDS(testthat::test_path("testdata", "no2_bimd_age.rds"))

  testthat::expect_error(
    ## healthiar FUNCTION CALL
    object =
      healthiar::socialize(
        impact = data$IMPACT,
        geo_id_micro = data$SECTOR,
        social_indicator = data$SCORE,
        n_quantile = 10.5, # Decimal to force error
        population = data$POP,
        age_group = data$AGE,
        ref_prop_pop = data$REF),
    regexp = "n_quantile must contain whole numeric value(s).",
    fixed = TRUE
  )

})

testthat::test_that("error if age_group does not match in output_attribute", {

  att_age <-
    healthiar::attribute_health(
      age_group = exdat_socialize$age_group,
      exp_central = exdat_socialize$pm25_mean,
      cutoff_central = 0,
      rr_central = 1.08, # The data set contains the RR for the exposure but not per increment. Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
      erf_shape = "log_linear",
      rr_increment = 10,
      bhd_central = exdat_socialize$mortality,
      population = exdat_socialize$population,
      geo_id_micro = exdat_socialize$geo_unit)

  testthat::expect_error(
    object =
      healthiar::socialize(
        age_group = c("40_minus", "40_plus"), # Different age_group to force error
        ref_prop_pop = c(0.5, 0.5),
        output_attribute = att_age,
        social_indicator = exdat_socialize$score,
        n_quantile = 10,
        increasing_deprivation = TRUE),
    regexp =  "age_group must be identical to the values in the column age_group in output_attribute."
  )
})

testthat::test_that("error if not fraction", {

  data <- base::readRDS(testthat::test_path("testdata", "no2_bimd_age.rds"))
  data$REF[1] <- 1.2 # Value higher than 0 to force error

  testthat::expect_error(
    ## healthiar FUNCTION CALL
    object =
      healthiar::socialize(
        impact = data$IMPACT,
        geo_id_micro = data$SECTOR,
        social_indicator = data$SCORE,
        n_quantile = 10,
        population = data$POP,
        age_group = data$AGE,
        ref_prop_pop = data$REF),
    regexp = "ref_prop_pop must have values between 0 and 1."
  )

})

testthat::test_that("error if not fraction in the last position", {

  data <- base::readRDS(testthat::test_path("testdata", "no2_bimd_age.rds"))
  # Value higher than 1 in the LAST position to force error.
  # All values of the argument must be checked and not only the first one
  data$REF[base::length(data$REF)] <- 1.2

  testthat::expect_error(
    ## healthiar FUNCTION CALL
    object =
      healthiar::socialize(
        impact = data$IMPACT,
        geo_id_micro = data$SECTOR,
        social_indicator = data$SCORE,
        n_quantile = 10,
        population = data$POP,
        age_group = data$AGE,
        ref_prop_pop = data$REF),
    regexp = "ref_prop_pop must have values between 0 and 1."
  )

})

testthat::test_that("error if var lower than 0", {

  data <- base::readRDS(testthat::test_path("testdata", "no2_bimd_age.rds"))

  testthat::expect_error(
    ## healthiar FUNCTION CALL
    object =
      healthiar::socialize(
        impact = data$IMPACT,
        geo_id_micro = data$SECTOR,
        social_indicator = data$SCORE,
        n_quantile = -10, # Negative value to force error
        population = data$POP,
        age_group = data$AGE,
        ref_prop_pop = data$REF),
    regexp = "The value(s) of n_quantile cannot be lower than 0.",
    fixed = TRUE
  )

})

## WARNING #########

testthat::test_that("warning and no least deprived quantile if social_indicator is NA", {

  # A geographic unit without social_indicator gets no social_quantile.
  # arrange() puts NA last, so it used to be reported as the last (least
  # deprived) quantile. It must be excluded from the comparison between the
  # quantiles, but stay part of the overall values
  geo_id_micro <- base::paste0("u", 1:8)
  social_indicator <- c(10, 20, 30, 40, 50, 60, 70, NA)
  impact <- c(13, 11, 9, 7, 6, 5, 3, 50)
  population <- base::rep(1000, 8)

  call_socialize <- function(){
    healthiar::socialize(
      age_group = base::rep("all", 8),
      geo_id_micro = geo_id_micro,
      social_indicator = social_indicator,
      n_quantile = 4,
      increasing_deprivation = TRUE,
      population = population,
      impact = impact)
  }

  testthat::expect_warning(
    object = call_socialize(),
    regexp = "have no value in social_indicator")

  output_socialize <- base::suppressWarnings(call_socialize())

  # first: quantile 1 = units u7 and u6, i.e. (3 + 5) / 2000 * 1E5 = 400
  # last: quantile 4 = units u2 and u1, i.e. (11 + 13) / 2000 * 1E5 = 1200
  #   (and not 50 / 1000 * 1E5 = 5000, the unit without social_indicator)
  # overall: all eight units, i.e. 104 / 8000 * 1E5 = 1300
  impact_rate_std <-
    output_socialize$social_detailed$results_all_parameters |>
    dplyr::filter(parameter %in% "impact_rate_std") |>
    # One row per difference_type, but first, last and overall are the same in
    # all of them
    dplyr::distinct(first, last, overall)

  testthat::expect_equal(
    object = base::unlist(impact_rate_std),
    expected = c(first = 400, last = 1200, overall = 1300))
})




## NOT SUMMED #################################################################

testthat::test_that("error if main_results_by and socialize", {

  # The impacts are grouped only by geo unit, age group and social quantile,
  # and the subgroups cannot be entered in the arguments of socialize() either,
  # so subgroups kept apart with main_results_by cannot be analyzed together
  output_attribute <-
    healthiar::attribute_health(
      info = base::data.frame(pair = base::rep(c("copd", "asthma"), each = 4)),
      main_results_by = "pair",
      age_group = base::rep(c("below_40", "above_40"), times = 4),
      geo_id_micro = base::rep(base::rep(c("g1", "g2"), each = 2), times = 2),
      exp_central = base::rep(c(8.1, 22.1), each = 4),
      cutoff_central = 0,
      bhd_central = base::rep(c(1000, 4000), times = 4),
      rr_central = base::rep(c(1.063, 1.041), each = 4),
      rr_increment = 10,
      erf_shape = "log_linear",
      population = base::rep(c(1E5, 5E5), times = 4))

  testthat::expect_error(
    object =
      healthiar::socialize(
        output_attribute = output_attribute,
        age_group = c("below_40", "above_40"),
        ref_prop_pop = c(0.5, 0.5),
        geo_id_micro = c("g1", "g2"),
        social_indicator = c(1, 2),
        n_quantile = 2),
    regexp = "only be applied to one subgroup")
})
