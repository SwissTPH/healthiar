# QUANTITATIVE TEST ############################################################
testthat::test_that("results the same |fake_socialize|input_is_attribute_output_TRUE|social_indicator_TRUE|ref_pop_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "social_data.rds"))

  att_age <-
    healthiar::attribute_health(
      age_group = rep(c("below_40", "above_40"), each = 9037),
      exp_central = c(data$PM25_MEAN, data$PM25_MEAN-0.1),
      cutoff_central = 0,
      rr_central = 1.08, # The data set contains the RR for the exposure but not per increment. Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
      erf_shape = "log_linear",
      rr_increment = 10,
      bhd_central = c(data$MORTALITY_TOTAL,
                      ifelse(data$MORTALITY_TOTAL-10<0, 0, data$MORTALITY_TOTAL-10)),
      population = c(data$POPULATION, ifelse(data$POPULATION-10<0, 0, data$POPULATION-10)),
      geo_id_micro = rep(data$CS01012020, 2))

  testthat::expect_equal(
    object =
      healthiar::socialize(
        age_group = c("below_40", "above_40"), # They have to be the same in socialize() and in attribute_health()
        ref_prop_pop = c(0.5, 0.5),
        output_attribute = att_age,
        geo_id_micro = data$CS01012020,
        social_indicator = data$score,
        n_quantile = 10,
        increasing_deprivation = TRUE)$social_main$difference_value |> base::round(2),
    expect = c(11.72, 0.20, -0.64, -0.01) # Results on 25 June 2025
  )
})

testthat::test_that("results correct |pathway_socialize|input_is_attribute_output_TRUE|social_indicator_TRUE|ref_pop_TRUE|", {

  pop_ref <- base::readRDS(testthat::test_path("data", "pop_ref.rds"))
  no2_mrt_mdi <- base::readRDS(testthat::test_path("data", "no2_mrt_mdi.rds"))

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
  data <- base::readRDS(testthat::test_path("data", "no2_bimd_age.rds"))

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
        # geo_id_micro = data$CS01012020, # geo IDs of the preparatory iteration call above and this function call must match!
        social_indicator = base::subset(data, AGE == '[0,5)')$SCORE,
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

testthat::test_that("results correct |pathway_socialize|input_is_attribute_output_FALSE|social_indicator_TRUE|ref_pop_TRUE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("data", "no2_bimd_age.rds"))

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
  data <- base::readRDS(testthat::test_path("data", "no2_bimd_age.rds"))

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

})

# ERROR OR WARNING ########
## ERROR #########

testthat::test_that("error if non-numeric", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("data", "no2_bimd_age.rds"))

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

  data <- base::readRDS(testthat::test_path("data", "no2_bimd_age.rds"))

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

  data <- base::readRDS(testthat::test_path("data", "no2_bimd_age.rds"))

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

  data <- base::readRDS(testthat::test_path("data", "no2_bimd_age.rds"))

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

  data <- base::readRDS(testthat::test_path("data", "no2_bimd_age.rds"))

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

  data <- base::readRDS(testthat::test_path("data", "social_data.rds"))

  att_age <-
    healthiar::attribute_health(
      age_group = rep(c("below_40", "above_40"), each = 9037),
      exp_central = c(data$PM25_MEAN, data$PM25_MEAN-0.1),
      cutoff_central = 0,
      rr_central = 1.08, # The data set contains the RR for the exposure but not per increment. Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
      erf_shape = "log_linear",
      rr_increment = 10,
      bhd_central = c(data$MORTALITY_TOTAL,
                      ifelse(data$MORTALITY_TOTAL-10<0, 0, data$MORTALITY_TOTAL-10)),
      population = c(data$POPULATION, ifelse(data$POPULATION-10<0, 0, data$POPULATION-10)),
      geo_id_micro = rep(data$CS01012020, 2))

  testthat::expect_error(
    object =
      healthiar::socialize(
        age_group = c("40_minus", "40_plus"), # Different age_group to force error
        ref_prop_pop = c(0.5, 0.5),
        output_attribute = att_age,
        geo_id_micro = data$CS01012020,
        social_indicator = data$score,
        n_quantile = 10,
        increasing_deprivation = TRUE),
    regexp =  "age_group must be identical to the values in the column age_group in output_attribute."
  )
})

testthat::test_that("error if not fraction", {

  data <- base::readRDS(testthat::test_path("data", "no2_bimd_age.rds"))
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

testthat::test_that("error if var lower than 0", {

  data <- base::readRDS(testthat::test_path("data", "no2_bimd_age.rds"))

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
