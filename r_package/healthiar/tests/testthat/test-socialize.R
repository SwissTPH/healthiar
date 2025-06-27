# QUANTITATIVE TEST ############################################################
testthat::test_that("results the same |fake_socialize|input_is_attribute_output_TRUE|social_indicator_TRUE|ref_pop_TRUE|", {

  data <- base::readRDS(testthat::test_path("data", "social_data.rds"))

  att_age_below_40 <-
    healthiar::attribute_health(
      exp_central = data$PM25_MEAN,
      cutoff_central = 0,
      rr_central = 1.08, # The data set contains the RR for the exposure but not per increment. Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
      erf_shape = "log_linear",
      rr_increment = 10,
      bhd_central = data$MORTALITY_TOTAL,
      population = data$POPULATION,
      geo_id_disaggregated = data$CS01012020)

  att_age_above_40 <-
    healthiar::attribute_health(
      exp_central = data$PM25_MEAN-0.1,
      cutoff_central = 0,
      rr_central = 1.08, # The data set contains the RR for the exposure but not per increment. Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
      erf_shape = "log_linear",
      rr_increment = 10,
      bhd_central = ifelse(data$MORTALITY_TOTAL-10<0, 0, data$MORTALITY_TOTAL-10),
      population = ifelse(data$POPULATION-10<0, 0, data$POPULATION-10),
      geo_id_disaggregated = data$CS01012020)


  testthat::expect_equal(
    object =
      healthiar::socialize(
        age_group = c("below_40", "above_40"),
        ref_prop_pop = c(0.5, 0.5),
        listed_output_attribute = list(att_age_below_40, att_age_above_40),
        geo_id_disaggregated = data$CS01012020,
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
        geo_id_disaggregated = data$SECTOR,
        social_quantile = data$MDI,
        increasing_deprivation = TRUE,
        age_group = data$AGE,
        population = data$POP,
        ref_prop_pop = data$REF
        )$social_main$difference_value[1:4] |> base::round(3),
    expect = round(c(42.4484118, 0.7791663, 23.92910057, 0.30518553), 3)
  )
})

#TODO
## Adjust this code to the new socialize structure
### Currently gives wrong result, but no error
# testthat::test_that("results correct |pathway_socialize|input_is_attribute_output_FALSE|social_indicator_TRUE|ref_pop_TRUE|", {
#
#   ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
#   data <- base::readRDS(testthat::test_path("data", "mort_pm25_sect_2021_bimd.rds"))
#
#   testthat::expect_equal(
#     ## healthiar FUNCTION CALL
#     object =
#       healthiar::socialize(
#         impact = data$VALUE,
#         geo_id_disaggregated = data$CS01012020, # geo IDs of the preparatory iteration call above and this function call must match!
#         social_indicator = data$score,
#         n_quantile = 10, # Specify number of quantiles, e.g. 10
#         # approach = "quantile", # default (and currently only) approach,
#         population = data$POPULATION,
#         age_group = c("total"),
#         ref_prop_pop = c(1)
#       ) |>
#       purrr::pluck("social_main") |>
#       dplyr::filter(
#         difference_type == "relative" &
#           difference_compared_with == "overall")  |>
#       dplyr::select(difference_value) |>
#       base::unlist() |>
#       base::as.numeric(),
#
#     ## RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
#     # expected = 1.34031 ## absolute + bottom_quantile
#     # expected = 0.01424282 ## relative + bottom_quantile
#     # expected = 10.26268 ## absolute + overall
#     expected = 0.0983327 ## relative + overall
#   )
#
#   ## ASSESSOR: Arno Pauwels, SCI
#   ## ASSESSMENT DETAILS: All-cause mortality attributable to PM2.5, by census tract (iteration)
#   ## INPUT DATA DETAILS: Modelled exposure, real mortality data from Belgium, 2021 + BIMD2011
#
# })


#TODO
## Adjust this code to the new socialize structure
### Right now gives error
# testthat::test_that("results correct |pathway_socialize|input_is_attribute_output_TRUE|social_indicator_TRUE|ref_pop_TRUE|", {
#
#   ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
#   data <- base::readRDS(testthat::test_path("data", "mort_pm25_sect_2021_bimd.rds"))
#
#   result_interim <- healthiar::attribute_health(
#     approach_risk = "relative_risk",
#     erf_shape = "log_linear",
#     rr_central = 1.118,
#     # rr_lower = NULL,
#     # rr_upper = NULL,
#     rr_increment = 10.0,
#     exp_central = as.list(data$PM25),
#     # exp_lower = NULL,
#     # exp_upper = NULL,
#     # cutoff_central = NULL,
#     # cutoff_lower = NULL,
#     # cutoff_upper = NULL,
#     bhd_central = as.list(data$VALUE_BASELINE),
#     # bhd_lower = NULL,
#     # bhd_upper = NULL,
#     geo_id_disaggregated = data$CS01012020,
#     # geo_id_aggregated = NULL,
#     population = data$POPULATION
#     # approach_multiexposure = NULL
#   )
#
#   testthat::expect_equal(
#     ## healthiar FUNCTION CALL
#     object =
#       healthiar::socialize(
#         listed_output_attribute = result_interim,
#         geo_id_disaggregated = data$CS01012020, # geo IDs of the preparatory iteration call above and this function call must match!
#         social_indicator = data$score,
#         n_quantile = 10, # Specify number of quantiles, e.g. 10
#         population = data$POPULATION,
#         age_group = c("total"),
#         ref_prop_pop = c(1)
#       ) |>
#       purrr::pluck("social_main") |>
#       dplyr::filter(
#         difference_type == "relative" &
#           difference_compared_with == "overall")  |>
#       dplyr::select(difference_value) |>
#       base::unlist() |>
#       base::as.numeric(),
#
#     ## RESULT(S) FROM THE COMPARISON ASSESSMENT YOU SELECTED
#     # expected = 1.34031 ## absolute + bottom_quantile
#     # expected = 0.01424282 ## relative + bottom_quantile
#     # expected = 10.26268 ## absolute + overall
#     expected = 0.0983327 ## relative + overall
#   )
# })

## ASSESSOR: Arno Pauwels, SCI
## ASSESSMENT DETAILS: All-cause mortality attributable to PM2.5, by census tract (iteration)
## INPUT DATA DETAILS: Modelled exposure, real mortality data from Belgium, 2021 + BIMD2011



# testthat::test_that("results correct", {
#
#   data <- base::readRDS(testthat::test_path("data", "social_data.rds"))
#
#   bestcost_pm_death <-
#     healthiar::attribute_health(
#       exp_central = as.list(data$PM25_MEAN),
#       cutoff_central = 0,
#       rr_central = 1.08, # The data set contains the RR for the exposure but not per increment. Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
#       erf_shape = "log_linear",
#       rr_increment = 10,
#       bhd_central = as.list(data$MORTALITY_TOTAL),
#       population = data$POPULATION,
#       geo_id_disaggregated = data$CS01012020)
#
#   testthat::expect_equal(
#     object =
#       healthiar::socialize(
#         output_attribute = bestcost_pm_death,
#         geo_id_disaggregated = data$CS01012020,
#         social_indicator = data$score,
#         n_quantile = 10,
#         approach = "quantile"
#       )$social_main$difference_value,
#     expect = c(22.52416423, 0.32236823, 14.5680866,0.17252793) # Results on 21 Nov 2024
#   )
# })
#
# testthat::test_that("results the same twice a socialize call", {
#
#   data <- base::readRDS(testthat::test_path("data", "social_data.rds"))
#
#   bestcost_pm_death <-
#     healthiar::attribute_health(
#       exp_central = as.list(data$PM25_MEAN),
#       cutoff_central = 0,
#       rr_central = 1.08, # The data set contains the RR for the exposure but not per increment. Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
#       erf_shape = "log_linear",
#       rr_increment = 10,
#       bhd_central = as.list(data$MORTALITY_TOTAL),
#       population = data$POPULATION,
#       geo_id_disaggregated = data$CS01012020)
#
#   testthat::expect_equal(
#     object =
#       healthiar::socialize(
#         output = bestcost_pm_death,
#         geo_id_disaggregated = data$CS01012020,
#         social_indicator = data$score,
#         n_quantile = 10,
#         approach = "quantile"
#         )$social_main$difference_value,
#     expect = healthiar::socialize(
#       impact = bestcost_pm_death[["health_main"]]$impact,
#       population = bestcost_pm_death[["health_main"]]$population,
#       bhd = bestcost_pm_death[["health_main"]]$bhd,
#       exp = bestcost_pm_death[["health_main"]]$exp,
#       pop_fraction = bestcost_pm_death[["health_main"]]$pop_fraction,
#       geo_id_disaggregated = data$CS01012020,
#       social_indicator = data$score,
#       n_quantile = 10,
#       approach = "quantile")$social_main$difference_value
#   )
# })

## tests with age groups
testthat::test_that("results correct |pathway_socialize|input_is_attribute_output_TRUE|social_indicator_TRUE|ref_pop_TRUE|", {

  ## IF APPLICABLE: LOAD INPUT DATA BEFORE RUNNING THE FUNCTION
  data <- base::readRDS(testthat::test_path("data", "no2_bimd_age.rds"))

  attribute_result_age <- base::by(
    data,
    data$AGE,
    function(data) {
      healthiar::attribute_health(
        approach_risk = 'relative_risk',
        exp_central = data$EXPOSURE,
        rr_central = 1.045,
        rr_increment = 10,
        cutoff_central = 0,
        erf_shape = 'log_linear',
        bhd_central = data$MORT,
        population = data$POP,
        geo_id_disaggregated = data$SECTOR
      )
    }
  )

  testthat::expect_equal(
    ## healthiar FUNCTION CALL
    object =
      healthiar::socialize(
        listed_output_attribute = attribute_result_age,
        # geo_id_disaggregated = data$CS01012020, # geo IDs of the preparatory iteration call above and this function call must match!
        social_indicator = base::subset(data, AGE == '[0,5)')$SCORE,
        n_quantile = 10, # Specify number of quantiles, e.g. 10
        # approach = "quantile", # default (and currently only) approach,
        # population = data$POPULATION,
        age_group = base::names(attribute_result_age),
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
    expected = c(43.3956942, 0.7786042, 24.4845985, 0.3052187)
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
        geo_id_disaggregated = data$SECTOR, # geo IDs of the preparatory iteration call above and this function call must match!
        social_indicator = data$SCORE,
        n_quantile = 10, # Specify number of quantiles, e.g. 10
        # approach = "quantile", # default (and currently only) approach,
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
        geo_id_disaggregated = data$SECTOR, # geo IDs of the preparatory iteration call above and this function call must match!
        # social_indicator = data$SCORE,
        social_quantile = base::as.numeric(base::gsub("D", "", data$DECILE)),
        # n_quantile = 10, # Specify number of quantiles, e.g. 10
        # approach = "quantile", # default (and currently only) approach,
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

## WARNING #########
