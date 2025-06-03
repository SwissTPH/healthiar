# QUANTITATIVE TEST ############################################################
testthat::test_that("results the same", {

  data <- base::readRDS(testthat::test_path("data", "social_data.rds"))

  att_age_below_40 <-
    healthiar::attribute_health(
      exp_central = as.list(data$PM25_MEAN),
      cutoff_central = 0,
      rr_central = 1.08, # The data set contains the RR for the exposure but not per increment. Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
      erf_shape = "log_linear",
      rr_increment = 10,
      bhd_central = as.list(data$MORTALITY_TOTAL),
      population = data$POPULATION,
      geo_id_disaggregated = data$CS01012020)

  att_age_above_40 <-
    healthiar::attribute_health(
      exp_central = base::as.list(data$PM25_MEAN-0.1),
      cutoff_central = 0,
      rr_central = 1.08, # The data set contains the RR for the exposure but not per increment. Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
      erf_shape = "log_linear",
      rr_increment = 10,
      bhd_central = base::as.list(ifelse(data$MORTALITY_TOTAL-10<0, 0, data$MORTALITY_TOTAL-10)),
      population = base::as.list(ifelse(data$POPULATION-10<0, 0, data$POPULATION-10)),
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
    expect = c(11.72, 0.20, 560.88,0.90) # Results on 21 Nov 2024
  )
})

testthat::test_that("results correct", {

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
        age_group = data$AGE,
        population = data$POP,
        ref_prop_pop = data$REF
        )$social_main$difference_value[1:2] |> base::round(3),
    expect = round(c(42.4484118, 0.7791663), 3)
  )
})


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

# ERROR OR WARNING ########
## ERROR #########

## WARNING #########
