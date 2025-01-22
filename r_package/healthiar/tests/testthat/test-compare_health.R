testthat::test_that("results correct delta comparison rr single exposure", {

  testthat::expect_equal(
    object =
      healthiar::compare_health(
        approach_comparison = "delta",
        exp_central_1 = 8.85,
        exp_central_2 = 6,
        cutoff_central = 5,
        bhd_central_1 = 25000,
        bhd_central_2 = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        info_1 = "PM2.5_mortality_2010",
        info_2 = "PM2.5_mortality_2020") |>
      helper_extract_main_results(),
    expected =
      c(774, 409, 1127) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct delta comparison when two scenarios are identical", {

  testthat::expect_equal(
    object =
      healthiar::compare_health(
        approach_comparison = "delta",
        exp_central_1 = 8.85,
        exp_central_2 = 8.85,
        cutoff_central = 5,
        bhd_central_1 = 25000,
        bhd_central_2 = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        info_1 = "PM2.5_mortality_2010",
        info_2 = "PM2.5_mortality_2020") |>
      helper_extract_main_results(),
    expected =
      c(0, 0, 0)
  )
})

testthat::test_that("results correct delta comparison iteration rr single exposures", {

  testthat::expect_equal(
    object =
      healthiar::compare_health(
        approach_comparison = "delta",
        exp_central_1 = list(8.85, 8.0),
        exp_central_2 = list(6, 6.5),
        cutoff_central = 5,
        bhd_central_1 = list(25000, 20000),
        bhd_central_2 = list(25000, 20000),
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        info_1 = "PM2.5_mortality_2010",
        info_2 = "PM2.5_mortality_2020",
        geo_id_disaggregated = c("a", "b"),
        geo_id_aggregated = rep("ch", 2)) |>
      helper_extract_main_results(),
    expected =
      c(1100, 582, 1603) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct pif comparison rr single exposure", {

  testthat::expect_equal(
    object =
      healthiar::compare_health(
        approach_comparison = "pif",
        exp_central_1 = 8.85,
        exp_central_2 = 6,
        cutoff_central = 5,
        bhd_central_1 = 25000,
        bhd_central_2 = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        info_1 = "PM2.5_mortality_2010",
        info_2 = "PM2.5_mortality_2020") |>
      helper_extract_main_results(),
    expected =
      c(782, 412, 1146) # Results on 16 May 2024; no comparison study
  )
})

testthat::test_that("results correct pif comparison iteration rr single exposures", {

  testthat::expect_equal(
    object =
      healthiar::compare_health(
        approach_comparison = "pif",
        exp_central_1 = list(8.85, 8.0),
        exp_central_2 = list(6, 6.5),
        cutoff_central = 5,
        bhd_central_1 = list(25000, 20000),
        bhd_central_2 = list(25000, 20000),
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        info_1 = "PM2.5_mortality_2010",
        info_2 = "PM2.5_mortality_2020",
        geo_id_disaggregated = c("a", "b"),
        geo_id_aggregated = rep("ch", 2)) |>
      helper_extract_main_results(),
    expected =
      c(1114, 586, 1634) # Results on 19 June 2024; no comparison study
  )
})

## Commented out because test takes a while
# testthat::test_that("results correct delta comparison iteration (high number of geo units) rr single exposures", {
#
#   testthat::expect_equal(
#     object =
#       healthiar::compare_health(
#         approach_comparison = "delta",
#         exp_central_1 = as.list(runif_with_seed(1E4, 8.0, 9.0, 1)),
#         exp_lower_1 = as.list(runif_with_seed(1E4, 8.0, 9.0, 1)-0.1),
#         exp_upper_1 = as.list(runif_with_seed(1E4, 8.0, 9.0, 1)+0.1),
#         exp_central_2 = as.list(runif_with_seed(1E4, 8.0, 9.0, 2)),
#         exp_lower_2 = as.list(runif_with_seed(1E4, 8.0, 9.0, 2)-0.1),
#         exp_upper_2 = as.list(runif_with_seed(1E4, 8.0, 9.0, 2)+0.1),
#         cutoff_central = 5,
#         bhd_central_1 = as.list(runif_with_seed(1E4, 25000, 35000, 1)),
#         bhd_lower_1 = as.list(runif_with_seed(1E4, 25000, 35000, 1)),
#         bhd_upper_1 = as.list(runif_with_seed(1E4, 25000, 35000, 1)),
#         bhd_central_2 = as.list(runif_with_seed(1E4, 25000, 35000, 1)),
#         bhd_lower_2 = as.list(runif_with_seed(1E4, 25000, 35000, 1)),
#         bhd_upper_2 = as.list(runif_with_seed(1E4, 25000, 35000, 1)),
#         rr_central = 1.369,
#         rr_lower = 1.124,
#         rr_upper = 1.664,
#         rr_increment = 10,
#         erf_shape = "log_linear",
#         geo_id_disaggregated = 1:1E4,
#         geo_id_aggregated = rep("CH", 1E4),
#         info_1 = "PM2.5_mortality_2010",
#         info_2 = "PM2.5_mortality_2020") |>
#       helper_extract_main_results(),
#     expected =
#       c(211111, 84203, 319618) # Results on 19 December 2024; no comparison study
#   )
# })

testthat::test_that("results correct delta comparison ar exposure distribution", {

  testthat::expect_equal(
    object =
      healthiar::compare_health(
        approach_risk = "absolute_risk",
        approach_comparison = "delta",
        exp_central_1 = c(57.5, 62.5, 67.5, 72.5, 77.5),# Values as example provided by NIPH
        exp_central_2 = c(50, 55, 60, 65, 75), # Fake values
        population_1 = sum(c(387500, 286000, 191800, 72200, 7700)),
        population_2 = sum(c(387500, 286000, 191800, 72200, 7700)),
        prop_pop_exp_1 = c(387500, 286000, 191800, 72200, 7700)/sum(c(387500, 286000, 191800, 72200, 7700)), # Values as example provided by NIPH
        prop_pop_exp_2 = c(387500, 286000, 191800, 72200, 7700)/sum(c(387500, 286000, 191800, 72200, 7700)), # Fake values
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        info_1 = data.frame(pollutant = "road_noise", outcome = "highly_annoyance", year = 2020),
        info_2 = data.frame(pollutant = "road_noise", outcome = "highly_annoyance", year = 2022)
      ) |>
      helper_extract_main_results(),
    expected =
      c(62531) # Results on 23 May 2024; no comparison study
  )
})

testthat::test_that("results correct delta comparison iteration ar exposure distribution", {

  testthat::expect_equal(
    object =
      healthiar::compare_health(
        approach_risk = "absolute_risk",
        approach_comparison = "delta",
        exp_central_1 = list(c(57.5, 62.5, 67.5, 72.5, 77.5), c(57, 62, 67, 72, 77)),# Fake values
        exp_central_2 = list(c(50, 55, 60, 65, 75), c(50.5, 55.5, 60.5, 65.5, 75.5)), # Fake values
        population_1 = list(945200, 929800),
        population_2 = list(945200, 929800),
        prop_pop_exp_1 = list(c(387500, 286000, 191800, 72200, 7700)/945200,
                              c(380000, 280000, 190800, 72000, 7000)/929800), # Fake values
        prop_pop_exp_2 = list(c(387500, 286000, 191800, 72200, 7700)/945200,
                              c(380000, 280000, 190800, 72000, 7000)/929800), # Fake values
        erf_eq_central = "78.9270-3.1162*c+0.0342*c^2",
        info_1 = data.frame(pollutant = "road_noise", outcome = "highly_annoyance", year = 2020),
        info_2 = data.frame(pollutant = "road_noise", outcome = "highly_annoyance", year = 2022),
        geo_id_disaggregated = c("a", "b"),
        geo_id_aggregated = rep("ch", 2)) |>
      helper_extract_main_results(),
    expected =
      c(115869) # Results on 19 June 2024; no comparison study
  )
})

