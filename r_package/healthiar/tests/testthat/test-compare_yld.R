testthat::test_that("results correct delta comparison yld rr single exposure", {

  testthat::expect_equal(
    object =
      healthiar::compare_yld(
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
        dw_central = 0.5,
        duration_central = 1,
        info_1 = "PM2.5_yld_before",
        info_2 = "PM2.5_yld_after") |>
      helper_extract_main_results(),
    expected =
      c(387, 205, 564) # Result on 16 May 2024; no comparison study to
  )
})

testthat::test_that("results from scenario 1 match those calculated by attribute call with same input data", {

  testthat::expect_equal(
    object =
      healthiar::compare_yld(
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
        dw_central = 0.5,
        duration_central = 1,
        info_1 = "PM2.5_yld_before",
        info_2 = "PM2.5_yld_after") |>
      purrr::pluck("health_main") |>
      dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
      dplyr::select(impact_rounded_1) |>
      base::unlist() |>
      base::as.numeric(),
    expected =
      healthiar::attribute_yld(
        exp_central = 8.85,
        cutoff_central = 5,
        bhd_central = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        dw_central = 0.5, dw_lower = 0.1, dw_upper = 10,
        duration_central = 1, duration_lower = 0.5, duration_upper = 10) |>
      helper_extract_main_results()
  )
})

testthat::test_that("results correct delta comparison yld iteration rr single exposure", {

  testthat::expect_equal(
    object =
      healthiar::compare_yld(
        approach_comparison = "delta",
        exp_central_1 = list(8.85, 8.0),
        exp_central_2 = list(6, 6.5),
        cutoff_central = 5,
        bhd_central_1 = 25000,
        bhd_central_2 = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        dw_central = 0.5,
        duration_central = 1,
        info_1 = "PM2.5_yld_before",
        info_2 = "PM2.5_yld_after",
        geo_id_disaggregated = c("a", "b"),
        geo_id_aggregated = rep("ch", 2)) |>
      helper_extract_main_results(),
    expected =
      c(591, 313, 861) # Result on 26 June 2024; no comparison study to
  )
})

testthat::test_that("results correct pif comparison yld rr single exposure", {

  testthat::expect_equal(
    object =
      healthiar::compare_yld(
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
        dw_central = 0.5,
        duration_central = 1,
        info_1 = "PM2.5_yld_before",
        info_2 = "PM2.5_yld_after") |>
      helper_extract_main_results(),
    expected =
      c(391,206,573) # Result on 16 May 2024; no comparison study to
  )
})

testthat::test_that("results correct pif comparison yld iteration rr single exposure", {

  testthat::expect_equal(
    object =
      healthiar::compare_yld(
        approach_comparison = "pif",
        exp_central_1 = list(8.85, 8.0),
        exp_central_2 = list(6, 6.5),
        cutoff_central = 5,
        bhd_central_1 = 25000,
        bhd_central_2 = 25000,
        rr_central = 1.118,
        rr_lower = 1.060,
        rr_upper = 1.179,
        rr_increment = 10,
        erf_shape = "log_linear",
        dw_central = 0.5,
        duration_central = 1,
        info_1 = "PM2.5_yld_before",
        info_2 = "PM2.5_yld_after",
        geo_id_disaggregated = c("a", "b"),
        geo_id_aggregated = rep("ch", 2)) |>
      helper_extract_main_results(),
    expected =
      c(599, 315, 878) # Result on 20 June 2024; no comparison study to
  )
})


