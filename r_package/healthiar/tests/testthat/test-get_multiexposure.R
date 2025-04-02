# ARE THESE COMPARISON RESULTS FROM A STUDY? ####
testthat::test_that("results correct rr multiple exposure additive approach no variable uncertainties", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_multiexposure = "additive",
        exp_central = c("pm2.5" = 8.1, "no2" =  10.9),
        cutoff_central =  setNames(c(0, 0), c("pm2.5", "no2")),
        bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
        rr_central = setNames(c(1.063, 1.031), c("pm2.5", "no2")),
        rr_increment = setNames(c(10, 10), c("pm2.5", "no2")),
        erf_shape = "log_linear") |>
      helper_extract_main_results(),
    expected =
      c(0.081 * 1000) # Unsure whether numbers from a study...; Results on 2025-01-16
  )
})

testthat::test_that("detailed results the same rr multiple exposure additive approach with variable uncertainties", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_multiexposure = "additive",
        exp_central = c("pm2.5" = 8.1, "no2" =  10.9),
        exp_lower = c("pm2.5" = 8.1 - 1, "no2" =  10.9 - 1),
        exp_upper = c("pm2.5" = 8.1 + 1, "no2" =  10.9 + 1),
        cutoff_central =  setNames(c(0, 0), c("pm2.5", "no2")),
        bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
        rr_central = setNames(c(1.063, 1.031), c("pm2.5", "no2")),
        rr_lower = setNames(c(1.063, 1.031) - 0.005, c("pm2.5", "no2")),
        rr_upper = setNames(c(1.063, 1.031) + 0.005, c("pm2.5", "no2")),
        rr_increment = setNames(c(10, 10), c("pm2.5", "no2")),
        erf_shape = "log_linear") |>
      helper_extract_detailed_results(),
    expected = # Results on 2025-01-20; no comparison study
      c(33, 30, 36, 28, 25, 30, 38, 34, 41, 48, 42, 54, 45, 39, 50, 52, 46, 58)
  )
})

# ARE THESE COMPARISON RESULTS FROM A STUDY? ####
testthat::test_that("results correct rr multiple exposure multiplicative approach", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_multiexposure = "multiplicative",
        exp_central = c("pm2.5" = 8.1, "no2" =  10.9),
        cutoff_central =  setNames(c(0, 0), c("pm2.5", "no2")),
        bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
        rr_central = setNames(c(1.063, 1.031), c("pm2.5", "no2")),
        rr_increment = setNames(c(10, 10), c("pm2.5", "no2")),
        erf_shape = "log_linear") |>
      helper_extract_main_results(),
    expected =
      c(0.079 * 1000) # Unsure whether numbers from a study...; Results on 16 Jan 2025
  )
})

# ARE THESE COMPARISON RESULTS FROM A STUDY? ####
testthat::test_that("results correct rr multiple exposure multiplicative approach without cutoff", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_multiexposure = "multiplicative",
        exp_central = c("pm2.5" = 8.1, "no2" =  10.9),
        bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
        rr_central = setNames(c(1.063, 1.031), c("pm2.5", "no2")),
        rr_increment = setNames(c(10, 10), c("pm2.5", "no2")),
        erf_shape = "log_linear") |>
      helper_extract_main_results(),
    expected =
      c(0.079 * 1000) # Unsure whether numbers from a study...; Results on 16 Jan 2025
  )
})

# ARE THESE COMPARISON RESULTS FROM A STUDY? ####
# ADD TEST ONCE BUG IS FIXED ###################################################
# testthat::test_that("results correct rr multiple exposure multiplicative approach with variable uncertainty", {
#
#   testthat::expect_equal(
#     object =
#       healthiar::attribute_health(
#         approach_multiexposure = "multiplicative",
#         exp_central = c("pm2.5" = 8.1, "no2" =  10.9),
#         exp_lower = c("pm2.5" = 8.1 - 1, "no2" =  10.9 - 1),
#         exp_upper = c("pm2.5" = 8.1 + 1, "no2" =  10.9 + 1),
#         cutoff_central =  setNames(c(0, 0), c("pm2.5", "no2")),
#         bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
#         rr_central = setNames(c(1.063, 1.031), c("pm2.5", "no2")),
#         rr_lower = setNames(c(1.063, 1.031) - 0.005, c("pm2.5", "no2")),
#         rr_upper = setNames(c(1.063, 1.031) + 0.005, c("pm2.5", "no2")),
#         rr_increment = setNames(c(10, 10), c("pm2.5", "no2")),
#         erf_shape = "log_linear") |>
#       helper_extract_main_results(),
#     expected =
#       c(0.079 * 1000) # Unsure whether numbers from a study...; Results on 16 Jan 2025
#   )
# })

# ARE THESE COMPARISON RESULTS FROM A STUDY? ####
testthat::test_that("results correct rr multiple exposure combined approach with cutoff", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_multiexposure = "combined",
        exp_central = c("pm2.5" = 8.1, "no2" =  10.9),
        cutoff_central =  setNames(c(0, 0), c("pm2.5", "no2")),
        bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
        rr_central = setNames(c(1.063, 1.031), c("pm2.5", "no2")),
        rr_increment = setNames(c(10, 10), c("pm2.5", "no2")),
        erf_shape = "log_linear") |>
      helper_extract_main_results(),
    expected =
      c(0.079 * 1000) # Unsure whether numbers from a study...; Results on 16 Jan 2025
  )
})

# ARE THESE COMPARISON RESULTS FROM A STUDY? ####
testthat::test_that("results correct rr multiple exposure combined approach without cutoff", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_multiexposure = "combined",
        exp_central = c("pm2.5" = 8.1, "no2" =  10.9),
        bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
        rr_central = setNames(c(1.063, 1.031), c("pm2.5", "no2")),
        rr_increment = setNames(c(10, 10), c("pm2.5", "no2")),
        erf_shape = "log_linear") |>
      helper_extract_main_results(),
    expected =
      c(0.079 * 1000) # Unsure whether numbers from a study...; Results on 16 Jan 2025
  )
})

# ARE THESE COMPARISON RESULTS FROM A STUDY? ####
testthat::test_that("results correct rr multiple exposure combined approach with ci", {

  testthat::expect_equal(
    object =
      healthiar::attribute_health(
        approach_multiexposure = "combined",
        exp_central = c("pm2.5" = 8.1, "no2" =  10.9),
        exp_lower = c("pm2.5" = 7, "no2" =  9),
        exp_upper = c("pm2.5" = 9, "no2" =  12),
        cutoff_central =  setNames(c(0, 0), c("pm2.5", "no2")),
        bhd_central = 1000, # Fake data just to get a similar value (PAF) as in the T1.4 report
        rr_central = setNames(c(1.063, 1.031), c("pm2.5", "no2")),
        rr_lower = setNames(c(1.05, 1.02), c("pm2.5", "no2")),
        rr_upper = setNames(c(1.07, 1.04), c("pm2.5", "no2")),
        rr_increment = setNames(c(10, 10), c("pm2.5", "no2")),
        erf_shape = "log_linear") |>
      helper_extract_main_results(),
    expected =
      c(79,59,93) # Unsure whether numbers from a study...; Results on 16 Jan 2025
  )
})

