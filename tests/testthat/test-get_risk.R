# QUANTITATIVE TEST ############################################################
testthat::test_that("results the same |linear rescaling results correct", {

  ## exp = 10, cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 10,
      cutoff = 5,
      rr = 1.1,
      rr_increment = 10,
      erf_shape = "linear"
    ),
    expected = 1.05
    )

  ## exp = 15, cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 15,
      cutoff = 5,
      rr = 1.1,
      rr_increment = 10,
      erf_shape = "linear"
    ),
    expected = 1.1
  )

  ## exp = 0, cutoff = 0
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 0,
      cutoff = 0,
      rr = 1.1,
      rr_increment = 10,
      erf_shape = "linear"
    ),
    expected = 1
  )

  ## exp = 0, cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 0,
      cutoff = 5,
      rr = 1.1,
      rr_increment = 10,
      erf_shape = "linear"
    ),
    expected = 1
  )

}
)

testthat::test_that("results the same | log-linear rescaling results the same", {

  ## exp = 20, cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 20,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_linear"
    ) |> base::round(x = _, digits = 4),
    expected =
      1.1224
  )

  ## exp = 15, cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 15,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_linear"
    ) |> base::round(x = _, digits = 4),
    expected =
      1.08
  )

  ## exp = 5, cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 5,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_linear"
    ) |> base::round(x = _, digits = 4),
    expected =
      1
  )

  ## exp = 0, cutoff = 0
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 0,
      cutoff = 0,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_linear"
    ) |> base::round(x = _, digits = 4),
    expected =
      1
  )

  ## exp = 0, cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 0,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_linear"
    ) |> base::round(x = _, digits = 4),
    expected =
      1
  )

}
)

## NOTE 2025-08-08: This example uses the log-log curve initially proposed by ChatGPT, which is not defined for exp = 0 or exp <= cutoff (that's why it's commented out); once we've settled on these new ERFs remove these error messages
# testthat::test_that("linear-log rescaling the same", {
#   testthat::expect_equal(
#     object = healthiar::get_risk(
#       exp = 20,
#       cutoff = 5,
#       rr = 1.08,
#       rr_increment = 10,
#       erf_shape = "log_log"
#       ) |> base::round(x = _, digits = 4),
#     expected =
#       1.0941 # Results on 06 August 2024 (ChatGPT); no comparison study
#   )
# }
# )

## This example uses the adapted lin-log curve (adapted based on the on the Pozzer 2022 (http://doi.org/10.1029/2022GH000711) log-log ERF)
## The curve itself is not published anywhere: it is an adaptation of the
## Pozzer log-log curve made for healthiar (see get_risk()), so there is no
## study whose figures it could be validated against. Only the anchors of the
## curve are known to be correct, i.e. no excess risk at the cut-off and
## exactly the relative risk of the study one increment above it
testthat::test_that("results the same |linear-log rescaling results the same", {

  ## exp = 20, cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 20,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "linear_log"
      ),
    expected =
      1.102179903 # Result on 08 August 2024; no comparison study
  )

  ## exp = 15, cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 15,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "linear_log"
    ),
    expected =
      1.08
  )

  ## exp = 5, cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 5,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "linear_log"
    ),
    expected =
      1
  )

  ## exp = 0, cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 5,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "linear_log"
    ),
    expected =
      1
  )

  ## exp = 0, cutoff = 0
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 5,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "linear_log"
    ),
    expected =
      1
  )

}
)


testthat::test_that("results correct |log-log rescaling|pozzer_2023|", {

  # Validation against the log-log exposure-response function published by
  # Pozzer et al. (2023), see rr_at_exp_pozzer() in helper.R
  rr <- 1.08
  rr_increment <- 10
  cutoff <- 5
  exp <- c(5, 10, 15, 20, 30)

  testthat::expect_equal(
    object =
      healthiar::get_risk(
        exp = exp,
        cutoff = cutoff,
        rr = rr,
        rr_increment = rr_increment,
        erf_shape = "log_log"),
    expected =
      rr_at_exp_pozzer(
        exp = exp,
        cutoff = cutoff,
        rr = rr,
        rr_increment = rr_increment))

  # The curve is anchored at the values of the epidemiological study: no
  # excess risk at the cut-off and exactly the published relative risk one
  # increment above it
  testthat::expect_equal(
    object =
      rr_at_exp_pozzer(
        exp = c(cutoff, cutoff + rr_increment),
        cutoff = cutoff,
        rr = rr,
        rr_increment = rr_increment),
    expected = c(1, rr))
})

## NOTE 2025-08-08: This example uses the log-log curve initially proposed by ChatGPT, which is not defined for exp = 0 or exp <= cutoff (that's why it's commented out); once we've settled on these new ERFs remove these error messages
# testthat::test_that("log-log rescaling the same", {
#   testthat::expect_equal(
#     object = healthiar::get_risk(
#       exp = 20,
#       cutoff = 5,
#       rr = 1.08,
#       rr_increment = 10,
#       erf_shape = "log_log"
#     ) |> base::round(x = _, digits = 4),
#     expected =
#       1.0947 # Results on 06 August 2024 (ChatGPT); no comparison study
#   )
# }
# )

testthat::test_that("results correct |log-log rescaling based on Lehtomäki et al.", {

#Lehtomäki et al. 2024
data <-
  utils::read.csv(testthat::test_path("testdata", "erf_helilog_logcurve.csv"))

testthat::expect_equal(
  signif(healthiar::get_risk(
  exp = rep(data$exposure, each = 3),
  cutoff = 0,
  rr = c(1.08,1.06,1.09), #actual-cause mortality was 1.08 (95%CI 1.06, 1.09) per 10 µg/m3 (Chen and Hoek 2020).
  rr_increment = c(10),
  erf_shape = "log_log"),5),
  expected = c(matrix(c(data$RRcentral,data$RR.lower,data$RRupper), nrow = 3, byrow = TRUE)))

})


# ERROR OR WARNING ########
## ERROR #########
## WARNING #########

