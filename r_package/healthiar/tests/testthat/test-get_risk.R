# QUANTITATIVE TEST ############################################################
testthat::test_that("linear rescaling correct", {
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
}
)

testthat::test_that("log-linear rescaling the same", {
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 20,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_linear"
    ) |> base::round(x = _, digits = 4),
    expected =
      1.1224 # Results on 06 August 2024 (ChatGPT); no comparison study
  )
}
)

testthat::test_that("linear-log rescaling the same", {
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 20,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_log"
      ) |> base::round(x = _, digits = 4),
    expected =
      1.0941 # Results on 06 August 2024 (ChatGPT); no comparison study
  )
}
)

## This example uses the log-log curve initially proposed by ChatGPT, which is not defined for exp = 0 or exp <= cutoff (that's why it's commented out)
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

## This example uses the log-log curve based on Pozzer 2022 (http://doi.org/10.1029/2022GH000711)
testthat::test_that("log-log rescaling the same", {

  ## exp = 15
  ### because exp - cutoff = 15 - 5 = 10, the result matches exactly the rr value from the literature
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 15,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_log"
    ),
    expected =
      1.08 # Results on 08 August 2024 (ChatGPT); no comparison study
  )

  ## exp = cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 5,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_log"
    ),
    expected =
      1 # Results on 08 August 2024 (ChatGPT); no comparison study
  )

  ## exp = 0, cutoff = 5
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 5,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_log"
    ),
    expected =
      1 # Results on 08 August 2024 (ChatGPT); no comparison study
  )

  ## exp = 20
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 20,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_log"
    ),
    expected =
      1.103291954 # Results on 08 August 2024 (ChatGPT); no comparison study
  )

  ## exp = 10
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 10,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_log"
    ),
    expected =
      1.048709767 # Results on 08 August 2024 (ChatGPT); no comparison study
  )

  ## exp = 30
  testthat::expect_equal(
    object = healthiar::get_risk(
      exp = 30,
      cutoff = 5,
      rr = 1.08,
      rr_increment = 10,
      erf_shape = "log_log"
    ),
    expected =
      1.13752842 # Results on 08 August 2024 (ChatGPT); no comparison study
  )
}
)

# ERROR OR WARNING ########
## ERROR #########

## WARNING #########
