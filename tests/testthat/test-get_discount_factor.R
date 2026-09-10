# QUANTITATIVE TEST ############################################################

testthat::test_that("results correct |get_discount_factor|exponential|hm_treasury_2026|", {

  # Validation against the discount factor tables published by HM Treasury in
  # "Discounting: Green Book supplementary guidance" (February 2026), Annex A.
  # Its paragraph 2.9 defines the factor as 1 / (1 + r)^n, i.e. the
  # exponential discount shape, and tabulates it for the years 0 to 30 at the
  # Social Time Preference Rate of 3.5% (Table A.1) and at the health discount
  # rate of 1.5% (Table A.2)
  #
  # Entering the whole time horizon at once also guards the vectorization:
  # before, the nested ifelse() returned a result of the length of its
  # condition, i.e. of the length of the single discount_shape, so the vector
  # of years was silently truncated to its first element

  ## TABLE A.1: SOCIAL TIME PREFERENCE RATE OF 3.5% ############################

  testthat::expect_equal(
    object =
      round(
        healthiar::get_discount_factor(
          discount_rate = 0.035,
          n_years = 0:30),
        digits = 4),
    expected =
      c(1.0000, 0.9662, 0.9335, 0.9019, 0.8714, 0.8420, 0.8135, 0.7860,
        0.7594, 0.7337, 0.7089, 0.6849, 0.6618, 0.6394, 0.6178, 0.5969,
        0.5767, 0.5572, 0.5384, 0.5202, 0.5026, 0.4856, 0.4692, 0.4533,
        0.4380, 0.4231, 0.4088, 0.3950, 0.3817, 0.3687, 0.3563))

  ## TABLE A.2: HEALTH DISCOUNT RATE OF 1.5% ###################################

  testthat::expect_equal(
    object =
      round(
        healthiar::get_discount_factor(
          discount_rate = 0.015,
          n_years = 0:30),
        digits = 4),
    expected =
      c(1.0000, 0.9852, 0.9707, 0.9563, 0.9422, 0.9283, 0.9145, 0.9010,
        0.8877, 0.8746, 0.8617, 0.8489, 0.8364, 0.8240, 0.8118, 0.7999,
        0.7880, 0.7764, 0.7649, 0.7536, 0.7425, 0.7315, 0.7207, 0.7100,
        0.6995, 0.6892, 0.6790, 0.6690, 0.6591, 0.6494, 0.6398))
})
