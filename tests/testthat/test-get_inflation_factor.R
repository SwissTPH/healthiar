# QUANTITATIVE TEST ############################################################

testthat::test_that("results correct |get_inflation_factor|eurostat_hicp|", {

  # Validation against the Harmonised Index of Consumer Prices (HICP) of the
  # euro area published by Eurostat (all-items COICOP CP00, annual average
  # index 2015 = 100, and annual average rate of change; online data code
  # prc_hicp_aind). Inflating a value of 2015 to 2020 prices must reproduce
  # the ratio of the published indices:
  #
  #   Year   2015    2016    2017    2018    2019    2020
  #   Index  100.00  100.23  101.78  103.56  104.80  105.06
  #   Rate       -      0.2%    1.5%    1.8%    1.2%    0.3%
  #
  # The tolerance is needed because Eurostat publishes the rates of change
  # rounded to one decimal, so their product differs from the ratio of the
  # published indices in the fourth decimal
  rate <- c(0.002, 0.015, 0.018, 0.012, 0.003)
  index_2015 <- 100.00
  index_2020 <- 105.06

  ## YEAR-SPECIFIC INFLATION RATES #############################################

  testthat::expect_equal(
    object =
      healthiar::get_inflation_factor(
        inflation_rate = rate,
        n_years = 5),
    expected = index_2020 / index_2015,
    tolerance = 1E-3)

  ## DEFLATION ################################################################

  # Deflating 2020 prices back to 2015 is the inverse operation
  testthat::expect_equal(
    object =
      healthiar::get_inflation_factor(
        inflation_rate = rate,
        n_years = 5,
        is_deflation = TRUE),
    expected = index_2015 / index_2020,
    tolerance = 1E-3)

  ## CONSTANT INFLATION RATE ##################################################

  # One single rate means constant inflation, so the factor compounds over the
  # years. Between 2015 and 2020 the euro area HICP grew at a compound annual
  # rate of 0.99%, which has to give the same 2015-to-2020 factor as the
  # year-specific rates above
  testthat::expect_equal(
    object =
      healthiar::get_inflation_factor(
        inflation_rate = (index_2020 / index_2015) ^ (1 / 5) - 1,
        n_years = 5),
    expected = index_2020 / index_2015)
})
