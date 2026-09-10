# QUANTITATIVE TEST ############################################################

# Validation against HM Treasury, "Discounting: Green Book supplementary
# guidance" (February 2026), Annex A, Table A.1: the discount factors of the
# Social Time Preference Rate of 3.5%, which practitioners "should multiply by
# the social costs and social benefits in the relevant years of an appraisal"
# (paragraph 1.3). The discounted impact divided by the undiscounted one is
# therefore the published factor, which the table gives with four decimals
green_book_year <- c(1, 5, 10, 20, 30)
green_book_factor <- c(0.9662, 0.8420, 0.7089, 0.5026, 0.3563)

testthat::test_that("results correct |direct discounting without valuation with exponential discount shape|hm_treasury_2026|", {

  impact <- 2E4

  testthat::expect_equal(
    object =
      purrr::map_dbl(
        green_book_year,
        \(year)
        healthiar::discount(
          impact = impact,
          discount_shape = "exponential",
          discount_rate = 0.035,
          n_years = year)$monetization_main$monetized_impact / impact) |>
      base::round(digits = 4),
    expected = green_book_factor)
})

testthat::test_that("results correct discount existing attribute_health() output|hm_treasury_2026|", {

  # Parameters of the COPD example of the Swiss GeLuft assessment in WHO AirQ+
  # (see testdata/airqplus_pm_copd.rds). The impact itself is not the point
  # here: discounting it over a given number of years has to reduce it by the
  # published Green Book factor
  results <- attribute_health(
    erf_shape = "log_linear",
    rr_central = 1.369,
    rr_increment = 10,
    exp_central = 8.85,
    cutoff_central = 5,
    bhd_central = 30747
  )

  testthat::expect_equal(
    object =
      purrr::map_dbl(
        green_book_year,
        \(year)
        healthiar::discount(
          output_attribute = results,
          discount_shape = "exponential",
          discount_rate = 0.035,
          n_years = year)$monetization_main$monetized_impact /
          results$health_main$impact) |>
      base::round(digits = 4),
    expected = green_book_factor)
})

testthat::test_that("results the same discount existing attribute_health() output", {

  # EKV2010 data
  data <- base::readRDS(testthat::test_path("testdata", "lifetable_male_ekv_2010.rds"))


  health_impact <- healthiar::attribute_lifetable(
    health_outcome = "yll",
    exp_central = 10,
    cutoff_central = 0,
    rr_central = 1.045,
    rr_increment = 10,
    erf_shape = "log_linear",
    age_group = data$age,
    sex = base::rep(c("male"), each = 106),
    population = data$population_male,
    bhd_central = as.numeric(data$deaths_natural_male),
    year_of_analysis = 2010,
    min_age = 20)

  testthat::expect_equal(
    object =
      healthiar::discount(
        output_attribute = health_impact,
        discount_shape = "exponential",
        discount_rate = 0.0099)$monetization_main$monetized_impact |> base::round(),
    expect = 13453)
  # The result in the EKV2010 project was 12600.
  # Similar deviation as when calculating only health impacts (without discounting)
})




# ERROR OR WARNING ########
## ERROR #########

## WARNING #########
