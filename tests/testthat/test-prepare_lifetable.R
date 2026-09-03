# QUANTITATIVE TEST ############################################################

testthat::test_that("results correct|prepare_lifetable", {

  testthat::expect_equal(
    object =
      healthiar::prepare_lifetable(
        age_group = c(0, 5, 10, 15),
        population = c(3387900, 3401300, 3212300, 3026100),
        bhd = c(4727, 472, 557, 1323))$bhd_for_attribute |>
      base::round(),
    expected =
      # Example from AirQ+ Life Table Manual
      c(948, 947, 945, 944, 943,
        94, 94, 94, 94, 94,
        111, 111, 111, 111, 111,
        265, 265, 265, 264, 264)
  )
})

# Comparison with a published life table:
# Preston, Heuveline & Guillot (2001), page 49, Austrian males 1992
# (original data source: United Nations 1994).
# The published table reports ax, the average number of years lived in the age
# interval by those dying in it. fraction_lived is that value expressed as a
# fraction of the age interval, i.e. ax / 5 for the 5-year age groups used
# here, where it ranges from 0.48 to 0.63 and is therefore clearly away from
# the 0.5 that AirQ+ assumes
testthat::test_that("results correct|prepare_lifetable|published life table", {

  # Ages 5 to 84 in 5-year age groups, i.e. the equally spaced part of the
  # published abridged life table
  population <- c(234793, 238790, 254996, 326831, 355086, 324222, 269963, 261971,
                  238011, 261612, 181385, 187962, 153832, 105169, 73694, 57512)
  deaths <- c(36, 46, 249, 420, 403, 441, 508, 769,
              1154, 1866, 2043, 3496, 4366, 4337, 5279, 6460)
  ax <- c(2.5, 3.143, 2.724, 2.520, 2.481, 2.601, 2.701, 2.663,
          2.698, 2.676, 2.645, 2.624, 2.619, 2.593, 2.518, 2.423)

  # Survivors and deaths of the published life table (radix 100 000)
  lx_published <- c(98986, 98910, 98815, 98334, 97704, 97151, 96492, 95588,
                    94195, 91937, 88711, 83845, 76377, 66225, 53803, 37441)
  dx_published <- c(76, 95, 481, 630, 553, 659, 904, 1393,
                    2258, 3225, 4867, 7467, 10152, 12422, 16362, 16307)

  prob_dying <-
    healthiar::prepare_lifetable(
      age_group = base::seq(5, 80, 5),
      population = population,
      bhd = deaths,
      fraction_lived = ax / 5) |>
    dplyr::distinct(age_group_n_years, prob_dying_n_years) |>
    dplyr::pull(prob_dying_n_years)

  # Applying the probability of dying to the published survivors has to
  # reproduce the deaths of the published life table
  testthat::expect_equal(
    object = base::round(prob_dying * lx_published),
    expected = base::as.numeric(dx_published)
  )
})

# Same published life table, age 0, where the published ax is 0.068 years.
# The mid-year population of an age is the person-years lived at that age
# (nLx = 99 192), i.e. the survivors entering the age (lx = 100 000) minus the
# part of the year not lived by those dying in it (dx = 867). The entry
# population determined by prepare_lifetable() therefore has to reproduce the
# published number of survivors.
# The second age group only exists because at least two age groups are needed;
# for one-year age groups the entry population of an age is calculated from
# that age alone
testthat::test_that("results correct|prepare_lifetable|published entry population", {

  testthat::expect_equal(
    object =
      healthiar::prepare_lifetable(
        age_group = c(0, 1),
        population = c(99192, 99192),
        bhd = c(867, 867),
        fraction_lived = c(0.068, 0.068))$entry_population_1_year[1] |>
      base::round(),
    expected = 100000
  )
})

# Now with different fraction_lived depending on the age
# The conversion may neither create nor lose population or deaths,
# whatever fraction_lived is entered.
# This is the condition from which the entry population is derived
# (see prepare_lifetable()) and it holds for the default fraction_lived
# of 0.5, i.e. for the AirQ+ example above, as well as for any other value
testthat::test_that("results correct|prepare_lifetable|fraction_lived", {

  population <- c(3387900, 3401300, 3212300, 3026100)
  bhd <- c(4727, 472, 557, 1323)

  lifetable <-
    healthiar::prepare_lifetable(
      age_group = c(0, 5, 10, 15),
      population = population,
      bhd = bhd,
      fraction_lived = c(0.1, 0.5, 0.5, 0.5))

  testthat::expect_equal(
    object =
      lifetable |>
      dplyr::summarize(.by = age_group_n_years,
                       population = base::sum(population_for_attribute),
                       bhd = base::sum(bhd_for_attribute)) |>
      dplyr::select(population, bhd),
    expected = tibble::tibble(population = population, bhd = bhd)
  )
})

# If the data are already stratified by one-year age groups there is nothing
# to convert, so the function has to return the population and the deaths
# entered by the user. Previously the population was halved in this case.
# fraction_lived cancels out exactly here (the entry population adds
# (1 - fraction_lived) * bhd and the mid-year population subtracts it again),
# so a value other than 0.5 is entered to check that it does not disturb
# the identity
testthat::test_that("results correct|prepare_lifetable|single year input", {

  population <- c(41633, 42666, 43054, 43061, 42726)
  bhd <- c(137, 6, 3, 1, 1)

  testthat::expect_equal(
    object =
      healthiar::prepare_lifetable(
        age_group = 0:4,
        population = population,
        bhd = bhd,
        fraction_lived = c(0.1, 0.5, 0.5, 0.5, 0.5)) |>
      dplyr::select(population_for_attribute, bhd_for_attribute),
    expected = tibble::tibble(population_for_attribute = population,
                              bhd_for_attribute = bhd)
  )
})

# ERROR OR WARNING ########
## ERROR #########

testthat::test_that("error if lenght different|prepare_lifetable", {

  testthat::expect_error(
    object =
      healthiar::prepare_lifetable(
        age_group = c(0, 5, 10, 15, 20), # 20 should not be there
        population = c(3387900, 3401300, 3212300, 3026100),
        bhd = c(4727, 472, 557, 1323))$bhd_for_attribute |>
      base::round(),
    regexp =
      "The following variables must all have the same length: age_group, population, bhd."
  )
})

testthat::test_that("error if lower than min for age_group|prepare_lifetable", {

  testthat::expect_error(
    object =
      healthiar::prepare_lifetable(
        age_group = c(-1, 5, 10, 15),
        population = c(3387900, 3401300, 3212300, 3026100),# 0 should not be there
        bhd = c(4727, 472, 557, 1323))$bhd_for_attribute |>
      base::round(),
    regexp =
      "The values of age_group must be greater than or equal to 0."
  )
})


testthat::test_that("error if lower than min for population|prepare_lifetable", {

  testthat::expect_error(
    object =
      healthiar::prepare_lifetable(
        age_group = c(0, 5, 10, 15),
        population = c(0, 3401300, 3212300, 3026100),# 0 should not be there
        bhd = c(4727, 472, 557, 1323))$bhd_for_attribute |>
      base::round(),
    regexp =
      "The values of population must be greater than 0."
  )
})


testthat::test_that("error if lenght different|prepare_lifetable", {

  testthat::expect_error(
    object =
      healthiar::prepare_lifetable(
        age_group = c(0, 5, 10, 15),
        population = c(3387900, 3401300, 3212300, 3026100),
        bhd = c(4727, 472, 557, 1323),
        fraction_lived = 1.1 # This value should not be higher than 1
        )$bhd_for_attribute |>
      base::round(),
    regexp =
      "The values of fraction_lived cannot be greater than 1."
  )
})



## WARNING #########
