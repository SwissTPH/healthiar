# Municipalities in Belgium ranked by BEST-COST Multidimensional Deprivation Index (MDI)

This tibble contains data for municipalities in Belgium ranked by
BEST-COST Multidimensional Deprivation Index (MDI).

## Usage

``` r
data(exdat_socialize)
```

## Format

`exdat_socialize`

- CS01012020:

  unique identifier of the geographic unit

- NUTS1:

  NUTS1 region tag

- PM25_MEAN:

  mean PM2.5 exposure

- RR:

  relative risk estimate from the literature

- score:

  BEST-COST Multidimensional Deprivation Index (MDI)

- rank:

  rank of the observation based on column *score*; note that the rank is
  not continuous, as some observations are missing

- deciles:

  deciles of the geo units based on the MDI

- POPULATION_below_40:

  (fake) populations up until and including 39 years of age

- POPULATION_40_plus:

  (fake) populations from 40 years of age onwards

- MORTALITY_below_40:

  (fake) mortality up until and including 39 years of age

- MORTALITY_40_plus:

  (fake) mortality from 40 years of age onwards

## Source

Real-world data combined with fake populatoin and mortality data

## Author

Arno Pauwels & Vanessa Gorasso
