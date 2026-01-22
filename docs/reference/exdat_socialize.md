# Municipalities in Belgium ranked by BEST-COST Multidimensional Deprivation Index (MDI)

This tibble contains data for municipalities in Belgium assigning
BEST-COST Multidimensional Deprivation Index (MDI).

## Usage

``` r
data(exdat_socialize)
```

## Format

`exdat_socialize`

- NUTS1:

  NUTS1 region tag

- geo_unit:

  Unique identifier of the geographic unit

- age_group:
  
  Age groups "below 40" (excluding 40) and "40_plus" (including 40)

- pm25_mean:

  Mean PM2.5 exposure

- rr:

  Relative risk estimate from the literature
  
- mortality:

  Fake mortality data

- population:

  Fake population data

- score:

  BEST-COST Multidimensional Deprivation Index (MDI)
  
- ref_prop_pop:

  Fake reference proportion of population for each age group

## Source

Real-world data combined with fake population and mortality data

## Author

Arno Pauwels, Vanessa Gorasso, Axel Luyten, Alberto Castro
