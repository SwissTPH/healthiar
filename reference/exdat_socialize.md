# Census sectors in Flanders (Belgium) with deprivation score

This data frame contains data for the census sectors of Flanders
(Belgium) with their deprivation score. Each census sector has one row
per age group. The census sectors without deprivation score were
removed, because the deprivation index covers fewer census sectors than
the mortality data.

## Usage

``` r
data(exdat_socialize)
```

## Format

`exdat_socialize`

- NUTS1:

  NUTS1 region tag

- geo_unit:

  unique identifier of the geographic unit (census sector)

- age_group:

  age group: `"below_40"` or `"40_plus"`

- pm25_mean:

  mean PM2.5 exposure

- rr:

  relative risk per 10 \\\mu g/m^3\\ increase in PM2.5 exposure

- mortality:

  (fake) mortality

- population:

  (fake) population

- score:

  deprivation score of the geographic unit

- ref_prop_pop:

  (fake) proportion of the age group in the reference population

## Source

Real-world data combined with fake population and mortality data

## Author

Arno Pauwels & Vanessa Gorasso
