# PM2.5 exposure and COPD incidence in Switzerland

This tibble contains PM2.5 exposure and COPD incidence data from
Switzerland.

## Usage

``` r
data(exdat_cantons)
```

## Format

`exdat_cantons`

- year:

  year

- canton:

  abbreviation of Swiss cantons

- lung_cancer_incidence:

  lung cancer incidence

- exposure:

  mean country-wide population-weighted exposure level

- pollutant:

  PM2.5

- exposure_type:

  exposure type

- population:

  number of inhabitants per canton

- rr:

  central relative risk estimate

- rr_l:

  lower 95% confidence interval bound of the relative risk estimate

- rr_u:

  upper 95% confidence interval bound of the relative risk estimate

- increment:

  exposure increment in \\\mu g/m^3\\ for which the relative risk
  estimates are valid

- function_shape:

  shape of the exposure-response function

- cutoff:

  cutoff level below which no health effects are attributable to the
  exposure

- language_main:

  language spoken by the majority of inhabitants in the canton

- canton_long:

  full (English) name of the canton

## Source

Real-world data

## Author

Alberto Castro & Axel Luyten
