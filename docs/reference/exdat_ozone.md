# PM2.5 exposure and COPD incidence in Switzerland

This tibble contains modelled ozone (\\O_3\\) exposure and chronic
obstructive pulmonary disease (COPD) incidence data from the Germany in
2016.

## Usage

``` r
data(exdat_ozone)
```

## Format

`exdat_ozone`

- pollutant:

  \\O_3\\

- exposure:

  mean exposure level in the exposure category

- exp_unit:

  unit of the exposure

- proportion_population_exposed:

  proportion of the total population exposed to each exposure category

- mortality_copd_tota_yearl:

  mortality due to chronic obstructive pulmonary disease (ICD-10 J40-44)

- rr_central:

  central relative risk estimate

- rr_lower:

  lower 95% confidence interval bound of the relative risk estimate

- rr_upper:

  upper 95% confidence interval bound of the relative risk estimate

- rr_increment:

  exposure increment in \\\mu g/m^3\\ for which the relative risk
  estimates are valid

- cutoff:

  cutoff level below which no health effects are attributable to the
  exposure

- erf_shape:

  shape of the exposure-response function

- exposure_type:

  exposure type

- rr_source:

  source of the relative risk estimates

- country:

  country

- year:

  year of the data

## Source

Real-world data

## Author

Alberto Castro & Axel Luyten
