# PM2.5 exposure and COPD incidence in Switzerland

This tibble contains PM2.5 exposure and COPD incidence data from
Switzerland.

## Usage

``` r
data(exdat_pm)
```

## Format

`exdat_pm`

- year_of_analysis:

  year that the exposure and incidence data is from

- mean_concentration:

  population-weighted annual mean concentration

- relative_risk:

  central relative risk estimate

- relative_risk_lower:

  lower 95% confidence interval bound of the relative risk estimate

- relative_risk_upper:

  upper 95% confidence interval bound of the relative risk estimate

- erf_shape:

  shape of the exposure-response function

- incidence:

  COPD incidence in the year of analysis

- cutoff_value:

  cut-off value

- rr_increment:

  exposure increment in \\\mu g/m^3\\ for which the relative risk
  estimates are valid

- rr_source:

  source of the relative risk

- rr_doi:

  DOI linking to the publication from which the relative risk was taken

## Source

Real-world data

## Author

Alberto Castro & Axel Luyten
