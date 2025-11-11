# Obtain age-standardized health impacts

This function obtains age-standardized health impacts based on multiple
age-group specific assessments

## Usage

``` r
standardize(output_attribute, age_group, ref_prop_pop = NULL)
```

## Arguments

- output_attribute:

  `List` containing the outputs of the
  [`healthiar::attribute_health()`](https://swisstph.github.io/healthiar/dev/reference/attribute_health.md)
  assessments for each age group (each list element should be an age
  group-specific assessment).

- age_group:

  `String vector` with the age groups included in the age
  standardization. The vector refers to age-dependent data in this
  function and to `output_attribute` (if provided).

- ref_prop_pop:

  `Numeric vector` specifying with the reference proportion of
  population for each age group. If this argument is empty, the
  proportion of `population` by age group in the provided data will be
  used.

## Value

This function returns a `list` containing:

1\) `health_main` (`tibble`) containing the age-standardized main
results;

2\) `health_detailed` (`tibble`) containing the results per age group.

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: age-standardize two age group-specific impacts
output_attribute <- attribute_health(
  rr_central = 1.063,
  rr_increment = 10,
  erf_shape = "log_linear",
  cutoff_central =  0,
  age_group = c("below_40", "above_40"),
  exp_central = c(8.1, 10.9),
  bhd_central = c(1000, 4000),
  population = c(100000, 500000)
)
results <- standardize(
  output_attribute = output_attribute,
  age_group = c("below_40", "above_40"),
  ref_prop_pop = c(0.5, 0.5)
)
results$health_detailed$impact_per_100k_inhab # age group-specific impact rate
#> [1] 48.28250 51.53977
results$health_main$impact_per_100k_inhab # age-standardized impact rate
#> [1] 49.91113
```
