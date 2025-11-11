# Consider socio-economic aspects in healthiar assessments

This function considers socio-economic aspects (e.g. multiple
deprivation index) in the attributable health impacts. If nothing is
entered in the argument `output_attribute`, it is assumed that all data
come from a table and the argument refer to the columns of that table.

## Usage

``` r
socialize(
  output_attribute = NULL,
  age_group,
  geo_id_micro,
  social_indicator = NULL,
  increasing_deprivation = TRUE,
  n_quantile = NULL,
  social_quantile = NULL,
  population = NULL,
  ref_prop_pop = NULL,
  impact = NULL,
  exp = NULL,
  bhd = NULL,
  pop_fraction = NULL
)
```

## Arguments

- output_attribute:

  `List` containing the outputs of the
  [`healthiar::attribute_health()`](https://github.com/SwissTPH/healthiar/reference/attribute_health.md)
  assessments for each age group (each list element should be an age
  group-specific assessment).

- age_group:

  `String vector` with the age groups included in the age
  standardization. The vector refers to age-dependent data in this
  function and to `output_attribute` (if provided).

- geo_id_micro, :

  `Numeric vector` or `string vector` specifying the unique ID codes of
  each geographic area considered in the assessment (`geo_id_micro`)
  Argument must be entered for iterations. See Details for more info.

- social_indicator:

  `Numeric vector` showing the social indicator used for the analysis,
  e.g. a deprivation score (indicator of economic wealth) for each
  geographic unit. Based on this and `n_quantile`, `social_quantile`
  will be calculated.

- increasing_deprivation:

  `Boolean` variable (`TRUE`/`FALSE`) specifying whether an increase in
  `social_indicator` corresponds to an increase (`TRUE`) or decrease
  `FALSE` in deprivation. Default: `TRUE`.

- n_quantile:

  `Integer value` specifying the number of quantiles in the analysis.

- social_quantile:

  `Integer vector` showing the values from 1 to the number of quantiles
  assigned to each geographic unit. Either enter `social_indicator` and
  `n_quantile` or `social_quantile`

- population:

  `Numeric vector` specifying the population by age group and geographic
  unit.

- ref_prop_pop:

  `Numeric vector` specifying with the reference proportion of
  population for each age group. If this argument is empty, the
  proportion of `population` by age group in the provided data will be
  used.

- impact:

  *(only if `output_attribute` not specified)* `Numeric vector`
  containing the attributable health impacts by both age group and geo
  id.

- exp:

  *(only if `output_attribute` not specified)* `Numeric vector`
  specifying the exposure level(s) to the environmental stressor.

- bhd:

  *(only if `output_attribute` not specified)* `Numeric vector`
  specifying the baseline health data of the health outcome of interest
  per age group. See Details for more info.

- pop_fraction:

  *(only if `output_attribute` not specified)* `Numeric vector`
  specifying the population attributable fraction by age group and
  geographic unit.

## Value

This function returns a `list` containing the impact (absolute and
relative) theoretically attributable to the difference in the social
indicator (e.g. degree of deprivation) between the quantiles:

1\) `social_main` (`tibble`) containing the main results;

- `difference_value` (`numeric` column) attributable health
  burden/impact due to differences in deprivation levels

- And more

2\) `social_detailed` (`list`) containing detailed (and interim)
results.

- `input_data_with_quantile` (`tibble`) containing input data and
  information about the social quantile

- `results_all_parameters` (`tibble`) containing deprivation-related
  results

- `parameters_overall` (`tibble`) containing overall results for
  different input variables

- `parameters_per_quantile` (`tibble`) containing quantile-specific
  results for different input variables

If the argument `output_attribute` was specified, then the two lists are
added next to the existing attribute output.

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: determine fraction of attributable health impact that can
# be attributed to differences in deprivation between the geographic
# units under analysis

## Create assessments for multiple geographic units for the age group
## 40 years and younger
results_age_groups <-
  healthiar::attribute_health(
    age_group = rep(c("below_40", "40_plus"), each = 9037),
    exp_central = c(exdat_socialize$PM25_MEAN, exdat_socialize$PM25_MEAN-0.1),
    cutoff_central = 0,
    rr_central = 1.08,
    erf_shape = "log_linear",
    rr_increment = 10,
    bhd_central =  c(exdat_socialize$MORTALITY_below_40, exdat_socialize$MORTALITY_40_plus),
    population = c(exdat_socialize$POPULATION_below_40, exdat_socialize$POPULATION_40_plus),
    geo_id_micro = rep(exdat_socialize$CS01012020, 2))

## Difference in attributable impacts between geographic units
## that is attributable to differences in deprivation
results <- socialize(
  age_group = c("below_40", "40_plus"),
  ref_prop_pop = c(0.5, 0.5),
  output_attribute = results_age_groups,
  geo_id_micro = exdat_socialize$CS01012020,
  social_indicator = exdat_socialize$score,
  n_quantile = 10,
  increasing_deprivation = TRUE)

results$social_main |>
  dplyr::filter(difference_type == "relative") |>
  dplyr::filter(difference_compared_with == "overall") |>
  dplyr::select(first, last, difference_type, difference_value, comment)
#> # A tibble: 1 × 5
#>   first  last difference_type difference_value comment                          
#>   <dbl> <dbl> <chr>                      <dbl> <chr>                            
#> 1  70.7  59.0 relative                 -0.0110 It can be interpreted as fractio…
```
