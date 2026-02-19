# Consider socio-economic aspects in the attributable health impacts

This function analyzes differences in attributable health impacts across
study areas looking at the value of a socio-economic indicator (e.g.
multiple deprivation index). If nothing is entered in the argument
`output_attribute`, it is assumed that all data come from a table and
the argument refer to the columns of that table.

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
  [`healthiar::attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md)
  assessments for each age group (each list element should be an age
  group-specific assessment).

- age_group:

  `String vector` with the age groups included in the age
  standardization. The vector refers to age-dependent data in this
  function and to `output_attribute` (if provided).

- geo_id_micro, :

  `Numeric vector` or `string vector` specifying the unique ID codes of
  each geographic area considered in the assessment (`geo_id_micro`).

- social_indicator:

  `Numeric vector` showing the social indicator used for the analysis,
  e.g. a deprivation score (indicator of economic wealth) for each
  geographic unit. The length and the values must correspond with
  `geo_id_micro`. If `geo_id_micro` is not entered when using argument
  `output_attribute`, `social_indicator` must correspond to the column
  `geo_id_micro` in `results_by_age_group` of `output_attribute`.

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
  per age group.

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

## Details

**Methodology**

This function estimates the absolute and relative differences in
attributable health impacts comparing study areas with different values
for a socio-economic indicator (Renard et al. 2019; Otavova et al. 2022)
.

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [Health impact attributable to social
  indicator](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#health-impact-attributable-to-social-indicator)

## References

Otavova M, Faes C, Bouland C, De Clercq E, Vandeninden B, Eggerickx T,
Sanderson J, Devleesschauwer B, Masquelier B (2022). “Inequalities in
mortality associated with housing conditions in Belgium between 1991 and
2020.” *BMC Public Health*, **22**(1), 2397. ISSN 1471-2458,
[doi:10.1186/s12889-022-14819-w](https://doi.org/10.1186/s12889-022-14819-w)
.  
  
Renard F, Devleesschauwer B, Speybroeck N, Deboosere P (2019).
“Monitoring health inequalities when the socio-economic composition
changes: are the slope and relative indices of inequality appropriate?
Results of a simulation study.” *BMC Public Health*, **19**(1), 662.
ISSN 1471-2458,
[doi:10.1186/s12889-019-6980-1](https://doi.org/10.1186/s12889-019-6980-1)
.

## See also

- Upstream:
  [`attribute_health`](https://swisstph.github.io/healthiar/reference/attribute_health.md),
  [`attribute_lifetable`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md),
  [`prepare_mdi`](https://swisstph.github.io/healthiar/reference/prepare_mdi.md),

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
     age_group = exdat_socialize$age_group,
     exp_central = exdat_socialize$pm25_mean,
     cutoff_central = 0,
     rr_central = exdat_socialize$rr,
     erf_shape = "log_linear",
     rr_increment = 10,
     bhd_central = exdat_socialize$mortality,
     population = exdat_socialize$population,
     geo_id_micro = exdat_socialize$geo_unit)

## Difference in attributable impacts between geographic units
## that is attributable to differences in deprivation
results <- socialize(
  output_attribute = results_age_groups,
  age_group = exdat_socialize$age_group, # The same as in attribute_health()
  ref_prop_pop = exdat_socialize$ref_prop_pop,
  geo_id_micro = exdat_socialize$geo_unit,
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
#> 1  70.9  59.4 relative                 -0.0143 It can be interpreted as fractio…

```
