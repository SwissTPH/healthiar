# Compare the attributable health impacts between two scenarios

This function calculates the health impacts between two scenarios (e.g.
before and after a intervention in a health impact assessments) using
either the delta or pif approach.

## Usage

``` r
compare(
  output_attribute_scen_1,
  output_attribute_scen_2,
  approach_comparison = "delta"
)
```

## Arguments

- output_attribute_scen_1:

  Scenario 1 as in the output of attribute()

- output_attribute_scen_2:

  Scenario 2 as in the output of attribute()

- approach_comparison:

  `String` showing the method of comparison. Options: "delta" or "pif".

## Value

This function returns a `list` containing:

1\) `health_main` (`tibble`) containing the main results from the
comparison;

- `impact` (`numeric` column) difference in attributable health
  burden/impact between scenario 1 and 2

- `impact_scen_1` (`numeric` column) attributable health impact of
  scenario 1

- `impact_scen_2` (`numeric` column) attributable health impact of
  scenario 2

- And many more

2\) `health_detailed` (`list`) containing detailed (and interim) results
from the comparison.

- `results_raw` (`tibble`) containing comparison results for each
  combination of input uncertainty for both scenario 1 and 2

- `results_by_geo_id_micro` (`tibble`) containing comparison results for
  each geographic unit under analysis (specified in `geo_id_micro`
  argument)

- `results_by_geo_id_macro` (`tibble`) containing comparison results for
  each aggregated geographic unit under analysis (specified in
  `geo_id_macro` argument))

- `input_table` (`list`) containing the inputs to each relevant argument
  for both scenario 1 and 2

- `input_args` (`list`) containing all the argument inputs for both
  scenario 1 and 2 used in the background

- `scen_1` (`tibble`) containing results for scenario 1

- `scen_2` (`tibble`) containing results for scenario 2

## Details

**Methodology** This function compares the attributable health impacts
in scenario 1 with scenario 2. It can use two approaches:

- Delta: Subtraction of health impacts in the two scenarios (two PAF)
  (Organization 2014)

- Population impact fraction (PIF): Single PIF for both scenarios (WHO
  2003; Murray et al. 2003)

Please note that the PIF comparison approach assumes same baseline
health data for scenario 1 and 2 (e.g. comparison of two scenarios at
the same time point).

With the delta comparison approach, the difference between two scenarios
is obtained by subtraction. The delta approach is suited for all
comparison cases, and specifically for comparison of a situation now
with a situation in the future.

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [comparison of two health
  scenarios](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#comparison-of-two-health-scenarios)

## References

Murray CJL, Ezzati M, Lopez AD, Rodgers A, Vander Hoorn S (2003).
“Comparative quantification of health risks conceptual framework and
methodological issues.” *Popul. Health Metr.*, **1**(1), 1.  
  
Organization WH (2014). *Health impact assessment of air pollution:
Guide for practitioners*. WHO Regional Office for Europe, Copenhagen.
<https://apps.who.int/iris/handle/10665/144701>.  
  
WHO (2003). “Introduction and methods: Assessing the environmental
burden of disease at national and local levels.” World Health
Organization. <https://www.who.int/publications/i/item/9241546204>.

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: comparison of two scenarios with delta approach
scenario_A <- attribute_health(
  exp_central = 8.85,   # EXPOSURE 1
  cutoff_central = 5,
  bhd_central = 25000,
  approach_risk = "relative_risk",
  erf_shape = "log_linear",
  rr_central = 1.118,
  rr_increment = 10
)
scenario_B <- attribute_health(
  exp_central = 6,     # EXPOSURE 2
  cutoff_central = 5,
  bhd_central = 25000,
  approach_risk = "relative_risk",
  erf_shape = "log_linear",
  rr_central = 1.118,
  rr_increment = 10
)
results <- compare(
approach_comparison = "delta",
output_attribute_scen_1 = scenario_A,
output_attribute_scen_2 = scenario_B
)
# Inspect the difference, stored in the \code{impact} column
results$health_main |>
  dplyr::select(impact, impact_scen_1, impact_scen_2) |>
  print()
#> # A tibble: 1 × 3
#>   impact impact_scen_1 impact_scen_2
#>    <dbl>         <dbl>         <dbl>
#> 1   774.         1051.          277.

# Goal: comparison of two scenarios with population impact fraction (pif) approach
output_attribute_scen_1 <- attribute_health(
  exp_central = 8.85,   # EXPOSURE 1
  cutoff_central = 5,
  bhd_central = 25000,
  approach_risk = "relative_risk",
  erf_shape = "log_linear",
  rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
  rr_increment = 10
)
output_attribute_scen_2 <- attribute_health(
  exp_central = 6,      # EXPOSURE 2
  cutoff_central = 5,
  bhd_central = 25000,
  approach_risk = "relative_risk",
  erf_shape = "log_linear",
  rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
  rr_increment = 10
)
results <- compare(
  output_attribute_scen_1 = output_attribute_scen_1,
  output_attribute_scen_2 = output_attribute_scen_2,
  approach_comparison = "pif"
)
# Inspect the difference, stored in the impact column
results$health_main$impact
#> [1]  782.2331  411.7377 1146.1450
```
