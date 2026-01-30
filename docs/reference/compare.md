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

**Function arguments**

`approach_comparison` Please note that the PIF comparison approach
assumes same baseline health data for scenario 1 and 2 (e.g. comparison
of two scenarios at the same time point).

With the delta comparison approach, the difference between two scenarios
is obtained by subtraction. The delta approach is suited for all
comparison cases, and specifically for comparison of a situation now
with a situation in the future.

**Methodology**

Information about the methodology (including corresponding equations and
literature) is available in the package vignette. More specifically, see
chapters:

- [comparison of two health
  scenarios](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#comparison-of-two-health-scenarios)

## Author

Alberto Castro & Axel Luyten

## Examples
