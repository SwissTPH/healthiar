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

Note that the PIF comparison approach assumes same baseline health data
for scenario 1 and 2 (e.g. comparison of two scenarios at the same
time).

**Equations population impact fraction (PIF)**

The Population Impact Fraction (PIF) is defined as the proportional
change in disease or mortality when exposure to a risk factor is changed
(for instance due to an intervention). The most general equation
describing this mathematically is an integral form (WHO 2003a,
https://www.who.int/publications/i/item/9241546204; WHO 2003b,
https://doi.org/10.1186/1478-7954-1-1): \$\$PIF = \frac{\int
RR(x)PE(x)dx - \int RR(x)PE'(x)dx}{\int RR(x)PE(x)dx}\$\$

Where:

x = exposure level

PE(x) = population distribution of exposure

PE'(x) = alternative population distribution of exposure

RR(x) = relative risk at exposure level compared to the reference level

If the population exposure is described as a categorical rather than
continuous exposure, the integrals in equation (5) may be converted to
sums, resulting in the following equations for the PIF (WHO 2003a,
https://www.who.int/publications/i/item/9241546204; WHO 2003b,
https://doi.org/10.1186/1478-7954-1-1): \$\$PIF = \frac{\sum RR\_{i}
\times PE\_{i} - \sum RR\_{i}PE'\_{i}}{\sum RR\_{i}PE\_{i}}\$\$

Where:

i = is the exposure category (e.g. in bins of 1 \\\mu g/m^3\\ PM2.5 or 5
dB noise exposure)

\\PE_i\\ = fraction of population in exposure category i

\\PE'\_i\\ = fraction of population in category i for alternative
(ideal) exposure scenario

\\RR_i\\ = relative risk for exposure category level i compared to the
reference level

Finally, if the exposure is provided as the population weighted mean
concentration (PWC), the equation for the PIF is reduced to: \$\$PIF =
\frac{RR\_{PWC} - RR\_{alt PWC}}{RR\_{PWC}}\$\$

Where:

\\RR\_{PWC}\\ = relative risk associated with the population weighted
mean exposure

\\RR\_{PWC}\\ = relative risk associated with the population weighted
mean for the alternative exposure scenario

**Delta comparison approach**

With the delta comparison the difference between two scenarios is
obtained by subtraction. The delta approach is suited for all comparison
cases, and specifically for comparison of a situation now with a
situation in the future.

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
