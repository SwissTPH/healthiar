# Aggregate health impacts from multiple exposures

This function aggregates health impacts from multiple exposures to
environmental stressors.

## Usage

``` r
multiexpose(
  output_attribute_exp_1,
  output_attribute_exp_2,
  exp_name_1,
  exp_name_2,
  approach_multiexposure = "additive"
)
```

## Arguments

- output_attribute_exp_1, output_attribute_exp_2:

  Output of attribute() for exposure 1 and 2, respectively. Baseline
  health data and population must be identical in outputs 1 and 2.

- exp_name_1, exp_name_2:

  `String` referring to the name of the environmental exposures 1 and 2

- approach_multiexposure:

  `String` specifying the multiple exposures approach to be used in the
  assessment. Options: "additive" (default), "multiplicative" or
  "combined".

## Value

This function returns a `list` containing:

1\) `health_main` (`tibble`) containing the main results;

- `impact` (`numeric` column) attributable health burden/impact

- `pop_fraction` (`numeric` column) population attributable fraction;
  only applicable in relative risk assessments

- And many more

2\) `health_detailed` (`list`) containing detailed (and interim)
results.

- `input_args` (`list`) containing all the argument inputs used in the
  background

- `input_table` (`tibble`) containing the inputs after preparation

- `results_raw` (`tibble`) containing results for all combinations of
  input (geo units, uncertainty, age and sex specific data...)

- `results_by_...` (`tibble`) containing results stratified by each
  geographic unit, age or sex.

## Details

**Methodology**

This function can add up the attributable health impacts from correlated
exposures applying one of the following methods (Strak et al. 2024) :

- Additive (Steenland and Armstrong 2006)

- Multiplicative (Jerrett et al. 2013)

- Combined (Steenland and Armstrong 2006)

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [Two correlated
  exposures](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#two-correlated-exposures)

## References

Jerrett M, Burnett RT, Beckerman BS, Turner MC, Krewski D, Thurston G,
Martin RV, van Donkelaar A, Hughes E, Shi Y, Gapstur SM, Thun MJ, Pope
3CA (2013). “Spatial analysis of air pollution and mortality in
California.” *American Journal of Respiratory and Critical Care
Medicine*, **188**(5), 593–599.
[doi:10.1164/rccm.201303-0609OC](https://doi.org/10.1164/rccm.201303-0609OC)
.  
  
Steenland K, Armstrong B (2006). “An overview of methods for calculating
the burden of disease due to specific risk factors.” *Epidemiology*,
**17**(5), 512–519.
[doi:10.1097/01.ede.0000229155.05644.43](https://doi.org/10.1097/01.ede.0000229155.05644.43)
.  
  
Strak M, Houthuijs D, Staatsen B (2024). “D1.2 Report on the methodology
for assessing the burden of correlated exposures.” EU Project BEST-COST.

## See also

- Upstream:
  [`attribute_health`](https://swisstph.github.io/healthiar/reference/attribute_health.md),
  [`attribute_lifetable`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md)

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: determine aggregated health impacts from multiple exposures
# Step 1: create assessment with exposure 1
output_attribute_exp_1 <- attribute_health(
  erf_shape = "log_linear",
  rr_central = 1.369,
  rr_increment = 10,
  exp_central = 8.85,
  cutoff_central = 5,
  bhd_central = 30747
)
output_attribute_exp_1$health_main$impact
#> [1] 3501.962
# Step 2: create assessment with exposure 2
output_attribute_exp_2 <- attribute_mod(
  output_attribute = output_attribute_exp_1,
  exp_central = 10.9,
  rr_central = 1.031
)
output_attribute_exp_2$health_main$impact
#> [1] 548.8641
# Step 3: aggregate impacts of the two assessments
results <- multiexpose(
  output_attribute_exp_1 = output_attribute_exp_1,
  output_attribute_exp_2 = output_attribute_exp_2,
  exp_name_1 = "pm2.5",
  exp_name_2 = "no2",
  approach_multiexposure = "multiplicative"
)
results$health_main$impact
#> [1] 3988.312

```
