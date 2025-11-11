# Attributable disability-adjusted life years

This function calculates the disability-adjusted life years (DALY)
attributable to the exposure to an environmental stressor by adding the
two DALY components YLL and YLD.

## Usage

``` r
daly(output_attribute_yll, output_attribute_yld)
```

## Arguments

- output_attribute_yll, output_attribute_yld:

  `variable` containing YLL or YLD results of a `attribute_...()`
  function call, respectively.

## Value

This function returns a `list` containing:

1\) `health_main` (`tibble`) containing the main results;

- `impact` (`numeric` column) attributable health burden/impact in DALY

- `impact_yld` (`numeric` column) attributable health burden/impact in
  YLD

- `impact_yll` (`numeric` column) attributable health burden/impact in
  YLL

- `dw` (`numeric` column) disability weight used for YLD calculation

- And many more

2\) `health_detailed` (`list`) containing detailed (and interim)
results.

- `results_raw` (`tibble`) containing results for each combination of
  input uncertainty

- `results_by_geo_id_micro` (`tibble`) containing results for each
  geographic unit under analysis (specified in `geo_id_micro` argument)

- `input_args` (`list`) containing all the argument inputs used in the
  background

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: obtain DALY (disability-adjusted life years) from two existing \code{attribute_...} outputs
# Step 1: Create YLL (years of life lost) assessment
results_yll <- attribute_lifetable(
  health_outcome = "yll",
  approach_exposure = "single_year",
  approach_newborns = "without_newborns",
  exp_central = 8.85,
  prop_pop_exp = 1,
  cutoff_central = 5,
  rr_central =  1.118,
  rr_increment = 10,
  erf_shape = "log_linear",
  age_group = exdat_lifetable$age_group,
  sex = exdat_lifetable$sex,
  bhd_central = exdat_lifetable$deaths,
  population = exdat_lifetable$midyear_population,
  year_of_analysis = 2019,
  min_age = 20
)
# Step 2: Create YLD (years lived with disability) assessment
results_yld  <- attribute_health(
  exp_central = 8.85,
  prop_pop_exp = 1,
  cutoff_central = 5,
  bhd_central = 1000,
  rr_central = 1.1,
  rr_increment = 10,
  erf_shape = "log_linear",
  duration_central = 100,
  dw_central = 0.5,
  info = "pm2.5_yld"
)
# Step 3: obtain DALY
results <- daly(
  output_attribute_yll = results_yll,
  output_attribute_yld = results_yld
)
# Attributable impact in DALY
results$health_main |>
  dplyr::select(impact, impact_yll, impact_yld)
#> # A tibble: 1 × 3
#>   impact impact_yll impact_yld
#>    <dbl>      <dbl>      <dbl>
#> 1 30611.     28810.      1801.
```
