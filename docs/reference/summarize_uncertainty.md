# Get Monte Carlo confidence intervals

This function determines summary uncertainty (based on central, lower
and upper estimates of at least one input variable) using attribute() or
compare() function output by Monte Carlo simulation.

Input variables that will be processed are:

- relative_risk (`rr_...`)

- exposure (`exp_...`)

- cutoff (`cutoff_...`)

- baseline health data (`bhd_...`)

- disability weight (`dw_...`)

- duration (`duration_...`)

## Usage

``` r
summarize_uncertainty(output_attribute, n_sim, seed = NULL)
```

## Arguments

- output_attribute:

  `variable` in which the output of a `healthiar::attribute_...()`
  function call are stored.

- n_sim:

  `numeric value` indicating the number of simulations to be performed.

- seed:

  `numeric value` for fixing the randomization. Based on it, each
  geographic unit is assigned a different. If empty, 123 is used as the
  base seed per default. The function preserves and restores the user's
  original random seed (if set prior to calling the function) upon
  function completion.

## Value

This function returns a `list` containing:

1\) `uncertainty_main` (`tibble`) containing the `numeric` summary
uncertainty central estimate and corresponding lower and upper
confidence intervals for the attributable health impacts obtained
through Monte Carlo simulation;

2\) `uncertainty_detailed` (`list`) containing detailed (and interim)
results.

- `impact_by_sim` (`tibble`) containing the results for each simulation

- `uncertainty_by_geo_id_micro` (`tibble`) containing results for each
  geographic unit under analysis (specified in `geo_id_micro` argument
  in the preceding `attribute_health` call)

The two results elements are added to the existing output.

## Details

**Function arguments** `seed` If the `seed` argument is specified then
the `parallel` package is used to generate independent L’Ecuyer random
number streams. One stream is allocated per variable (or per
variable–geography combination, as needed), ensuring reproducible and
independent random draws across variables and scenarios.

**Methodology**

Information about the methodology (including corresponding equations and
literature) is available in the package vignette. More specifically, see
chapters:

- [Monte Carlo
  simulation](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#monte-carlo-simulation)

## References

Robert CP, Casella G (2004). *Monte Carlo Statistical Methods*, Springer
Texts in Statistics. Springer Science \\ Business Media.
[doi:10.1007/978-1-4757-4145-2](https://doi.org/10.1007/978-1-4757-4145-2)
.

Doucet A, Lee A, others (2020). “Monte Carlo Methods in the Twenty-First
Century.” *Annual Review of Statistics and Its Application*, **7**,
435–458.
[doi:10.1146/annurev-statistics-031219-041122](https://doi.org/10.1146/annurev-statistics-031219-041122)
.

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: obtain summary uncertainty for an existing attribute_health() output
# First create an assessment
attribute_health_output <- attribute_health(
  erf_shape = "log_linear",
  rr_central = 1.369,
  rr_lower = 1.124,
  rr_upper = 1.664,
  rr_increment = 10,
  exp_central = 8.85,
  exp_lower = 8,
  exp_upper = 10,
  cutoff_central = 5,
  bhd_central = 30747,
  bhd_lower = 28000,
  bhd_upper = 32000
)
# Then run Monte Carlo simulation
results <- summarize_uncertainty(
  output_attribute = attribute_health_output,
  n_sim = 100
)
results$uncertainty_main$impact # Central, lower and upper estimates
#> [1] 3646.684 1496.535 5770.173
```
