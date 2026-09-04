# Get Monte Carlo confidence intervals

This function obtains a summary of uncertainty (based on central, lower
and upper estimates of at least one input variable) using a Monte Carlo
simulation.

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

  `numeric value` for fixing the randomization, so that the same call
  always returns the same results. If empty (default), the base seed is
  drawn from the random number generator currently in use, i.e. the
  results differ across calls unless the user calls
  [`set.seed()`](https://rdrr.io/r/base/Random.html) beforehand. The
  function preserves and restores the user's original random seed (if
  set prior to calling the function) upon function completion.

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

**Function arguments** `seed` The `parallel` package is used to generate
independent L’Ecuyer random number streams. One stream is allocated per
variable (or per variable–geography combination, as needed), ensuring
reproducible and independent random draws across variables. The streams
are shared by both scenarios of a comparison (see
[`compare`](https://swisstph.github.io/healthiar/reference/compare.md)),
so that the variables that are common to both scenarios (e.g. `rr_...`)
take the same simulated value in each simulation of both scenarios. This
is the case whether or not `seed` is entered by the user.

**Methodology**

This function summarizes the uncertainty of the attributable health
impacts (i.e. a single confidence interval instead of many
combinations). For this purpose, it employs a Monte Carlo simulation
methodology (Robert and Casella 2004) and framework application
(Rubinstein and Kroese 2016) .

The variables that cannot be negative and are simulated with a normal
distribution (`exp_...`, `cutoff_...`, `bhd_...` and `duration_...`) are
drawn from that distribution truncated at zero. As the normal
distribution is symmetric, the simulated values reproduce the entered
confidence interval only if `..._lower` and `..._upper` are symmetric
around `..._central`. The more asymmetric the entered confidence
interval, the more the simulated values depart from it.

If the assessment covers several geographic units, the uncertainty of
the aggregated unit (`geo_id_macro`) is obtained by first summing the
impacts of all `geo_id_micro` within each simulation and only then
taking the quantiles of those sums.

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [Monte Carlo
  simulation](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#monte-carlo-simulation)

## References

Robert CP, Casella G (2004). *Monte Carlo Statistical Methods*, Springer
Texts in Statistics. Springer Science and Business Media.
[doi:10.1007/978-1-4757-4145-2](https://doi.org/10.1007/978-1-4757-4145-2)
.  
  
Rubinstein RY, Kroese DP (2016). *Simulation and the Monte Carlo
Method*. John Wiley and Sons.
[doi:10.1002/9781118631980](https://doi.org/10.1002/9781118631980) .

## See also

- Upstream:
  [`attribute_health`](https://swisstph.github.io/healthiar/reference/attribute_health.md),
  [`attribute_lifetable`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md),
  [`compare`](https://swisstph.github.io/healthiar/reference/compare.md)

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
#> [1] 3422.472 1528.661 5602.330

```
