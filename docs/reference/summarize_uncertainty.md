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

**Method**

For each processed input variable with a provided 95% confidence
interval value, a distribution is fitted (see below). From these,
`n_sim` input value sets are sampled to compute `n_sim` attributable
impacts. The median value of these attributable impacts is reported as
the central estimate, and the 2.5th and 97.5th percentiles define the
lower and upper bounds of the 95% summary uncertainty confidence
interval, respectively. Aggregated central, lower and upper estimates
are obtained by summing the corresponding values of each lower level
unit.

**Distributions used for simulation**

Relative risk values are simulated based on an optimized gamma
distribution, which fits well as relative risks are positive and its
distributions usually right-skewed. The gamma distribution best fitting
the inputted central relative risk estimate and corresponding lower and
upper 95% confidence interval values is fitted using
[`stats::qgamma()`](https://rdrr.io/r/stats/GammaDist.html) (with
`rate = shape / rr_central`) and then
[`stats::optimize`](https://rdrr.io/r/stats/optimize.html) is used to
optimize the distribution parameters. Finally, `n_sim` relative risk
values are simulated using
[`stats::rgamma()`](https://rdrr.io/r/stats/GammaDist.html).

Exposure values are simulated based on a normal distribution using
[`stats::rnorm()`](https://rdrr.io/r/stats/Normal.html) with
`mean = exp_central` and a standard deviation based on corresponding
lower and upper 95% exposure confidence interval values.

Cutoff values are simulated based on a normal distribution using
[`stats::rnorm()`](https://rdrr.io/r/stats/Normal.html) with
`mean = cutoff_central` and a standard deviation based on corresponding
lower and upper 95% cutoff confidence interval values.

Baseline health data values are simulated based on a normal distribution
using [`stats::rnorm()`](https://rdrr.io/r/stats/Normal.html) with
`mean = bhd_central` and a standard deviation based on corresponding
lower and upper 95% exposure confidence interval values.

Disability weights values of the morbidity health outcome of interest
are simulated based on a beta distribution, as both the disability
weights and the beta distribution are bounded by 0 and 1. The beta
distribution best fitting the inputted central disability weight
estimate and corresponding lower and upper 95% confidence interval
values is fitted using
[`stats::qgamma()`](https://rdrr.io/r/stats/GammaDist.html) (the best
fitting distribution parameters `shape1` and `shape2` are determined
using [`stats::optimize()`](https://rdrr.io/r/stats/optimize.html)).
Finally, `n_sim` disability weight values are simulated using
[`stats::rbeta()`](https://rdrr.io/r/stats/Beta.html).

Duration values of the morbidity health outcome of interest are
simulated based on a normal distribution using
[`stats::rnorm()`](https://rdrr.io/r/stats/Normal.html) with
`mean = duration_central` and a standard deviation based on
corresponding lower and upper 95% exposure confidence interval values.

**Function arguments**

`seed`

If the `seed` argument is specified then the `parallel` package is used
to generate independent L’Ecuyer random number streams. One stream is
allocated per variable (or per variable–geography combination, as
needed), ensuring reproducible and independent random draws across
variables and scenarios.

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
#> [1] 3394.468 1141.035 5770.980
```
