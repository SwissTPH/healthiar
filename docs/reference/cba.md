# Cost-benefit analysis

This function performs a cost-benefit analysis

## Usage

``` r
cba(
  output_attribute = NULL,
  impact_benefit = NULL,
  valuation,
  cost,
  discount_rate_benefit = NULL,
  discount_rate_cost = NULL,
  inflation_rate = NULL,
  discount_shape = "exponential",
  n_years_benefit = 1,
  n_years_cost = 1
)
```

## Arguments

- output_attribute:

  `List` produced by `healthiar::attribute()` or
  [`healthiar::compare()`](https://github.com/SwissTPH/healthiar/reference/compare.md)
  as results.

- impact_benefit:

  `Numeric value` referring to the positive health impact as result of a
  reduction of harmful exposure.

- valuation:

  `Numberic value` referring to unit value of a health impact.

- cost:

  `Numeric value` referring to the investment cost to achieve the
  reduction of exposure.

- discount_rate_benefit, discount_rate_cost:

  `Numeric value` referring to the the discount rate used in the benefit
  and the cost side (respectively). Their values determine the approach
  of cost-benefit analysis: direct approach (if the same discount_rate
  is used for cost and benefit) and indirect approach (different
  discount rates).

- inflation_rate:

  `Numeric value` between 0 and 1 referring to the annual inflation
  (increase of prices). Only to be entered if nominal (not real)
  discount rate is entered in the function. Default value = NULL
  (assuming no nominal discount rate).

- discount_shape:

  `String` referring to the assumed equation for the discount factor. By
  default: "exponential". Otherwise: "hyperbolic_harvey_1986" or
  "hyperbolic_mazur_1987".

- n_years_benefit, n_years_cost:

  `Numeric value` referring to number of years in the future to be
  considered in the benefit and cost side (respectively). Years for
  discounting and/or inflation. Be aware that the year 0 (without
  discounting/inflation, i.e. the present) is not be counted here. If a
  vector is entered in the argument impact, n_years does not need to be
  entered (length of impact = n_years + 1)

## Value

This function returns a `list` containing:

1\) `cba_main` (`tibble`) containing the main CBA results;

- `net_benefit` (`numeric` column) containing the difference between
  benefit and cost (i.e. benefit - cost)

- `benefit` (`numeric` column) containing discounted benefit (i.e.
  monetized attributable health impact)

- `cost` (`numeric` column) containing discounted cost

- And many more

2\) `cba_detailed` (`list`) containing detailed (and interim) results.

- `benefit` (`list`)

- `cost` (`tibble`)

If the argument `output_attribute` was specified, then the two results
elements are added to the existing output.

## Details

**Equation cost-benefit analysis**

\$\$net\\benefit = benefit - cost\$\$

\$\$cost\\benefit\\ratio = \frac{benefit}{cost}\$\$

\$\$return\\on\\investment = \frac{benefit - cost}{cost } \times 100\$\$

For the equations regarding the monetization of the cost and the benefit
please see the function documentation of
[`monetize()`](https://github.com/SwissTPH/healthiar/reference/monetize.md).

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: performs a cost-benefit analysis using an existing output
# of a attribute_... function

output_attribute <- attribute_health(
  erf_shape = "log_linear",
  rr_central = 1.369,
  rr_increment = 10,
  exp_central = 8.85,
  cutoff_central = 5,
  bhd_central = 30747
)

results <- cba(
  output_attribute = output_attribute,
  valuation = 50000,
  cost = 100000000,
  discount_shape = "exponential",
  discount_rate_benefit = 0.03,
  discount_rate_cost = 0.03,
  n_years_benefit = 5,
  n_years_cost = 5
)

results$cba_main |>
  dplyr::select(benefit, cost, net_benefit)
#> # A tibble: 1 × 3
#>      benefit      cost net_benefit
#>        <dbl>     <dbl>       <dbl>
#> 1 151041153. 86260878.   64780274.
```
