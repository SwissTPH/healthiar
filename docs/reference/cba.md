# Cost-benefit analysis

This function performs a cost-benefit analysis. It is assumed that the
benefit is caused by the positive health impacts of a policy
intervention, which generates some costs.

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
  real_growth_rate = NULL,
  discount_shape = "exponential",
  n_years_benefit = 1,
  n_years_cost = 0
)
```

## Arguments

- output_attribute:

  `List` produced by
  [`healthiar::attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md),
  [`healthiar::attribute_lifetable()`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md)
  or
  [`healthiar::compare()`](https://swisstph.github.io/healthiar/reference/compare.md)
  as results.

- impact_benefit:

  `Numeric value` referring to the positive health impact as result of a
  reduction of harmful exposure.

- valuation:

  `Numeric value` referring to unit value of a health impact.

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
  (increase of prices). This value is used to adjust monetization for
  inflation (converting nominal into real values by appyling a
  deflator). If this adjustment for inflation is not needed leave this
  argument empty (default value = NULL).

- real_growth_rate:

  `Numeric value` between 0 and 1 referring to the annual real-term
  appreciation in the societal value of health (e.g., income
  elasticity). This adjusts the valuation upward to reflect rising
  wealth, independent of general price inflation.

- discount_shape:

  `String` referring to the assumed equation for the discount factor. By
  default: `"exponential"`. Otherwise: `"hyperbolic_harvey_1986"` or
  `"hyperbolic_mazur_1987"`.

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

**Methodology**

This function provides as output three types of cost-benefit indicators
(Boardman et al. 2018) :

- net benefit

- cost-benefit ratio

- return of investment

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [cost-benefit
  analysis](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#cost-benefit-analysis)

- [monetization](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#monetization)

## References

Boardman AE, Greenberg DH, Vining AR, Weimer DL (2018). *Cost-Benefit
Analysis: Concepts and Practice*, 5th edition. Cambridge University
Press, Cambridge, UK. ISBN 978-1108401296.

## See also

- Upstream:
  [`attribute_health`](https://swisstph.github.io/healthiar/reference/attribute_health.md)

- Alternative:
  [`monetize`](https://swisstph.github.io/healthiar/reference/monetize.md)

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
