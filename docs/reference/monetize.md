# Monetize health impacts

This function monetizes health impacts

## Usage

``` r
monetize(
  output_attribute = NULL,
  impact = NULL,
  valuation,
  discount_rate = NULL,
  discount_shape = "exponential",
  n_years = NULL,
  inflation_rate = NULL,
  info = NULL
)
```

## Arguments

- output_attribute:

  `List` produced by `healthiar::attribute()` or
  [`healthiar::compare()`](https://swisstph.github.io/healthiar/reference/compare.md)
  as results.

- impact:

  `Numberic value` referring to the health impacts to be monetized
  (without attribute function). If a `Numberic vector` is entered
  multiple assessments (by year) will be carried out. Be aware that the
  value for year 0 (current) must be entered, while n_years does not
  include the year 0. Thus, length of impact = n_years + 1.

- valuation:

  `Numberic value` referring to unit value of a health impact.

- discount_rate:

  `Numeric value` showing the discount rate for future years. If it is a
  nominal discount rate, no inflation is to be entered. If it is a real
  discount rate, the result can be adjusted by entering inflation in
  this function.

- discount_shape:

  `String` referring to the assumed equation for the discount factor. By
  default: "exponential". Otherwise: "hyperbolic_harvey_1986" or
  "hyperbolic_mazur_1987".

- n_years:

  `Numeric value` referring to number of years in the future to be
  considered in the discounting and/or inflation. Be aware that the year
  0 (without discounting/inflation, i.e. the present) is not be counted
  here. If a vector is entered in the argument impact, n_years does not
  need to be entered (length of impact = n_years + 1).

- inflation_rate:

  `Numeric value` between 0 and 1 referring to the annual inflation
  (increase of prices). Only to be entered if nominal (not real)
  discount rate is entered in the function. Default value = NULL
  (assuming no nominal discount rate).

- info:

  `String`, `data frame` or `tibble` providing **information about the
  assessment**. Only attached if `impact` is entered by the users. If
  `output_attribute` is entered, use `info` in that function or add the
  column manually. *Optional argument.*

## Value

This function returns a `list` containing:

1\) `monetization_main` (`tibble`) containing the main monetized
results;

- `monetized_impact` (`numeric` column)

- `discount_factor` (`numeric` column) calculated based on the entered
  `discount_rate`

- And many more

2\) `monetization_detailed` (`list`) containing detailed (and interim)
results.

- `results_by_year` (`tibble`)

- `health_raw` (`tibble`) containing the monetized results for each for
  each combination of input uncertainty that were provided to the
  initial
  [`attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md)
  call

If the argument `output_attribute` was specified, then the two results
elements are added to the existing output.

## Details

**Equation inflation factor (without discounting)**

\$\$inflation\\factor = (1 + inflation\\rate)^{n\\years}\$\$

**Equations discount factors (without inflation)**

*Exponential discounting (no inflation)* \$\$discount\\factor =
\frac{1}{(1 + discount\\rate) ^{n\\years}}\$\$

*Hyperbolic discounting Harvey (no inflation)* \$\$discount\\factor =
\frac{1}{(1 + n\\years)^{discount\\rate}}\$\$ *Hyperbolic discounting
Mazure (no inflation)* \$\$discount\\factor = \frac{1}{(1 +
(discount\\rate \times n\\years)}\$\$

**Equations discount factors with inflation**

*Exponential discounting (with inflation)*
\$\$discount\\and\\inflation\\factor = \frac{1}{((1 + discount\\rate)
\times (1 + inflation\\rate)) ^{n\\years}}\$\$

*Hyperbolic discounting Harvey (with inflation)*
\$\$discount\\and\\inflation\\factor = \frac{1}{(1 +
n\\years)^{discount\\rate} \times (1 + inflation\\rate)^{n\\years}}\$\$
*Hyperbolic discounting Mazure (with inflation)*
\$\$discount\\and\\inflation\\factor = \frac{1}{(1 + (discount\\rate
\times n\\years) \times (1 + inflation\\rate)^{n\\years}}\$\$

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: monetize the attributable impacts of an existing healthiar
# assessment
output_attribute <- attribute_health(
erf_shape = "log_linear",
rr_central = exdat_pm$relative_risk,
rr_increment = 10,
exp_central = exdat_pm$mean_concentration,
cutoff_central = exdat_pm$cut_off_value,
bhd_central = exdat_pm$incidence
)

results <- monetize(
  output_attribute = output_attribute,
  discount_shape = "exponential",
  discount_rate = 0.03,
  n_years = 5,
  valuation = 50000 # E.g. EURO
)

# Attributable COPD cases its monetized impact
results$monetization_main |>
  dplyr::select(impact, monetized_impact)
#> # A tibble: 1 × 2
#>   impact monetized_impact
#>    <dbl>            <dbl>
#> 1  3502.       151041149.
```
