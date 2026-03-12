# Discount health impacts

This function calculates discounted health impacts (without valuation).

## Usage

``` r
discount(
  output_attribute = NULL,
  impact = NULL,
  discount_rate = NULL,
  n_years = NULL,
  discount_shape = NULL
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

- impact:

  `Numberic value` referring to the health impacts to be monetized
  (without attribute function). If a `Numberic vector` is entered
  multiple assessments (by year) will be carried out. Be aware that the
  value for year 0 (current) must be entered, while n_years does not
  include the year 0. Thus, length of impact = n_years + 1.

- discount_rate:

  `Numeric value` showing the discount rate for future years.

- n_years:

  `Numeric value` referring to number of years in the future to be
  considered in the discounting and/or inflation. Be aware that the year
  0 (without discounting/inflation, i.e. the present) is not be counted
  here. If a vector is entered in the argument impact, n_years does not
  need to be entered (length of impact = n_years + 1).

- discount_shape:

  `String` referring to the assumed equation for the discount factor. By
  default: `"exponential"`. Otherwise: `"hyperbolic_harvey_1986"` or
  `"hyperbolic_mazur_1987"`.

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

**Methodology**

This function applies a discount (Frederick et al. 2002) to attributable
health impacts into the future.

Burden of disease studies may be interested in calculating + discounted
health impacts over time, and these may also be used in economic
evaluation models, where benefits are not monetized. For this specific
purpose, this function is offered.

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [Monetization](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#monetization)

## References

Adummy A (2026). “Some keys from package healthiar are not avalable.”
Failed to insert reference with keys: Frederick2002_jel, Harvey1986_ms,
Mazur1987_book from package = 'healthiar'. Possible cause - missing
REFERENCES.bib in package 'healthiar' or 'healthiar' not installed.  
  
Frederick S, Loewenstein G, O'Donoghue T (2002). “Time Discounting and
Time Preference: A Critical Review.” *Journal of Economic Literature*,
**40**(2), 351–401.
[doi:10.1257/002205102320161311](https://doi.org/10.1257/002205102320161311)
.

## See also

- Upstream:
  [`attribute_health`](https://swisstph.github.io/healthiar/reference/attribute_health.md),
  [`attribute_health`](https://swisstph.github.io/healthiar/reference/attribute_health.md)

- Alternative:
  [`monetize`](https://swisstph.github.io/healthiar/reference/monetize.md)

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: discount attributable health impacts
results <- discount(
  impact = 20000,
  discount_shape = "exponential",
  discount_rate = 0.03,
  n_years = 20
)
results$monetization_main$monetized_impact
#> [1] 11073.52

```
