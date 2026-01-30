# Discount health impacts

This function calculates discounted health impacts (without valuation).

## Usage

``` r
discount(
  output_attribute = NULL,
  impact = NULL,
  discount_rate = NULL,
  n_years = NULL,
  discount_shape = NULL,
  inflation_rate = NULL
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

- discount_rate:

  `Numeric value` showing the discount rate for future years. If it is a
  nominal discount rate, no inflation is to be entered. If it is a real
  discount rate, the result can be adjusted by entering inflation in
  this function.

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

- inflation_rate:

  `Numeric value` between 0 and 1 referring to the annual inflation
  (increase of prices). Only to be entered if nominal (not real)
  discount rate is entered in the function. Default value = NULL
  (assuming no nominal discount rate).

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
