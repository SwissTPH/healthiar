# Get inflation factor

This function calculates the inflation factor based on inflation rate.

## Usage

``` r
get_inflation_factor(n_years, inflation_rate = NULL, is_deflation = FALSE)
```

## Arguments

- n_years:

  `Numeric value` referring to number of years in the future to be
  considered in the discounting and/or inflation. Be aware that the year
  0 (without discounting/inflation, i.e. the present) is not be counted
  here. If a vector is entered in the argument impact, n_years does not
  need to be entered (length of impact = n_years + 1).

- inflation_rate:

  `Numeric value` between 0 and 1 referring to the annual inflation
  (increase of prices). This value is used to adjust monetization for
  inflation (converting nominal into real values by appyling a
  deflator). If this adjustment for inflation is not needed leave this
  argument empty (default value = NULL).

- is_deflation:

  `Boolean value` (TRUE vs. FALSE) referring to the type of inflation
  factor. FALSE (default) means inflate present values to future nominal
  values, while TRUE means deflate future nominal values to present real
  values

## Value

This function returns the `numeric` inflation factor.

## Details

**Methodology**

This function is called inside
[`monetize()`](https://swisstph.github.io/healthiar/reference/monetize.md).

It calculates the inflation factor based on the inflation rate and the
number of years into the future as described in Brealey et al. (2023) .

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [Monetization](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#monetization)

## References

Brealey RA, Myers SC, Allen F, Benninga S, Read J (2023). *Principles of
Corporate Finance*, 14th edition. McGraw-Hill Education, New York, NY.
ISBN 978-1264117464.

## See also

- Alternative:
  [`monetize`](https://swisstph.github.io/healthiar/reference/monetize.md)

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
get_inflation_factor(
  inflation_rate = 0.02,
  n_years = 5
)
#> [1] 1.104081

```
