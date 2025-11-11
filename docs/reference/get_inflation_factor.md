# Get inflation factor

This function calculates the inflation factor based on inflation rate.

## Usage

``` r
get_inflation_factor(n_years, inflation_rate = NULL)
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
  (increase of prices). Only to be entered if nominal (not real)
  discount rate is entered in the function. Default value = NULL
  (assuming no nominal discount rate).

## Value

This function returns the `numeric` inflation factor.

## Details

**Equation inflation factor (without discounting)**

\$\$inflation\\factor = (1 + inflation\\rate)^{n\\years}\$\$

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
