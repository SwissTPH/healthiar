# Get discount factor

This function calculates the discount factor based on discount rate. If
the argument `inflation_rate` is NULL (default), it is assumed that the
discount rate is already corrected for inflation). Otherwise (if a value
for `inflation_rate` is entered), the resulted discount factor is
adjusted for inflation.

## Usage

``` r
get_discount_factor(
  discount_rate,
  n_years,
  discount_shape = "exponential",
  inflation_rate = NULL
)
```

## Arguments

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

This function returns the `numeric` discount factor.

## Details

**Methodology**

This function is called inside
[`monetize()`](https://swisstph.github.io/healthiar/reference/monetize.md).

One of the following three discount shapes can be selected:

- Exponential (Frederick et al. 2002)

- Hyperbolic as Harvey (1986)

- Hyperbolic as Mazur (1987)

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [Monetization](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#monetization)

## References

Frederick S, Loewenstein G, O'Donoghue T (2002). “Time Discounting and
Time Preference: A Critical Review.” *Journal of Economic Literature*,
**40**(2), 351–401.
[doi:10.1257/002205102320161311](https://doi.org/10.1257/002205102320161311)
.  
  
Harvey CM (1986). “Value Functions for Infinite-Period Planning.”
*Management Science*, **32**(9), 1123–1139.
[doi:10.1287/mnsc.32.9.1123](https://doi.org/10.1287/mnsc.32.9.1123) .  
  
Mazur JE (1987). “An adjusting procedure for studying delayed
reinforcement.” In Commons ML, Mazur JE, Nevin JA, Rachlin H (eds.),
*Quantitative Analyses of Behavior: Volume V. The Effect of Delay and of
Intervening Events on Reinforcement Value*, 55–73. Lawrence Erlbaum
Associates, Hillsdale, NJ. ISBN 0-89859-800-1.

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
get_discount_factor(
  discount_rate = 0.07,
  n_years = 5
 )
#> [1] 0.7129862

```
