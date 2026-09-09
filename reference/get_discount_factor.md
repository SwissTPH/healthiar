# Get discount factor

This function calculates the discount factor based on discount rate.

## Usage

``` r
get_discount_factor(discount_rate, n_years, discount_shape = "exponential")
```

## Arguments

- discount_rate:

  `Numeric value` showing the discount rate for future years.

- n_years:

  `Numeric value` or `numeric vector` specifying the number of years
  elapsed for which the discount factor is to be calculated. One factor
  is returned per entered value. The year 0, i.e. the present, gets a
  factor of 1 (no discounting). Note that this differs from the argument
  of the same name in
  [`monetize()`](https://swisstph.github.io/healthiar/reference/monetize.md),
  which is the time horizon:
  [`monetize()`](https://swisstph.github.io/healthiar/reference/monetize.md)
  calls this function with each single year from 0 to that horizon.

- discount_shape:

  `String` referring to the assumed equation for the discount factor. By
  default: `"exponential"`. Otherwise: `"hyperbolic_harvey_1986"` or
  `"hyperbolic_mazur_1987"`.

## Value

This function returns the `numeric` discount factor(s), one per value
entered in `n_years`.

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

## See also

- Alternative:
  [`monetize`](https://swisstph.github.io/healthiar/reference/monetize.md)

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: discount factor after a given number of years
get_discount_factor(
  discount_rate = 0.07,
  n_years = 5
 )
#> [1] 0.7129862

# Goal: discount factor for each year of a time horizon
get_discount_factor(
  discount_rate = 0.07,
  n_years = 0:5
 )
#> [1] 1.0000000 0.9345794 0.8734387 0.8162979 0.7628952 0.7129862

```
