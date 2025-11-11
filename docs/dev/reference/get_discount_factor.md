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
  default: "exponential". Otherwise: "hyperbolic_harvey_1986" or
  "hyperbolic_mazur_1987".

- inflation_rate:

  `Numeric value` between 0 and 1 referring to the annual inflation
  (increase of prices). Only to be entered if nominal (not real)
  discount rate is entered in the function. Default value = NULL
  (assuming no nominal discount rate).

## Value

This function returns the `numeric` discount factor.

## Details

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
get_discount_factor(
  discount_rate = 0.07,
  n_years = 5
 )
#> [1] 0.7129862
```
