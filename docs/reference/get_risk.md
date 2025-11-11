# Get the relative risk of an exposure level

This function re-scales the relative risk from the increment value in
the epidemiological study (e.g. for PM2.5 10 or 5 ug/m3) to the actual
population exposure

## Usage

``` r
get_risk(
  erf_shape = NULL,
  rr = NULL,
  rr_increment = NULL,
  erf_eq = NULL,
  cutoff = 0,
  exp
)
```

## Arguments

- erf_shape:

  `String value` specifying the **exposure-response function shape** to
  be assumed. Options (no default): `"linear"`, `log_linear`",
  `"linear_log"`, `"log_log"`. *Only applicable in RR pathways; not
  required if `erf_eq_...` argument(s) already specified.*

- rr:

  `Numeric vector` containing the relative risk. The data frame must
  contain the central estimate as well as the lower and upper bound of
  the exposure-response function.

- rr_increment:

  `Numeric value` specifying the **exposure increment** for which the
  provided relative risk is valid. See Details for more info. *Only
  applicable in RR pathways; not required if `erf_eq_...` argument(s)
  already specified.*

- erf_eq:

  Equation of the user-defined exposure-response function that puts the
  relative risk (y) in relation with exposure (x). If the function is
  provided as `string`, it can only contains one variable: x (exposure).
  E.g. "3+x+x^2". If the function is provided as a `function`, the
  object should have a function class. If only the values of the x-axis
  (exposure) and y axis (relative risk) of the dots in the
  exposure-response function are available, a cubic spline natural
  interpolation can be assumed to get the function using, e.g.,
  `stats::splinefun(x, y, method="natural")`

- cutoff:

  `Numeric value` showing the cut-off exposure level in ug/m3 (i.e. the
  exposure level below which no health effects occur).

- exp:

  Population exposure to the stressor (e.g. annual population-weighted
  mean).

## Value

This function returns the `numeric` relative risk(s) at the specified
exposure level(s), referred to as *rr_at_exp* in the equations above.

## Details

**Equations for scaling of relative risk**

*linear ERF* \$\$rr\\at\\exp = 1 + \frac{(rr - 1)}{rr\\increment} \cdot
(exp - cutoff)\$\$

*log-linear ERF*

\$\$rr\\at\\exp = e^{\frac{\log(\mathrm{rr})}{\mathrm{rr\\increment}}
\cdot (\mathrm{exp} - \mathrm{cutoff})}\$\$

*log-log ERF*

\$\$rr\\at\\exp = (\frac{exp + 1}{cutoff +
1})^{\frac{\log(\mathrm{rr})}{\log(\mathrm{rr\\increment + cutoff +
1}) - \log(cutoff + 1)}}\$\$

*linear-log ERF*

\$\$rr\\at\\exp = 1 + \frac{\log(\mathrm{rr -
1})}{\log(\mathrm{rr\\increment + cutoff + 1}) - \log(cutoff + 1)} \cdot
\frac{\log(exp + 1)}{\log(cutoff + 1)}\$\$

**Sources**

For the log-linear, log-log and linear-log exposure-response function
equations see Pozzer et al. 2022 (https://doi.org/10.1029/2022GH000711).

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: scale relative risk to observed exposure level
get_risk(
  rr = 1.05,
  rr_increment = 10,
  erf_shape = "linear",
  exp = 10,
  cutoff = 5
)
#> [1] 1.025
```
