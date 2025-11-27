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

  `Numeric value` or `numeric vector` specifying the **relative risk**
  estimate(s) and (optionally) the corresponding lower and upper 95%
  confidence interval bounds. Not required if the `erf_eq` argument is
  already specified.

- rr_increment:

  `Numeric value` specifying the **exposure increment** for which the
  provided relative risk is valid. See Details for more info. *Only
  applicable in RR pathways; not required if `erf_eq_...` argument(s)
  already specified.*

- erf_eq:

  `String` or `function` specifying the **exposure-response function**
  and (optionally) the corresponding lower and upper 95% confidence
  interval functions. See Details and Examples sections below.

- cutoff:

  `Numeric value` specifying the **exposure cut-off value** (i.e. the
  exposure level below which no health effects occur) and (optionally)
  the corresponding lower and upper 95% confidence interval bounds.

- exp:

  `Numeric value` or `numeric vector` specifying the **exposure
  level(s)** to the environmental stressor (e.g. annual
  population-weighted mean) and (optionally) the corresponding lower and
  upper bound of the 95% confidence interval.

## Value

This function returns the `numeric` risk value(s) at the specified
exposure level(s), referred to as *rr_at_exp* in the relative risk
equations above.

## Details

**Function arguments**

`erf_eq`

If the function is provided as `string`, it can only contain the
variable c (exposure), e.g. "3+c+c^2". If the function is provided as a
`function`, the object must be of the class function. If only the values
of the x-axis (exposure) and y axis (relative risk) of the dots in the
exposure-response function are available, a cubic spline natural
interpolation can be assumed to get the function using, e.g.,
`stats::splinefun(x, y, method="natural")`

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

# Goal: determine the absolute risk for high annoyance at specific noise exposure levels
get_risk(
  erf_eq = "78.9270-3.1162*c+0.0342*c^2",
  exp = c(57.5, 62.5, 67.5, 72.5, 77.5)
)
#> [1] 12.81925 17.75825 24.40725 32.76625 42.83525

# Goal: attribute COPD cases to air pollution exposure by applying a user-defined exposure response function,
# e.g. MR-BRT curves from Global Burden of Disease study.
get_risk(
  erf_eq = splinefun(
    x = c(0, 5, 10, 15, 20, 25, 30, 50, 70, 90, 110),
    y = c(1.00, 1.04, 1.08, 1.12, 1.16, 1.20, 1.23, 1.35, 1.45, 1.53, 1.60),
    method = "natural"),
  exp = c(8, 9, 10)
)
#> [1] 1.063984 1.071987 1.080000
```
