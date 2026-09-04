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
  threshold = NULL,
  exp
)
```

## Arguments

- erf_shape:

  `String value` specifying the **exposure-response function shape** to
  be assumed. Options (no default): `"linear"`, `log_linear`",
  `"linear_log"`, `"log_log"`. Input exposure values must be expressed
  in same unit as the increment of the relative risk. The re-scale of
  the relative risk is unbounded above and users are responsible for the
  plausible range. *Only applicable in RR pathways; not required if
  `erf_eq_...` argument(s) already specified.*

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

  `Numeric value` specifying the **exposure cut-off value**, i.e. the
  exposure level below which no health impacts are quantified. Default:
  0, or same value as `threshold`, if it is entered. If `cutoff` is
  higher than `threshold`, the exposure-response function is truncated
  at the cut-off value. Expressed in the same unit as the exposure. See
  the vignette chapter *Cut-off vs. threshold*.

- threshold:

  `Numeric value` specifying the **effect threshold**, i.e. the exposure
  level from which the exposure-response function starts to show an
  effect. It is the anchor of the curve and is therefore subtracted from
  the exposure. Default: same value as the cut-off. Expressed in the
  same unit as the exposure. See the vignette chapter *Cut-off vs.
  threshold*.

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

**Function arguments** `erf_eq` If the function is provided as `string`,
it can only contain the variable c (exposure), e.g. "3+c+c^2". If the
function is provided as a `function`, the object must be of the class
function. If only the values of the x-axis (exposure) and y axis
(relative risk) of the dots in the exposure-response function are
available, a cubic spline natural interpolation can be assumed to get
the function using, e.g., `stats::splinefun(x, y, method="natural")`

**Methodology**

This function is called internally inside other `healthiar` functions,
e.g.
[`attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md).
The function calculates the relative risk at the exposure level based on
the relative risk available in the epidemiological literature and the
assumed shape of the exposure-response function (Pozzer et al. 2023;
Lehtomäki et al. 2025) .

Detailed information about the methodology (including corresponding
equations and literature) is available in the package vignette. More
specifically, see chapters:

- [relative
  risk](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#relative-risk)

- [Cut-off vs.
  threshold](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#cutoff-vs-threshold)

## References

Lehtomäki H, Aasvang GM, Sulo G, Denby BR, Hänninen OO, Brauer M,
Pereira G, Dadras O, Bølling AK (2025). “Burden of disease attributable
to PM2.5 at low exposure levels: impact of methodological choices.”
*Environmental Health*, **25**(1), 4. ISSN 1476-069X.
[doi:10.1186/s12940-025-01250-y](https://doi.org/10.1186/s12940-025-01250-y)
.  
  
Pozzer A, Anenberg SC, Dey S, Haines A, Lelieveld J, Chowdhury S (2023).
“Mortality Attributable to Ambient Air Pollution: A Review of Global
Estimates.” *GeoHealth*, **7**(1), e2022GH000711.
[doi:10.1029/2022GH000711](https://doi.org/10.1029/2022GH000711) .
e2022GH000711 2022GH000711,
https://agupubs.onlinelibrary.wiley.com/doi/pdf/10.1029/2022GH000711,
<https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2022GH000711>.

## See also

- Alternative:
  [`attribute_health`](https://swisstph.github.io/healthiar/reference/attribute_health.md),
  [`attribute_lifetable`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md)

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

# Goal: scale relative risk to observed noise exposure levels assuming
# health effects above 45 dB (threshold) but exposure data only above 55 dB (cutoff)
get_risk(
  rr = 1.055,
  rr_increment = 10,
  erf_shape = "log_linear",
  exp = c(47, 52, 57, 62, 67, 72, 77),
  cutoff = 55,
  threshold = 45
)
#> [1] 1.000000 1.000000 1.066358 1.095290 1.125007 1.155531 1.186883

# Goal: determine the absolute risk for high annoyance at specific noise exposure levels
get_risk(
  erf_eq = "78.9270-3.1162*c+0.0342*c^2",
  exp = c(57.5, 62.5, 67.5, 72.5, 77.5)
)
#> [1] 12.81925 17.75825 24.40725 32.76625 42.83525

# Goal: attribute COPD cases to air pollution exposure
# by applying a user-defined exposure response function,
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
