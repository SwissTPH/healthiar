# Get population attributable fraction

This function calculates the population attributable fraction (PAF) of a
health outcome due to exposure to an environmental stressor

## Usage

``` r
get_paf(rr_at_exp, prop_pop_exp)
```

## Arguments

- rr_at_exp:

  `Numerical value` Risk estimate of the concentration response function
  for a specific concentration. The population attributable fraction is
  normally calculated using the risk estimate that refers to the
  concentration that reflects the population exposure and the cut-off.
  This risk estimate is obtained after re-scaling from the
  epidemiological study with a particular increment (e.g. for PM2.5 10
  or 5 ug/m3) to the aimed concentration.

- prop_pop_exp:

  `Numeric value` or `numeric vector` specifying the **population
  fraction(s) exposed** for each exposure (category), i.e. the
  proportion of the **total** population that falls in each exposure
  category. Default: 1. The fractions do not have to add up to 1: the
  part of the population that they do not cover is treated as unexposed,
  i.e. it gets the relative risk of the reference level. In air
  pollution assessments everybody is usually exposed and the fractions
  add up to 1, whereas in noise assessments they typically add up to
  less than 1, because exposure is only reported above a given level.
  Accordingly, `bhd_...` must always refer to the total population. See
  Details for more info. *Only applicable in RR pathways.*

## Value

This function returns the population attributable fraction as a
`numeric value`.

## Details

**Methodology**

This function is called internally inside other `healthiar` functions,
e.g.
[`attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md).
The function calculates the population attributable fraction (i.e. the
percent of health cases that are attributable to the exposure) based on
the relative risk as described in the extensive existing literature (WHO
2003) .

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [relative
  risk](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#relative-risk)

## References

WHO (2003). “Introduction and methods: Assessing the environmental
burden of disease at national and local levels.” World Health
Organization. <https://www.who.int/publications/i/item/9241546204>.

## See also

- Alternative:
  [`attribute_health`](https://swisstph.github.io/healthiar/reference/attribute_health.md),
  [`attribute_lifetable`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md)

## Author

Alberto Castro & Axel Luyten

## Examples

``` r

# Goal: calculate PAF based on RR and the proportion of population exposed
get_paf(rr = 1.062, prop_pop_exp = 1)
#> [1] 0.05838041

```
