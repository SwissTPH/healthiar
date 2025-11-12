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
  fraction(s) exposed** for each exposure (category). Default: 1. See
  Details for more info. *Only applicable in RR pathways.*

## Value

This function returns the population attributable fraction as a
`numeric value`.

## Details

For more information about the equations used by `get_paf` please see
the function documentation of `attribute_health`.

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: calculate PAF based on RR and the proportion of population exposed
get_paf(rr = 1.062, prop_pop_exp = 1)
#> [1] 0.05838041
```
