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

**Methodology**

This function is called internally inside other `healthiar` functions,
e.g.
[`attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md).
The function calculates the population attributable fraction (i.e. the
percent of health cases that are attributable to the exposure) based on
the relative risk as described in the extensive existing literature (WHO
2003; Steenland and Armstrong 2006; Lehtomäki et al. 2025) .

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [relative
  risk](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#relative-risk)

## References

Lehtomäki H, Aasvang GM, Sulo G, Denby BR, Hänninen OO, Brauer M,
Pereira G, Dadras O, Bølling AK (2025). “Burden of disease attributable
to PM2.5 at low exposure levels: impact of methodological choices.”
*Environmental Health*, **25**(1), 4. ISSN 1476-069X,
[doi:10.1186/s12940-025-01250-y](https://doi.org/10.1186/s12940-025-01250-y)
.  
  
Steenland K, Armstrong B (2006). “An overview of methods for calculating
the burden of disease due to specific risk factors.” *Epidemiology*,
**17**(5), 512–519.
[doi:10.1097/01.ede.0000229155.05644.43](https://doi.org/10.1097/01.ede.0000229155.05644.43)
.  
  
WHO (2003). “Introduction and methods: Assessing the environmental
burden of disease at national and local levels.” World Health
Organization. <https://www.who.int/publications/i/item/9241546204>.

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: calculate PAF based on RR and the proportion of population exposed
get_paf(rr = 1.062, prop_pop_exp = 1)
#> [1] 0.05838041
```
