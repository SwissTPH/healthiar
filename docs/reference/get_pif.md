# Get population impact fraction (PIF)

This function calculates the population impact fraction of a health
outcome due to exposure to an environmental stressor

## Usage

``` r
get_pif(rr_at_exp_1, rr_at_exp_2, prop_pop_exp_1, prop_pop_exp_2)
```

## Arguments

- rr_at_exp_1:

  `Numerical value` showing the risk estimate of the concentration
  response function for a specific concentration in the scenario 1. The
  population attributable fraction is normally calculated using the risk
  estimate that refers to the concentration that reflects the population
  exposure and the cut-off. This risk estimate is obtained after
  re-scaling from the epidemiological study with a particular increment
  (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.

- rr_at_exp_2:

  `Numerical value` showing the risk estimate of the concentration
  response function for a specific concentration in the scenario 2. The
  population attributable fraction is normally calculated using the risk
  estimate that refers to the concentration that reflects the population
  exposure and the cut-off. This risk estimate is obtained after
  re-scaling from the epidemiological study with a particular increment
  (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.

- prop_pop_exp_1:

  `Numerical value` showing the fraction (\[0,1\]) of population exposed
  to the environmental stressor in the scenario 1. Per default = 1 (i.e.
  100% of population is exposed).

- prop_pop_exp_2:

  `Numerical value` showing the fraction (\[0,1\]) of population exposed
  to the environmental stressor in the scenario 1. Per default = 1 (i.e.
  100% of population is exposed).

## Value

This function returns the population impact fraction as a
`numeric value`.

## Details

**Methodology**

This function is internally used inside the `healthiar` function
[`compare()`](https://swisstph.github.io/healthiar/reference/compare.md).
The population impact fraction (PIF) is calculated as described in WHO
(2003) and Murray et al. (2003) .

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [comparison of two health
  scenarios](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#comparison-of-two-health-scenarios)

## References

Murray CJL, Ezzati M, Lopez AD, Rodgers A, Vander Hoorn S (2003).
“Comparative quantification of health risks conceptual framework and
methodological issues.” *Popul. Health Metr.*, **1**(1), 1.  
  
WHO (2003). “Introduction and methods: Assessing the environmental
burden of disease at national and local levels.” World Health
Organization. <https://www.who.int/publications/i/item/9241546204>.

## See also

- Alternative:
  [`compare`](https://swisstph.github.io/healthiar/reference/compare.md)

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: calculate the population impact fraction (PIF)
results <- get_pif(
  rr_at_exp_1 = 1.043879,
  rr_at_exp_2 = 1.011217,
  prop_pop_exp_1 = 1,
  prop_pop_exp_2 = 1
)
print(results)
#> [1] 0.03128907

```
