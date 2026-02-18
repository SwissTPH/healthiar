# Get population attributable or impact fraction

This function calculates the population attributable fraction or
potential impact fraction of a health outcome due to exposure to an
environmental stressor

## Usage

``` r
get_pop_fraction(rr_at_exp_1, rr_at_exp_2, prop_pop_exp_1, prop_pop_exp_2)
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

This function returns a `value` corresponding to the population
attributable fraction

## Details

For more information about the equations used please see the function
documentation of `attribute_health`.

## Author

Alberto Castro & Axel Luyten
