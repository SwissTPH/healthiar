# Convert multi-year life table to single year life table

This function determines populations and deaths by one year age groups.

## Usage

``` r
prepare_lifetable(age_group, population, bhd)
```

## Arguments

- age_group:

  `Numeric vector` referring to the first years of the age groups. E.g.
  c(0, 20, 40, 60) means \[0, 20), \[20, 40), \[40, 60), \[60, )

- population:

  `Numeric vector` referring to mid-year populations by age group.

- bhd:

  `Numeric vector` referring to the baseline health data (deaths) by age
  group.

## Value

This function returns a `tibble` containing the columns:

- `population_for_attribute` (`numeric`) containing population values
  for each age

- `bhd_for_attribute` (`numeric`) containing baseline health data values
  for each age

- and more columns containing input data or results

## Details

**Methodology**

The conversion follows the methodology of the WHO tool. See the AirQ+
manual "Health impact assessment of air pollution: AirQ+ life table
manual" for guidance on how to convert larger age groups to 1 year age
groups, section "Estimation of yearly values" (WHO 2020) .

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [YLL and deaths with life
  table](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#yll-deaths-with-life-table)

## References

WHO (2020). “Health impact assessment of air pollution: AirQ+ life table
manual.” World Health Organization - Regional Office for Europe.
<https://iris.who.int/server/api/core/bitstreams/3ebe7c55-be17-4ebe-89b9-8871fd287acd/content>.

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: Convert 5-year population and death data into single year life table
results <- prepare_lifetable(
  age_group = c(0, 5, 10, 15),
  population = c(3387900, 3401300, 3212300, 3026100),
  bhd = c(4727, 472, 557, 1323)
)
```
