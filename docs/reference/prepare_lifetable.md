# Convert multi-year life table to single year life table

This function determines populations and deaths by one year age groups.

## Usage

``` r
prepare_lifetable(age_group, population, bhd, fraction_lived = 0.5)
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

- fraction_lived:

  `Numeric vector` Numeric vector or `single numeric scalar` referring
  to the average fraction of the age interval lived by individuals who
  die within that interval. Default is 0.5.

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

`fraction_lived` is by default 0.5 for all age groups. However, in
low-mortality settings, infant deaths are heavily concentrated in the
first weeks of life. Therefore, `fraction_lived` can be lower e.g. 0.1
for the first age group.

When `fraction_lived[1] < 0.5`, the function reallocates the baseline
health data (`bhd`) for the first age group \\\[0, 5)\\ such that
`bhd[1] * (1 - fraction_lived[1])` deaths are assigned to single-year
age 0, and the remaining deaths are split equally across single-year
ages 1–4.

**Last age group**

[`attribute_lifetable()`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md)
closes the life table at the last age group. The age groups entered here
should therefore reach an age in which essentially all remaining deaths
occur. See more information in the vignette.

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [YLL and deaths with life
  table](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#yll-deaths-with-life-table)

## References

WHO (2020). “Health impact assessment of air pollution: AirQ+ life table
manual.” World Health Organization - Regional Office for Europe.
<https://www.who.int/europe/publications/i/item/WHO-EURO-2020-1559-41310-56212>.

## See also

- Downstream:
  [`attribute_lifetable`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md)

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
