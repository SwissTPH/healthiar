# Obtain age-standardized health impacts

This function obtains age-standardized health impacts based on multiple
age-group specific assessments

## Usage

``` r
standardize(output_attribute, age_group, ref_prop_pop = NULL)
```

## Arguments

- output_attribute:

  `List` containing the output of **one single**
  `healthiar::attribute_...()` call in which the age groups of interest
  were entered in the argument `age_group`, i.e. one assessment
  stratified by age group and **not** a list of age group-specific
  assessments.

- age_group:

  `String vector` with the age groups included in the age
  standardization. The vector refers to age-dependent data in this
  function and to `output_attribute` (if provided).

- ref_prop_pop:

  `Numeric vector` specifying with the reference proportion of
  population for each age group. If this argument is empty, the
  proportion of `population` by age group in the provided data will be
  used.

## Value

This function returns a `list` containing:

1\) `health_main` (`tibble`) containing the main results. The direct
method of standardization applies the age group-specific rates observed
in the study population to a reference population distribution. It
therefore standardizes **rates** and not counts:

- `impact_per_100k_inhab` and `bhd_per_100k_inhab` (`numeric` columns)
  are age-standardized, i.e. the age group-specific rates weighted with
  `ref_prop_pop`;

- `pop_fraction` (`numeric` column) is the ratio of these two
  age-standardized rates, i.e. the age-standardized attributable
  fraction;

- `impact`, `bhd` and `population` (`numeric` columns) are the crude
  totals across the age groups, and `exp` (`numeric` column) the
  population-weighted mean exposure. They are **not** standardized: a
  standardized count would require the absolute size of the reference
  population, while `ref_prop_pop` provides only its age distribution.

Note that all results are identical to the crude ones if `ref_prop_pop`
is not entered, because in that case the age distribution of the study
population itself is taken as reference.

2\) `health_detailed` (`tibble`) containing the results per age group,
including the interim columns of the standardization. The columns ending
in `_std` are the contribution of each age group and add up to the
corresponding column of `health_main`, i.e.
`sum(impact_per_100k_inhab_std)`, `sum(exp_std)` and
`sum(pop_fraction_std)`.

## Details

**Methodology**

This function applies the direct method of standardization, where the
age-specific rates observed in a study population are applied to a
standard (reference) population distribution.

For age standardization in health impact assessments, the World Health
Organization (Ahmad et al. 2001) and the Global Burden of Disease study
(GBD 2019 Demographics Collaborators 2020) provide the relevant
information on this topic.

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [Standardization](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#standardization)

This function works after running
[`attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md)
or
[`attribute_lifetable()`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md)
functions.

**Combination with compare()**

Age-standardized results cannot currently be compared with
[`compare()`](https://swisstph.github.io/healthiar/reference/compare.md).
[`compare()`](https://swisstph.github.io/healthiar/reference/compare.md)
reads the results by age group of each assessment, i.e.
`health_detailed$results_raw`, which `standardize()` passes on
unchanged: the standardization is in `health_main` and in
`health_detailed$impact_std_by_age_group`, so applying
[`compare()`](https://swisstph.github.io/healthiar/reference/compare.md)
to the output of `standardize()` gives exactly the same result as
applying it to the assessments themselves. The other way round does not
work either, because a comparison has one exposure and one population
per scenario and therefore none of the columns that `standardize()`
needs. To compare two scenarios in terms of age-standardized rates,
apply `standardize()` to each of them and compare the resulting
`health_main$impact_per_100k_inhab` directly.

## References

Ahmad OB, Pinto CB, Lopez AD, Murray CJ, Lozano R, Inoue M (2001). “Age
standardization of rates: a new WHO standard.” Technical Report GPE
Discussion Paper Series: No. 31, World Health Organization, Geneva.  
  
GBD 2019 Demographics Collaborators (2020). “Global age-sex-specific
fertility, mortality, healthy life expectancy (HALE), and population
estimates in 204 countries and territories, 1950-2019: a comprehensive
demographic analysis for the Global Burden of Disease Study 2019.” *The
Lancet*, **396**(10258), 1160-1203.
[doi:10.1016/S0140-6736(20)30977-6](https://doi.org/10.1016/S0140-6736%2820%2930977-6)
.

## See also

- Upstream:
  [`attribute_health`](https://swisstph.github.io/healthiar/reference/attribute_health.md),
  [`attribute_lifetable`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md)

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: age-standardize two age group-specific impacts
output_attribute <- attribute_health(
  rr_central = 1.063,
  rr_increment = 10,
  erf_shape = "log_linear",
  cutoff_central =  0,
  age_group = c("below_40", "above_40"),
  exp_central = c(8.1, 10.9),
  bhd_central = c(1000, 4000),
  population = c(100000, 500000)
)
results <- standardize(
  output_attribute = output_attribute,
  age_group = c("below_40", "above_40"),
  ref_prop_pop = c(0.5, 0.5)
)
results$health_detailed$results_raw$impact_per_100k_inhab # age group-specific impact rate
#> [1] 48.28250 51.53977
results$health_main$impact_per_100k_inhab # age-standardized impact rate
#> [1] 49.91113

```
