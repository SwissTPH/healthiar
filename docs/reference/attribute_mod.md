# Create a scenario 2 by modifying an existing scenario 1 and determine attributable health impacts in it

This function quantifies the attributable health impacts in a new
scenario 2 as follows:

- taking the input data of an existing scenario 1 (obtained using
  [`attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md)),

- modifying some of these input data of scenario 1 for the scenario 2
  and

- calling in the background
  [`attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md)
  with the new data for scenario 2

## Usage

``` r
attribute_mod(
  output_attribute,
  erf_shape = NULL,
  rr_central = NULL,
  rr_lower = NULL,
  rr_upper = NULL,
  rr_increment = NULL,
  erf_eq_central = NULL,
  erf_eq_lower = NULL,
  erf_eq_upper = NULL,
  exp_central = NULL,
  exp_lower = NULL,
  exp_upper = NULL,
  prop_pop_exp = NULL,
  pop_exp = NULL,
  cutoff_central = NULL,
  cutoff_lower = NULL,
  cutoff_upper = NULL,
  bhd_central = NULL,
  bhd_lower = NULL,
  bhd_upper = NULL,
  geo_id_micro = NULL,
  geo_id_macro = NULL,
  age_group = NULL,
  sex = NULL,
  population = NULL,
  info = NULL,
  min_age = NULL,
  max_age = NULL,
  approach_exposure = NULL,
  approach_newborns = NULL,
  year_of_analysis = NULL
)
```

## Arguments

- output_attribute:

  `List` containing the output of the function attribute() for scenario
  1.

- erf_shape:

  `String value` specifying the **exposure-response function shape** to
  be assumed. Options (no default): `"linear"`, `log_linear`",
  `"linear_log"`, `"log_log"`. *Only applicable in RR pathways; not
  required if `erf_eq_...` argument(s) already specified.*

- rr_central, rr_lower, rr_upper:

  `Numeric value` specifying the **central relative risk** estimate and
  (optionally) the corresponding lower and upper 95% confidence interval
  bounds. *Only applicable in RR pathways; not required if `erf_eq_...`
  argument(s) already specified.*

- rr_increment:

  `Numeric value` specifying the **exposure increment** for which the
  provided relative risk is valid. See Details for more info. *Only
  applicable in RR pathways; not required if `erf_eq_...` argument(s)
  already specified.*

- erf_eq_central, erf_eq_lower, erf_eq_upper:

  `String` or `function` specifying the **exposure-response function**
  and (optionally) the corresponding lower and upper 95% confidence
  interval functions. See Details for more info. *Required in AR
  pathways; in RR pathways required only if `rr_...` argument(s) not
  specified.*

- exp_central, exp_lower, exp_upper:

  `Numeric value` or `numeric vector` specifying the **exposure
  level(s)** to the environmental stressor and (optionally) the
  corresponding lower and upper bound of the 95% confidence interval.
  See Details for more info.

- prop_pop_exp:

  `Numeric value` or `numeric vector` specifying the **population
  fraction(s) exposed** for each exposure (category). Default: 1. See
  Details for more info. *Only applicable in RR pathways.*

- pop_exp:

  `Numeric vector` specifying the absolute size of the **population(s)
  exposed** to each exposure category. See Details for more info. *Only
  applicable in AR pathways; always required.*

- cutoff_central, cutoff_lower, cutoff_upper:

  `Numeric value` specifying the **exposure cut-off value** and
  (optionally) the corresponding lower and upper 95% confidence interval
  bounds. Default: 0. See Details for more info.

- bhd_central, bhd_lower, bhd_upper:

  `Numeric value` or `numeric vector` providing the **baseline health
  data** of the health outcome of interest in the study population and
  (optionally) the corresponding lower bound and the upper 95%
  confidence interval bounds. See Details for more info. *Only
  applicable in RR pathways; always required.*

- geo_id_micro, geo_id_macro:

  `Numeric vector` or `string vector` providing **unique IDs of the
  geographic area** considered in the assessment (`geo_id_micro`) and
  (optionally) providing higher-level IDs (`geo_id_macro`) to aggregate
  the geographic areas at. See Details for more info. *Only applicable
  in assessments with multiple geographic units.*

- age_group:

  `Numeric vector` or `string vector` providing the **age groups**
  considered in the assessment. In case of use in
  `attribute_lifetable)()`, it must be a `numeric` and contain single
  year age groups. See Details for more info. *Optional argument for
  [`attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md);
  needed for
  [`attribute_lifetable()`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md).*

- sex:

  `Numeric vector` or `string vector` specifying the **sex** of the
  groups considered in the assessment.*Optional argument.*

- population:

  `Numeric vector` **`For attribute_lifetable()`**, it is an *obligatory
  argument* specifying the **mid-year populations** per age (i.e. age
  group size = 1 year) for the (first) year of analysis.
  **`For attribute_health()`** it is an *optional argument* which
  specifies the **population used to calculate attributable impacts
  rate** per 100 000 population. See Details for more info.

- info:

  `String`, `data frame` or `tibble` providing **information about the
  assessment**. See Details for more info. *Optional argument.*

- min_age, max_age:

  `Numberic value` specifying the minimum and maximum age for which the
  exposure will affect the exposed population, respectively. Default
  `min_age`: 30. Default `max_age`: none. See Details for more info.

- approach_exposure:

  `String` specifying whether exposure is constant or only in one year.
  Options: `"single_year"` (default), `"constant"`.

- approach_newborns:

  `String` specifying whether newborns are to be considered in the years
  after the year of analysis or not. Options: `"without_newborns"`
  (default), `"with_newborns"`. See Details for more info.

- year_of_analysis:

  `Numeric value` providing the first with exposure to the environmental
  stressor.

## Value

This function returns a `list` containing:

1\) `health_main` (`tibble`) containing the main results;

- `impact` (`numeric` column) attributable health burden/impact

- `pop_fraction` (`numeric` column) population attributable fraction;
  only applicable in relative risk assessments

- And many more

2\) `health_detailed` (`list`) containing detailed (and interim)
results.

- `input_args` (`list`) containing all the argument inputs used in the
  background

- `input_table` (`tibble`) containing the inputs after preparation

- `results_raw` (`tibble`) containing results for all combinations of
  input (geo units, uncertainty, age and sex specific data...)

- `results_by_...` (`tibble`) containing results stratified by each
  geographic unit, age or sex.

## Details

**Methodology**

This function calls in the background
[`attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md),
which can use the relative risk (Murray et al. 2003) and absolute risk
(WHO 2011) .

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [Modification of
  scenarios](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#modification-of-scenarios)

- [Relative
  risk](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#relative-risk)

- [Absolute
  risk](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#absolute-risk)

## References

Murray CJ, Ezzati M, Lopez AD, Rodgers A, Hoorn SV (2003). “Comparative
risk assessment: conceptual framework and design.” *Epidemiology*,
**14**(4), 447-458.
[doi:10.1186/1478-7954-1-1](https://doi.org/10.1186/1478-7954-1-1) .  
  
WHO (2011). “Burden of disease from environmental noise: Quantification
of healthy life years lost in Europe.” World Health Organization.
<https://www.who.int/publications/i/item/burden-of-disease-from-environmental-noise-quantification-of-healthy-life-years-lost-in-europe>.

## See also

- Upstream:
  [`attribute_health`](https://swisstph.github.io/healthiar/reference/attribute_health.md),
  [`attribute_lifetable`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md)

- Downstream:
  [`compare`](https://swisstph.github.io/healthiar/reference/compare.md)

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: adjust an existing healthiar scenario and determine the health
# impacts in the modified scenario

## First create a scenario to be modified
scenario_A <- attribute_health(
  exp_central = 8.85,   # EXPOSURE 1
  cutoff_central = 5,
  bhd_central = 25000,
  approach_risk = "relative_risk",
  erf_shape = "log_linear",
  rr_central = 1.118,
  rr_increment = 10
)

scenario_A$health_main$impact # Attributable impact in scenario A
#> [1] 1050.86

## Modify scenario (adjust exposure value)
scenario_B <- attribute_mod(
  output_attribute = scenario_A,
  exp_central = 6       # EXPOSURE 2
)

scenario_B$health_main$impact # Attributable impact in scenario B
#> [1] 277.304

```
