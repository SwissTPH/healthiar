# Attribute premature deaths or YLL to an environmental stressor using a life table approach

This function assesses premature deaths or years of life lost (YLL)
attributable to exposure to an environmental stressor using a life table
approach.

## Usage

``` r
attribute_lifetable(
  age_group,
  sex,
  bhd_central,
  bhd_lower = NULL,
  bhd_upper = NULL,
  population,
  health_outcome = NULL,
  min_age = NULL,
  max_age = NULL,
  approach_exposure = "single_year",
  approach_newborns = "without_newborns",
  year_of_analysis,
  time_horizon = NULL,
  fraction_lived = 0.5,
  exp_central = NULL,
  exp_lower = NULL,
  exp_upper = NULL,
  cutoff_central = 0,
  cutoff_lower = NULL,
  cutoff_upper = NULL,
  threshold = NULL,
  erf_eq_central = NULL,
  erf_eq_lower = NULL,
  erf_eq_upper = NULL,
  rr_central = NULL,
  rr_lower = NULL,
  rr_upper = NULL,
  rr_increment = NULL,
  erf_shape = NULL,
  prop_pop_exp = 1,
  geo_id_micro = "a",
  geo_id_macro = NULL,
  info = NULL
)
```

## Arguments

- age_group:

  `Numeric vector` or `string vector` providing the **age groups**
  considered in the assessment. In case of use in
  `attribute_lifetable)()`, it must be a `numeric` and contain single
  year age groups. See Details for more info. *Optional argument for
  [`attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md);
  needed for `attribute_lifetable()`.*

- sex:

  `Numeric vector` or `string vector` specifying the **sex** of the
  groups considered in the assessment.*Optional argument.*

- bhd_central, bhd_lower, bhd_upper:

  `Numeric value` or `numeric vector` providing the **baseline health
  data** of the health outcome of interest in the study population and
  (optionally) the corresponding lower bound and the upper 95%
  confidence interval bounds. See Details for more info. *Only
  applicable in RR pathways; always required.*

- population:

  `Numeric vector` **`For attribute_lifetable()`**, it is an *obligatory
  argument* specifying the **mid-year populations** per age (i.e. age
  group size = 1 year) for the (first) year of analysis.
  **`For attribute_health()`** it is an *optional argument* which
  specifies the **population used to calculate attributable impacts
  rate** per 100 000 population. See Details for more info.

- health_outcome:

  `String` specifying the desired result of the life table assessment.
  Options: `"deaths"` (premature deaths), `"yll"` (years of life lost).

- min_age, max_age:

  `Numeric value` specifying the minimum and maximum age for which the
  exposure will affect the exposed population, respectively. See Details
  for more info.

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

- time_horizon:

  `Numeric value` specifying the time horizon (number of years) for
  which the attributable YLL or premature deaths are to be considered.
  See Details for more info. *Optional argument.*

- fraction_lived:

  `Numeric vector` Numeric vector or `single numeric scalar` referring
  to the average fraction of the age interval lived by individuals who
  die within that interval. Default is 0.5.

- exp_central, exp_lower, exp_upper:

  `Numeric value` or `numeric vector` specifying the **exposure
  level(s)** to the environmental stressor and (optionally) the
  corresponding lower and upper bound of the 95% confidence interval.
  See Details for more info.

- cutoff_central, cutoff_lower, cutoff_upper:

  `Numeric value` specifying the **exposure cut-off value**, i.e. the
  exposure level below which no health impacts are quantified, and
  (optionally) the corresponding lower and upper 95% confidence interval
  bounds. Default: 0, or same value as `threshold`, if it is entered. If
  `cutoff` is higher than `threshold`, the exposure-response function is
  truncated at the cut-off value. Expressed in the same unit as the
  exposure. See the vignette chapter *Cut-off vs. threshold*.

- threshold:

  `Numeric value` specifying the **effect threshold**, i.e. the exposure
  level from which the exposure-response function starts to show an
  effect. It is the anchor of the curve and is therefore subtracted from
  the exposure. Default: same value as the cut-off. Expressed in the
  same unit as the exposure. See the vignette chapter *Cut-off vs.
  threshold*.

- erf_eq_central, erf_eq_lower, erf_eq_upper:

  `String` or `function` specifying the **exposure-response function**
  and (optionally) the corresponding lower and upper 95% confidence
  interval functions. See Details for more info. *Required in AR
  pathways; in RR pathways required only if `rr_...` argument(s) not
  specified.*

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

- erf_shape:

  `String value` specifying the **exposure-response function shape** to
  be assumed. Options (no default): `"linear"`, `log_linear`",
  `"linear_log"`, `"log_log"`. Input exposure values must be expressed
  in same unit as the increment of the relative risk. The re-scale of
  the relative risk is unbounded above and users are responsible for the
  plausible range. *Only applicable in RR pathways; not required if
  `erf_eq_...` argument(s) already specified.*

- prop_pop_exp:

  `Numeric value` or `numeric vector` specifying the **population
  fraction(s) exposed** for each exposure (category). Default: 1. See
  Details for more info. *Only applicable in RR pathways.*

- geo_id_micro, geo_id_macro:

  `Numeric vector` or `string vector` providing **unique IDs of the
  geographic area** considered in the assessment (`geo_id_micro`) and
  (optionally) providing higher-level IDs (`geo_id_macro`) to aggregate
  the geographic areas at. See Details for more info. *Only applicable
  in assessments with multiple geographic units.*

- info:

  `String`, `data frame` or `tibble` providing **information about the
  assessment**. See Details for more info. *Optional argument.*

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

**Function arguments**

`age_group` The numeric values must refer to 1 year age groups, e.g.
`c(0:99)`. To convert multi-year/larger age groups to 1 year age groups
use the function
[`prepare_lifetable()`](https://swisstph.github.io/healthiar/reference/prepare_lifetable.md)
(see its function documentation for more info).

**Last age group** The life table is closed at the last age group, i.e.
its survivors are not projected into a further age. Therefore, the last
age group must be one in which essentially all remaining deaths occur,
as in most national life tables. Otherwise the attributable health
impacts are underestimated. If needed, condensate the last age group
(i.e. sum the populations and the deaths of the highest ages into it)
instead of adding age groups beyond the data. More information in the
vignette.

`bhd_central,bhd_lower,bhd_upper` Deaths per age must be inputted with 1
value per age (i.e. age group size = 1 year). There must be greater than
or equal to 1 deaths per age to avoid issues during the calculation of
survival probabilities. If zeros show up in the last ages (e.g. age 98 =
0 deaths, 99 years old = 1), please sum the values and condensate last
category (e.g. age 98 = 1).

`population` The population data must be inputted with 1 value per age
(i.e. age group size = 1 year). The values must be greater than or equal
to 1 per age to avoid issues during the calculation of survival
probabilities. Mid-year population of year x can be approximated as the
mean of either end-year populations of years x-1 and x or start-of-year
populations of years x and x+1. For each age, the inputted values must
be greater than or equal to 1 to avoid issues during the calculation of
survival probabilities.

`approach_newborns` If `"with_newborns"` is selected, it is assumed that
for each year after the year of analysis n babies (population aged 0)
are born.

`approach_exposure` Strategy for modeling exposure over time:

- `"single_year"`: Air pollution exposure is evaluated for a single year
  (year of analysis). Attributable premature deaths are calculated for
  this year only.

- `"constant"`: Air pollution exposure is sustained across the full
  projection horizon. Attributable premature deaths and YLLs are
  accumulated across all years within `time_horizon`.

`time_horizon` Applicable for the following cases:

- YLL: `single_year` or `constant` exposure

- premature deaths: `constant` exposure

For example, if 10 is entered one is interested in the impacts of
exposure during the year of analysis and the next 9 years (= 10 years in
total). Default value: the number of age groups entered in the
`age_group` argument.

`min_age`, `max_age` Both bounds are inclusive, e.g. `min_age = 30`
implies that all adults aged 30 or older will be affected by the
exposure and `max_age = 69` that no health effects are considered above
the age of 69. By default the exposure affects all age groups, i.e.
`min_age` is the youngest and `max_age` the oldest age group entered in
`age_group`.

`fraction_lived` is by default 0.5 for all age groups. However, in
low-mortality settings, infant deaths are heavily concentrated in the
first weeks of life. Therefore, `fraction_lived` can be lower e.g. 0.1
for the first age group.

**Methodology**

The life table approach to obtain YLL and deaths requires population and
baseline mortality data to be stratified by one year age groups. This
function applies the same approach as the on applied in the WHO tool
AirQ+ (WHO 2020) , which is described in previous literature (Miller and
Hurley 2003) .

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [YLL and deaths with life
  table](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#yll-deaths-with-life-table)

- [Cut-off vs.
  threshold](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#cutoff-vs-threshold)

**Conversion of multi-year to single year age groups**

To convert multi-year/larger age groups to 1 year age groups, use the
`healthiar` function
[`prepare_lifetable()`](https://swisstph.github.io/healthiar/reference/prepare_lifetable.md).

## Note

For this specific function, the return object `health_detailed` also
contains `intermediate_calculations`. This is a nested `tibble`
containing intermediate results, such as population projections and
impact by age/year.

## References

Miller BG, Hurley JF (2003). “Life table methods for quantitative impact
assessments in chronic mortality.” *Journal of Epidemiology and
Community Health*, **57**(3), 200–206. ISSN 0143-005X,
[doi:10.1136/jech.57.3.200](https://doi.org/10.1136/jech.57.3.200) .  
  
WHO (2020). “Health impact assessment of air pollution: AirQ+ life table
manual.” World Health Organization - Regional Office for Europe.
<https://www.who.int/europe/publications/i/item/WHO-EURO-2020-1559-41310-56212>.

## See also

- Upstream:
  [`prepare_exposure`](https://swisstph.github.io/healthiar/reference/prepare_exposure.md)
  (only if no exposure data available)

- Alternative:
  [`attribute_health`](https://swisstph.github.io/healthiar/reference/attribute_health.md),
  [`get_paf`](https://swisstph.github.io/healthiar/reference/get_paf.md),
  [`get_risk`](https://swisstph.github.io/healthiar/reference/get_risk.md)

- Downstream:
  [`attribute_mod`](https://swisstph.github.io/healthiar/reference/attribute_mod.md),
  [`compare`](https://swisstph.github.io/healthiar/reference/compare.md),
  [`daly`](https://swisstph.github.io/healthiar/reference/daly.md),
  [`multiexpose`](https://swisstph.github.io/healthiar/reference/multiexpose.md),
  [`standardize`](https://swisstph.github.io/healthiar/reference/standardize.md),
  [`monetize`](https://swisstph.github.io/healthiar/reference/monetize.md),
  [`socialize`](https://swisstph.github.io/healthiar/reference/socialize.md)

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: determine YLL attributable to air pollution exposure during one year
# using the life table approach
results <- attribute_lifetable(
  health_outcome = "yll",
  approach_exposure = "single_year",
  approach_newborns = "without_newborns",
  exp_central = 8.85,
  prop_pop_exp = 1,
  cutoff_central = 5,
  rr_central =  1.118,
  rr_increment = 10,
  erf_shape = "log_linear",
  age_group = exdat_lifetable$age_group,
  sex = exdat_lifetable$sex,
  bhd_central = exdat_lifetable$deaths,
  population = exdat_lifetable$midyear_population,
  year_of_analysis = 2019,
  min_age = 20
)
results$health_main$impact # Attributable YLL
#> [1] 28809.87


# Goal: determine attributable premature deaths due to air pollution exposure
# during one year using the life table approach
results_pm_deaths <- attribute_lifetable(
  health_outcome = "deaths",
  approach_exposure = "single_year",
  exp_central = 8.85,
  prop_pop_exp = 1,
  cutoff_central = 5,
  rr_central =  1.118,
  rr_increment = 10,
  erf_shape = "log_linear",
  age_group = exdat_lifetable$age_group,
  sex = exdat_lifetable$sex,
  bhd_central = exdat_lifetable$deaths,
  population = exdat_lifetable$midyear_population,
  year_of_analysis = 2019,
  min_age = 20
)
results_pm_deaths$health_main$impact # Attributable premature deaths
#> [1] 2599.366


# Goal: determine YLL attributable to air pollution exposure (exposure distribution)
# during one year using the life table approach
results <- attribute_lifetable(
  health_outcome = "yll",
  exp_central = rep(c(8, 9, 10), each = 100*2), # each = length of sex or age_group vector
  prop_pop_exp = rep(c(0.2, 0.3, 0.5), each = 100*2), # each = length of sex or age_group vector
  cutoff_central = 5,
  rr_central = 1.118,
  rr_lower = 1.06,
  rr_upper = 1.179,
  rr_increment = 10,
  erf_shape = "log_linear",
  age_group = rep(
    exdat_lifetable$age_group,
    times = 3), # times = number of exposure categories
  sex = rep(
    exdat_lifetable$sex,
    times = 3), # times = number of exposure categories
  population = rep(
    exdat_lifetable$midyear_population,
    times = 3), # times = number of exposure categories
  bhd_central = rep(
    exdat_lifetable$deaths,
    times = 3), # times = number of exposure categories
  year_of_analysis = 2019,
  min_age = 20
)
results$health_main$impact_rounded # Attributable YLL
#> [1] 32185 16849 47413

```
