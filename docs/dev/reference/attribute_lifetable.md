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
  exp_central = NULL,
  exp_lower = NULL,
  exp_upper = NULL,
  cutoff_central = 0,
  cutoff_lower = NULL,
  cutoff_upper = NULL,
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
  [`attribute_health()`](https://swisstph.github.io/healthiar/dev/reference/attribute_health.md);
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

- time_horizon:

  `Numeric value` specifying the time horizon (number of years) for
  which the attributable YLL or premature deaths are to be considered.
  See Details for more info. *Optional argument.*

- exp_central, exp_lower, exp_upper:

  `Numeric value` or `numeric vector` specifying the **exposure
  level(s)** to the environmental stressor and (optionally) the
  corresponding lower and upper bound of the 95% confidence interval.
  See Details for more info.

- cutoff_central, cutoff_lower, cutoff_upper:

  `Numeric value` specifying the **exposure cut-off value** and
  (optionally) the corresponding lower and upper 95% confidence interval
  bounds. Default: 0. See Details for more info.

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
  `"linear_log"`, `"log_log"`. *Only applicable in RR pathways; not
  required if `erf_eq_...` argument(s) already specified.*

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

- `results_raw` (`tibble`) containing results for each combination of
  input uncertainty

- `results_by_geo_id_micro` (`tibble`) containing results for each
  geographic unit under analysis (specified in `geo_id_micro` argument)

- `results_by_year` (`tibble`) containing results by year

- `results_by_sex` (`tibble`) containing results by sex

- `results_by_age_group` (`tibble`) containing results by age group

- `intermediate_calculations` (`tibble`) containing intermediate
  results, among others population projections (for both the exposed and
  unexposed scenarios) and impact by age and year stored in nested
  `tibbles`

- `input_table` (`tibble`) containing the inputs to each relevant
  argument

- `input_args` (`list`) containing all the argument inputs used in the
  background

## Details

**Function arguments**

`age_group`

The numeric values must refer to 1 year age groups, e.g. `c(0:99)`. To
convert multi-year/larger age groups to 1 year age groups use the
function
[`prepare_lifetable()`](https://swisstph.github.io/healthiar/dev/reference/prepare_lifetable.md)
(see its function documentation for more info).

`bhd_central,bhd_lower,bhd_upper`

Deaths per age must be inputted with 1 value per age (i.e. age group
size = 1 year). There must be greater than or equal to 1 deaths per age
to avoid issues during the calculation of survival probabilities.

`population`

The population data must be inputted with 1 value per age (i.e. age
group size = 1 year). The values must be greater than or equal to 1 per
age to avoid issues during the calculation of survival probabilities.

Mid-year population of year x can be approximated as the mean of either
end-year populations of years x-1 and x or start-of-year populations of
years x and x+1. For each age, the inputted values must be greater than
or equal to 1 to avoid issues during the calculation of survival
probabilities.

`approach_newborns`

If `"with_newborns"` is selected, it is assumed that for each year after
the year of analysis n babies (population aged 0) are born.

`time_horizon`

Applicable for the following cases: \#'

- YLL: `single_year` or `constant` exposure

- premature deaths: `constant` exposure

For example, if 10 is entered one is interested in the impacts of
exposure during the year of analysis and the next 9 years (= 10 years in
total). Default value: length of the numeric vector specified in the
`age_group` argument.

`min_age`, `max_age` The `min_age` default value 30 implies that all
adults aged 30 or older will be affected by the exposure; `max_age`
analogeously specifies the age above which no health effects of the
exposure are considered.

**Conversion of multi-year to single year age groups**

To convert multi-year/larger age groups to 1 year age groups use the
function
[`prepare_lifetable()`](https://swisstph.github.io/healthiar/dev/reference/prepare_lifetable.md)
and see its function documentation for more info.

**Life table methodology**

The life table methodology of `attribute_lifetable()` follows that of
the WHO tool AirQ+, and is described in more detail by Miller & Hurley
(2003, https://doi.org/10.1136/jech.57.3.200).

In short, two scenarios are compared: 1) a scenario with the exposure
level specified in the function ("exposed scenario") and 2) a scenario
with no exposure ("unexposed scenario"). First, the entry and mid-year
populations of the (first) year of analysis in the unexposed scenario is
determined using modified survival probabilities. Second, age-specific
population projections using scenario-specific survival probabilities
are done for both scenarios. Third, by subtracting the populations in
the unexposed scenario from the populations in the exposed scenario the
premature deaths/years of life lost attributable to the exposure are
determined.

An expansive life table case study by Miller (2010) is available here:
https://cleanair.london/app/uploads/CAL-098-Mayors-health-study-report-June-2010-1.pdf.

***Determination of populations in the (first) year of analysis***

The entry (i.e. start of year) populations in both scenarios is
determined as follows: \$\$entry\\population\_{year_1} =
midyear\\population\_{year_1} + \frac{deaths\_{year_1}}{2}\$\$

***Exposed scenario*** The survival probabilities in the exposed
scenario from start of year i to start of year i+1 are calculated as
follows: \$\$prob\\survival = \frac{midyear\\population_i -
\frac{deaths_i}{2}}{midyear\\population_i + \frac{deaths_i}{2}}\$\$
Analogously, the probability of survival from start of year i to
mid-year i: \$\$prob\\survival\\until\\midyear = 1 - \frac{1 -
prob\\survival}{2}\$\$

***Unexposed scenario*** The survival probabilities in the unexposed
scenario are calculated as follows:

First, the age-group specific hazard rate in the exposed scenario is
calculated using the inputted age-specific mid-year populations and
deaths. \$\$hazard\\rate = \frac{deaths}{mid\\year\\population}\$\$
Second, the hazard rate is multiplied with the modification factor (\\=
1 - PAF\\) to obtain the age-specific hazard rate in the unexposed
scenario. \$\$hazard\\rate\\mod = hazard\\rate \times
modification\\factor\$\$ Third, the the age-specific survival
probabilities (from the start until the end in a given age group) in the
unexposed scenario are calculated as follows (cf. Miller & Hurley 2003):
\$\$prob\\survival\\mod =
\frac{2-hazard\\rate\\mod}{2+hazard\\rate\\mod}\$\$ Then the mid-year
populations of the (first) year of analysis (year_1) in the unexposed
scenario are determined as follows:

First, the survival probabilities from start of year i to mid-year i in
the unexposed scenario is calculated as:
\$\$prob\\survival\\until\\midyear\\{mod} = 1 - \frac{1 -
prob\\survival\\mod}{2}\$\$ Second, the mid-year populations of the
(first) year of analysis (year_1) in the unexposed scenario is
calculated: \$\$midyear\\population\\unexposed\_{year_1} =
entry\\population\_{year_1} \times
prob\\survival\\until\\midyear\_{mod}\$\$

***Population projection***

Using the age group-specific and scenario-specific survival
probabilities calculated above, future populations of each age-group
under each scenario are calculated.

***Unexposed scenario*** The entry and mid-year population projections
of in the exposed scenario is done as follows:

First, the entry population of year i+1 is calculated (which is the same
as the end of year population of year i) by multiplying the entry
population of year i and the modified survival probabilities.
\$\$entry\\population\_{i+1} = entry\\population_i \times
prob\\survival\\mod\$\$ Second, the mid-year population of year i+1 is
calculated. \$\$midyear\\population\_{i+1} = entry\\population\_{i+1}
\times prob\\survival\\until\\midyear\$\$

***Exposed scenario*** The population projections for the two possible
options of `approach_exposure` (`"single_year"` and `"constant"`) for
the unexposed scenario are different. In the case of `"single_year"`
exposure, the population projection for the years after the year of
exposure is the same as in the unexposed scenario.

In the case of `"constant"` the population projection is done as
follows:

First, the entry population of year i+1 is calculated (which is the same
as the end of year population of year i) using the entry population of
year i. \$\$entry\\population\_{i+1} = entry\\population_i \times
prob\\survival\$\$ Second, the mid-year population of year i+1 is
calculated. \$\$midyear\\population\_{i+1} = entry\\population\_{i+1}
\times prob\\survival\\until\\midyear\$\$

**Conversion of alternative risk measures to relative risks**

For conversion of hazard ratios and/or odds ratios to relative risks
refer to https://doi.org/10.1111/biom.13197 and/or use the conversion
tool for hazard ratios (https://ebm-helper.cn/en/Conv/HR_RR.html) and/or
odds ratios (https://ebm-helper.cn/en/Conv/OR_RR.html).

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
