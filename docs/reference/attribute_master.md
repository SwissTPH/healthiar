# Attributabe health impact to an environmental stressor

This INTERNAL function calculates the health impacts, mortality or
morbidity, of an environmental stressor using a single value for
baseline heath data, i.e. without life table.

## Usage

``` r
attribute_master(
  approach_risk = NULL,
  exp_central,
  exp_lower = NULL,
  exp_upper = NULL,
  cutoff_central = NULL,
  cutoff_lower = NULL,
  cutoff_upper = NULL,
  pop_exp = NULL,
  erf_eq_central = NULL,
  erf_eq_lower = NULL,
  erf_eq_upper = NULL,
  rr_central = NULL,
  rr_lower = NULL,
  rr_upper = NULL,
  rr_increment = NULL,
  erf_shape = NULL,
  bhd_central = NULL,
  bhd_lower = NULL,
  bhd_upper = NULL,
  prop_pop_exp = NULL,
  geo_id_micro = NULL,
  geo_id_macro = NULL,
  age_group = "all",
  sex = "all",
  population = NULL,
  info = NULL,
  dw_central = NULL,
  dw_lower = NULL,
  dw_upper = NULL,
  duration_central = NULL,
  duration_lower = NULL,
  duration_upper = NULL,
  is_lifetable = NULL,
  health_outcome = NULL,
  min_age = NULL,
  max_age = NULL,
  approach_newborns = NULL,
  approach_exposure = NULL,
  year_of_analysis = NULL,
  time_horizon = NULL,
  input_args = NULL
)
```

## Arguments

- approach_risk:

  `String value` specifying the **risk method**. Options:
  `"relative_risk"` (default) or `"absolute_risk"`.

- exp_central, exp_lower, exp_upper:

  `Numeric value` or `numeric vector` specifying the **exposure
  level(s)** to the environmental stressor and (optionally) the
  corresponding lower and upper bound of the 95% confidence interval.
  See Details for more info.

- cutoff_central, cutoff_lower, cutoff_upper:

  `Numeric value` specifying the **exposure cut-off value** and
  (optionally) the corresponding lower and upper 95% confidence interval
  bounds. Default: 0. See Details for more info.

- pop_exp:

  `Numeric vector` specifying the absolute size of the **population(s)
  exposed** to each exposure category. See Details for more info. *Only
  applicable in AR pathways; always required.*

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

- bhd_central, bhd_lower, bhd_upper:

  `Numeric value` or `numeric vector` providing the **baseline health
  data** of the health outcome of interest in the study population and
  (optionally) the corresponding lower bound and the upper 95%
  confidence interval bounds. See Details for more info. *Only
  applicable in RR pathways; always required.*

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

- age_group:

  `Numeric vector` or `string vector` providing the **age groups**
  considered in the assessment. In case of use in
  `attribute_lifetable)()`, it must be a `numeric` and contain single
  year age groups. See Details for more info. *Optional argument for
  [`attribute_health()`](https://github.com/SwissTPH/healthiar/reference/attribute_health.md);
  needed for
  [`attribute_lifetable()`](https://github.com/SwissTPH/healthiar/reference/attribute_lifetable.md).*

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

- dw_central, dw_lower, dw_upper:

  `Numeric value` or `numeric vector` providing the **disability
  weight** associated with the morbidity health outcome of interest and
  (optionally) the corresponding lower bound and the upper 95%
  confidence interval bounds. *Only applicable in assessments of YLD
  (years lived with disability).*

- duration_central, duration_lower, duration_upper:

  `Numeric value` or `numeric vector` providing the **duration**
  associated with the morbidity health outcome of interest in years and
  (optionally) the corresponding lower and upper bounds of the 95%
  confidence interval. Default: 1. See Details for more info. *Only
  applicable in assessments of YLD (years lived with disability).*

- is_lifetable:

  `Boolean` INTERNAL argument specifying if the life table approach is
  applied (TRUE) or not (FALSE)

- health_outcome:

  `String` specifying the desired result of the life table assessment.
  Options: `"deaths"` (premature deaths), `"yll"` (years of life lost).

- min_age, max_age:

  `Numberic value` specifying the minimum and maximum age for which the
  exposure will affect the exposed population, respectively. Default
  `min_age`: 30. Default `max_age`: none. See Details for more info.

- approach_newborns:

  `String` specifying whether newborns are to be considered in the years
  after the year of analysis or not. Options: `"without_newborns"`
  (default), `"with_newborns"`. See Details for more info.

- approach_exposure:

  `String` specifying whether exposure is constant or only in one year.
  Options: `"single_year"` (default), `"constant"`.

- year_of_analysis:

  `Numeric value` providing the first with exposure to the environmental
  stressor.

- time_horizon:

  `Numeric value` specifying the time horizon (number of years) for
  which the attributable YLL or premature deaths are to be considered.
  See Details for more info. *Optional argument.*

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

- `input_table` (`tibble`) containing the inputs to each relevant
  argument

- `input_args` (`list`) containing all the argument inputs used in the
  background

## Author

Alberto Castro & Axel Luyten
