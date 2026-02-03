# Attribute health impacts to an environmental stressor

This function calculates the attributable health impacts (mortality or
morbidity) due to exposure to an environmental stressor (air pollution
or noise), using either relative risk (**RR**) or absolute risk
(**AR**).

Arguments for both **RR & AR** pathways

- `approach_risk`

- `exp_central`, `exp_lower`, `exp_upper`

- `cutoff_central`, `cutoff_lower`, `cutoff_upper`

- `erf_eq_central`, `erf_eq_lower`, `erf_eq_upper`

Arguments only for **RR** pathways

- `rr_central`, `rr_lower`, `rr_upper`

- `rr_increment`

- `erf_shape`

- `bhd_central`, `bhd_lower`, `bhd_upper`

- `prop_pop_exp`

Argument for **AR** pathways

- `pop_exp`

**Optional** arguments for both **RR & AR** pathways

- `geo_id_micro`, `geo_id_macro`,

- `age_group`, `sex`, `info`, `population`

- `dw_central`, `dw_lower`, `dw_upper`

- `duration_central`, `duration_lower`, `duration_upper`

## Usage

``` r
attribute_health(
  approach_risk = "relative_risk",
  exp_central,
  exp_lower = NULL,
  exp_upper = NULL,
  cutoff_central = 0,
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
  prop_pop_exp = 1,
  geo_id_micro = "a",
  geo_id_macro = NULL,
  age_group = "all",
  sex = "all",
  dw_central = NULL,
  dw_lower = NULL,
  dw_upper = NULL,
  duration_central = NULL,
  duration_lower = NULL,
  duration_upper = NULL,
  info = NULL,
  population = NULL
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
  `attribute_health()`; needed for
  [`attribute_lifetable()`](https://swisstph.github.io/healthiar/reference/attribute_lifetable.md).*

- sex:

  `Numeric vector` or `string vector` specifying the **sex** of the
  groups considered in the assessment.*Optional argument.*

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

- info:

  `String`, `data frame` or `tibble` providing **information about the
  assessment**. See Details for more info. *Optional argument.*

- population:

  `Numeric vector` **`For attribute_lifetable()`**, it is an *obligatory
  argument* specifying the **mid-year populations** per age (i.e. age
  group size = 1 year) for the (first) year of analysis.
  **`For attribute_health()`** it is an *optional argument* which
  specifies the **population used to calculate attributable impacts
  rate** per 100 000 population. See Details for more info.

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

**What you put in is what you get out**

The health metric you put in (e.g. absolute disease cases, deaths per
100 000 population, DALYs, prevalence, incidence, ...) is the one you
get out.

*Exception*: if you enter a disability weight (via the `dw_...`
arguments) the attributable impact will be in YLD.

**Function arguments**

`exp_central`, `exp_lower`, `exp_upper` In case of exposure bands enter
only one exposure value per band (e.g. the means of the lower and upper
bounds of the exposure bands).

`cutoff_central`, `cutoff_lower`, `cutoff_upper` The cutoff level refers
to the exposure level below which no health effects occur in the same
unit as the exposure. If exposure categories are used, the length of
this input must be the same as in the `exp_...` argument(s).

`pop_exp` *Only applicable in AR pathways; always required.* In AR
pathways the population exposed per exposure category is multiplied with
the corresonding category-specific risk to obtain the absolute number of
people affected by the health outcome.

`erf_eq_central`, `erf_eq_lower`, `erf_eq_upper` *Required in AR
pathways; in RR pathways required only if rr\_... arguments not
specified.* Enter the exposure-response function as a `function`, e.g.
output from
[`stats::splinefun()`](https://rdrr.io/r/stats/splinefun.html) or
[`stats::approxfun()`](https://rdrr.io/r/stats/approxfun.html), or as a
`string` formula, e.g. `"3+c+c^2"` (with the *c* representing the
concentration/exposure). If you have x-axis (exposure) and y-axis
(relative risk) value pairs of multiple points lying on the the
exposure-response function, you could use e.g.
`stats::splinefun(x, y, method="natural")` to derive the
exposure-response function (in this example using a cubic spline natural
interpolation). `rr_increment` *Only applicable in RR pathways.*
Relative risks from the literature are valid for a specific increment in
the exposure, in case of air pollution often 10 or 5 \\\mu g/m^3\\).

`bhd_central`, `bhd_lower`, `bhd_upper` *Only applicable in RR
pathways.* Baseline health data for each exposure category must be
entered.

`prop_pop_exp` *Only applicable in RR pathways.* In RR pathways
indicates the fraction(s) (value(s) from 0 until and including 1) of the
total population exposed to the exposure categories. See equation of the
population attributable fraction for categorical exposure below.

`geo_id_macro`, `geo_id_micro` *Only applicable in assessments with
multiple geographic units.* For example, if you provide the names of the
municipalities under analysis to `geo_id_micro`, you might provide to
`geo_id_macro` the corresponding region / canton / province names.
Consequently, the vectors fed to `geo_id_micro` and `geo_id_macro` must
be of the same length.

`age_group` Can be either `numeric` or `character`. If it is numeric, it
refers to the first age of the age group. E.g. `c(0, 40, 80)` means age
groups `[0, 40), [40, 80), >=80]`.

`info` *Optional argument.* Information entered to this argument will be
added as column(s) names `info_1`, `info_2`, `info_...` to the results
table. These additional columns can be used to further stratify the
analysis in a secondary step (see example below).

`population` *Optional argument.* The population entered here is used to
determine impact rate per 100 000 population. Note the requirement for
the vector length in the paragraph *Assessment of multiple geographic
units* below.

`duration_central`, `duration_lower`, `duration_upper` *Only applicable
in assessments of YLD (years lived with disability).* Measured in years.
A value of 1 (year) refers to the prevalence-based approach, while
values above 1 to the incidence-based approach.

**Methodology**

Information about the methodology (including corresponding equations and
literature) is available in the package vignette. More specifically, see
chapters:

- [relative
  risk](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#relative-risk)

- [absolute
  risk](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#absolute-risk)

## References

WHO (2003). “Introduction and methods: Assessing the environmental
burden of disease at national and local levels.” World Health
Organization. <https://www.who.int/publications/i/item/9241546204>.

Steenland K, Armstrong B (2006). “An overview of methods for calculating
the burden of disease due to specific risk factors.” *Epidemiology*,
**17**(5), 512–519.
[doi:10.1097/01.ede.0000229155.05644.43](https://doi.org/10.1097/01.ede.0000229155.05644.43)
.

2019 Risk Factors Collaborators GBD (2020). “Global burden of 87 risk
factors in 204 countries and territories, 1990–2019.” *The Lancet*.
[doi:10.1016/S0140-6736(20)30752-2](https://doi.org/10.1016/S0140-6736%2820%2930752-2)
, <https://www.thelancet.com/article/S0140-6736(20)30752-2/fulltext>.

Soares J, González Ortiz A, Gsella A, Horálek J, Plass D, Kienzler S
(2022). “Health risk assessment of air pollution and the impact of the
new WHO guidelines (Eionet Report – ETC HE 2022/10).” European Topic
Centre on Human Health and the Environment.
<https://iris.who.int/server/api/core/bitstreams/3ebe7c55-be17-4ebe-89b9-8871fd287acd/content>.

Pozzer A, Anenberg SC, Dey S, Haines A, Lelieveld J, Chowdhury S (2023).
“Mortality Attributable to Ambient Air Pollution: A Review of Global
Estimates.” *GeoHealth*, **7**(1), e2022GH000711.
[doi:10.1029/2022GH000711](https://doi.org/10.1029/2022GH000711) ,
e2022GH000711 2022GH000711,
https://agupubs.onlinelibrary.wiley.com/doi/pdf/10.1029/2022GH000711,
<https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2022GH000711>.

Lehtomäki H, Aasvang GM, Sulo G, Denby BR, Hänninen OO, Brauer M,
Pereira G, Dadras O, Bølling AK (2025). “Burden of disease attributable
to PM2.5 at low exposure levels: impact of methodological choices.”
*Environmental Health*, **25**(1), 4. ISSN 1476-069X,
[doi:10.1186/s12940-025-01250-y](https://doi.org/10.1186/s12940-025-01250-y)
.

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
# Goal: attribute lung cancer cases to population-weighted PM2.5 exposure
# using relative risk

results <- attribute_health(
  erf_shape = "log_linear",
  rr_central = 1.369,            # Central relative risk estimate
  rr_increment = 10,             # per \mu g / m^3 increase in PM2.5 exposure
  exp_central = 8.85,            # Central exposure estimate in \mu g / m^3
  cutoff_central = 5,            # \mu g / m^3
  bhd_central = 30747            # Baseline health data: lung cancer incidence
 )

results$health_main$impact_rounded # Attributable cases
#> [1] 3502


# Goal: attribute cases of high annoyance to (road traffic) noise exposure
# using absolute risk

results <- attribute_health(
  approach_risk = "absolute_risk",
  exp_central = c(57.5, 62.5, 67.5, 72.5, 77.5),
  pop_exp = c(387500, 286000, 191800, 72200, 7700),
  erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
)

results$health_main$impact_rounded # Attributable high annoyance cases
#> [1] 174232


# Goal: attribute disease cases to PM2.5 exposure in multiple geographic
# units, such as municipalities, provinces, countries, ...

results <- attribute_health(
  geo_id_micro = c("Zurich", "Basel", "Geneva", "Ticino"),
  geo_id_macro = c("Ger","Ger","Fra","Ita"),
  rr_central = 1.369,
  rr_increment = 10,
  cutoff_central = 5,
  erf_shape = "log_linear",
  exp_central = c(11, 11, 10, 8),
  bhd_central = c(4000, 2500, 3000, 1500)
)

# Attributable cases (aggregated)
results$health_main$impact_rounded
#> [1] 1116  436  135

# Attributable cases (disaggregated)
results$health_detailed$results_raw |> dplyr::select(
  geo_id_micro,
  geo_id_macro,
  impact_rounded
)
#> # A tibble: 4 × 3
#>   geo_id_micro geo_id_macro impact_rounded
#>   <chr>        <chr>                 <dbl>
#> 1 Zurich       Ger                     687
#> 2 Basel        Ger                     429
#> 3 Geneva       Fra                     436
#> 4 Ticino       Ita                     135


# Goal: determine attributable YLD (years lived with disability)
results  <- attribute_health(
  exp_central = 8.85,
  prop_pop_exp = 1,
  cutoff_central = 5,
  bhd_central = 1000,
  rr_central = 1.1,
  rr_increment = 10,
  erf_shape = "log_linear",
  duration_central = 100,
  dw_central = 1,
  info = "pm2.5_yld"
)

results$health_main$impact
#> [1] 3602.934
```
