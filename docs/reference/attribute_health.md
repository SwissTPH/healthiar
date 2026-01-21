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

- `results_raw` (`tibble`) containing results for each combination of
  input uncertainty

- `results_by_geo_id_micro` (`tibble`) containing results for each
  geographic unit under analysis (specified in `geo_id_micro` argument)

- `input_table` (`tibble`) containing the inputs to each relevant
  argument

- `input_args` (`list`) containing all the argument inputs used in the
  background

## Details

**What you put in is what you get out**

The health metric you put in (e.g. absolute disease cases, deaths per
100 000 population, DALYs, prevalence, incidence, ...) is the one you
get out.

*Exception*: if you enter a disability weight (via the `dw_...`
arguments) the attributable impact will be in YLD.

**Function arguments**

`exp_central`, `exp_lower`, `exp_upper`

In case of exposure bands enter only one exposure value per band (e.g.
the means of the lower and upper bounds of the exposure bands).

`cutoff_central`, `cutoff_lower`, `cutoff_upper`

The cutoff level refers to the exposure level below which no health
effects occur in the same unit as the exposure. If exposure categories
are used, the length of this input must be the same as in the `exp_...`
argument(s).

`pop_exp`

*Only applicable in AR pathways; always required.* In AR pathways the
population exposed per exposure category is multiplied with the
corresonding category-specific risk to obtain the absolute number of
people affected by the health outcome.

`erf_eq_central`, `erf_eq_lower`, `erf_eq_upper`

*Required in AR pathways; in RR pathways required only if rr\_...
arguments not specified.* Enter the exposure-response function as a
`function`, e.g. output from
[`stats::splinefun()`](https://rdrr.io/r/stats/splinefun.html) or
[`stats::approxfun()`](https://rdrr.io/r/stats/approxfun.html), or as a
`string` formula, e.g. `"3+c+c^2"` (with the *c* representing the
concentration/exposure).

If you have x-axis (exposure) and y-axis (relative risk) value pairs of
multiple points lying on the the exposure-response function, you could
use e.g. `stats::splinefun(x, y, method="natural")` to derive the
exposure-response function (in this example using a cubic spline natural
interpolation).

`rr_increment`

*Only applicable in RR pathways.* Relative risks from the literature are
valid for a specific increment in the exposure, in case of air pollution
often 10 or 5 \\\mu g/m^3\\).

`bhd_central`, `bhd_lower`, `bhd_upper`

*Only applicable in RR pathways.* Baseline health data for each exposure
category must be entered.

`prop_pop_exp`

*Only applicable in RR pathways.* In RR pathways indicates the
fraction(s) (value(s) from 0 until and including 1) of the total
population exposed to the exposure categories. See equation of the
population attributable fraction for categorical exposure below.

`geo_id_macro`, `geo_id_micro`

*Only applicable in assessments with multiple geographic units.* For
example, if you provide the names of the municipalities under analysis
to `geo_id_micro`, you might provide to `geo_id_macro` the corresponding
region / canton / province names. Consequently, the vectors fed to
`geo_id_micro` and `geo_id_macro` must be of the same length.

`age_group`

Can be either `numeric` or `character`. If it is numeric, it refers to
the first age of the age group. E.g. `c(0, 40, 80)` means age groups
`[0, 40), [40, 80), >=80]`.

`info`

*Optional argument.* Information entered to this argument will be added
as column(s) names `info_1`, `info_2`, `info_...` to the results table.
These additional columns can be used to further stratify the analysis in
a secondary step (see example below).

`population`

*Optional argument.* The population entered here is used to determine
impact rate per 100 000 population. Note the requirement for the vector
length in the paragraph *Assessment of multiple geographic units* below.

`duration_central`, `duration_lower`, `duration_upper`

*Only applicable in assessments of YLD (years lived with disability).*
If the value of `duration_central` is 1 year, it refers to the
prevalence-based approach, while a value above 1 year to the
incidence-based approach (Kim et al. 2022,
https://doi.org/10.3961/jpmph.21.597).

**Assessment of multiple geographic units**

To assess the attributable health impact/burden across multiple
geographic units with `attribute_health()`, you must specify the
argument `geo_id_micro` and (optionally) `geo_id_macro`, in addition to
the other required function arguments.

The length of the input vectors to the function arguments must be:

\$\$\text{length input vectors} = \text{number of geo units} \times
\text{number of exposure categories}\$\$

\$\$( \times \text{number of age groups (if entered)} \times
\text{number of sex groups (if entered) )}\$\$

I.e. there must be one line / observation for each specific combination
of geo unit, exposure category, age and sex group.

Alternatively, for those arguments that are independent of location
(e.g. `approach_risk`, `rr_...`, `erf_shape`, ...), you can enter a
single value, which will be recycled to match the length of the other
geo unit-specific input data. Additional categories can be passed on via
the `info` argument.

**Equations (relative risk)**

The most general equation describing the population attributable
fraction (PAF) mathematically is an integral form (GBD 2019 Risk Factors
Collaborators 2020, https://doi.org/10.1016/S0140-6736(20)30752-2):
\$\$PAF = \frac{\int RR(x)PE(x)dx - 1}{\int RR(x)PE(x)dx}\$\$

Where:

\\x\\ = exposure level

\\PE(x)\\ = population distribution of exposure

\\RR(x)\\ = relative risk at exposure level compared to the reference
level

If the population exposure is described as a categorical rather than
continuous exposure, the integrals in this equation may be converted to
sums, resulting in the following equation for the PAF (WHO 2003a,
https://www.who.int/publications/i/item/9241546204; WHO 2011,
https://iris.who.int/items/723ab97c-5c33-4e3b-8df1-744aa5bc1c27):
\$\$PAF = \frac{\sum RR_i \times PE_i - 1}{\sum RR_i \times PE_i}\$\$

Where:

\\i\\ = is the exposure category (e.g. in bins of 1 \\\mu g/m^3\\ PM2.5
or 5 dB noise exposure)

\\PE_i\\ = fraction of population in exposure category i

\\RR_i\\ = relative risk associated with the mean exposure level in
exposure category i compared to the reference level

There is one alternative for the PAF for categorical exposure
distribution that is commonly used, which is mathematically equivalent
to the equation right above, meaning that numerical estimates based on
these equations are identical (WHO 2003b,
https://doi.org/10.1186/1478-7954-1-1; WHO 2011,
https://iris.who.int/items/723ab97c-5c33-4e3b-8df1-744aa5bc1c27):
\$\$PAF = \frac{\sum PE_i(RR_i - 1)}{\sum PE_i(RR_i - 1) + 1}\$\$

Where:

\\i\\ = is the exposure category (e.g. in bins of 1 \\\mu g/m^3\\ PM2.5
or 5 dB noise exposure)

\\PE_i\\ = fraction of population in exposure category i

\\RR_i\\ = relative risk associated with the mean exposure level in
exposure category i compared to the reference level

Finally, if the exposure is provided as the population weighted mean
concentration (PWC), the equation for the PAF is reduced to (ETC HE
2022, https://www.eionet.europa.eu/etcs/all-etc-reports: \$\$PAF =
\frac{RR\_{PWC} - 1}{RR\_{PWC}}\$\$ Where \\RR\_{PWC}\\ is the relative
risk associated with the population weighted mean exposure.

**Equation (absolute risk)** \$\$N = \sum AR_i\times PE_i\$\$

Where:

\\N\\ = the number of cases of the exposure-specific health outcome that
are attributed to the exposure

\\AR_i\\ = absolute risk associated with the mean exposure level of
exposure category i

\\PE_i\\ = population exposed (absolute number) to exposure levels of
exposure category i

**Conversion of alternative risk measures to relative risks** For
conversion of hazard ratios and/or odds ratios to relative risks refer
to VanderWeele 2019 (https://doi.org/10.1111/biom.13197) and/or use the
conversion tools developed by the Teaching group in EBM in 2022 for
hazard ratios (https://ebm-helper.cn/en/Conv/HR_RR.html) and/or odds
ratios (https://ebm-helper.cn/en/Conv/OR_RR.html).

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
