#' Attribute health impacts to an environmental stressor

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the attributable health impacts (mortality or morbidity) due to
#' exposure to an environmental stressor (air pollution or noise), using either relative risk (\strong{RR}) or absolute risk (\strong{AR}).
#' @description
#' For (short) \strong{examples} please see below or consult the vignette for more detailed examples.

# ARGUMENTS ####################################################################
#' @inheritParams attribute_master

# USAGE ########################################################################
#' @usage
#' attribute_health(
#'   # RR & AR
#'   approach_risk = "relative_risk", # alternative: "absolute_risk"
#'   exp_central, exp_lower = NULL, exp_upper = NULL,
#'   cutoff_central = 0, cutoff_lower = NULL, cutoff_upper = NULL,
#'   pop_exp = NULL,
#'   erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
#'   # RR ONLY
#'   rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
#'   rr_increment = NULL,
#'   erf_shape = NULL,
#'   bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
#'   prop_pop_exp = 1,
#'   # ITERATION (OPTIONAL)
#'   geo_id_disaggregated = "a",
#'   geo_id_aggregated = NULL,
#'   age_group = "all",
#'   sex = "all",
#'   # META (OPTIONAL)
#'   info = NULL,
#'   population = NULL,
#'   # YLD (OPTIONAL)
#'   dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
#'   duration_central = 1, duration_lower = NULL, duration_upper = NULL
#' )

# DETAILS ######################################################################
#' @details
#' \strong{Assessment of multiple geographic units}
#' @details
#' To assess the attributable health impact/burden across multiple geographic units with \code{attribute_health()}, you must specify the argument \code{geo_id_disaggregated} and (optionally) \code{geo_id_aggregated}, in addition to the other required function arguments.
#' @details
#' The length of the input vectors to the function arguments must be
#' @details
#' \deqn{\text{length input vectors} = \text{number of geo units} \times \text{number of exposure categories} \times \text{number of age groups (if entered)} \times \text{number of sex groups (if entered),}}
#' @details
#' i.e. there must be one line / observation for each specific combination of geo unit, exposure category, age and sex group.
#' @details
#' Alternatively, for those arguments that are independent of location (e.g. \code{approach_risk}, \code{rr_...}, \code{erf_shape}, ...), you can enter a single value, which will be recycled to match the length of the other geo unit-specific input data. Additional categories can be passed on via the \code{info} argument.

#' @details
#' \strong{What you put in is what you get out}
#' @details
#' The health metric you put in (e.g. absolute disease cases, deaths per 100 000 population, DALYs, prevalence, incidence, ...) is the one you get out. Exception: if you enter a disability weight (to the \code{dw_...} arguments) the attributable impact will be in YLD (years lived with disability).

#' @details
#' \strong{Function arguments}
#' @details
#' \code{exp_central}, \code{exp_lower}, \code{exp_upper}
#' @details
#' In case of exposure bands enter only one exposure value per band (e.g. the means of the lower and upper bounds of the exposure bands).
#' @details
#' \code{cutoff_central}, \code{cutoff_lower}, \code{cutoff_upper}
#' @details
#' The cutoff level refers to the exposure level below which no health effects occur in the same unit as the exposure. If exposure categories are used, the length of this input must be the same as in the \code{exp_...} argument(s).
#' @details
#' \code{pop_exp}
#' @details
#' \emph{Required in AR pathways; optional in RR pathways.} In AR pathways the population exposed per exposure category is multiplied with the corresonding category-specific risk to obtain the absolute number of people affected by the health outcome.
#' @details
#' In RR pathways, only to be specified if \code{prop_pop_exp} is not specified: based on the values entered the proportion of people exposed to each exposure category (needed for calculation of the population attributable fraction) is derived. See the equation for the population attributable fraction below.
#' @details
#' \code{erf_eq_central}, \code{erf_eq_lower}, \code{erf_eq_upper}
#' @details
#' \emph{Required in AR pathways; in RR pathways required only if rr_... arguments not specified.} Enter the exposure-response function as a \code{function}, e.g. output from \code{stats::splinefun()} or \code{stats::approxfun()}, or as a \code{string} formula, e.g. \code{"3+c+c^2"} (with the \emph{c} representing the concentration/exposure).
#' @details
#' If you have x-axis (exposure) and y-axis (relative risk) value pairs of multiple points lying on the the exposure-response function, you could use e.g. \code{stats::splinefun(x, y, method="natural")} to derive the exposure-response function (in this example using a cubic spline natural interpolation).
#' @details
#' \code{rr_increment}
#' @details
#' \emph{Only applicable in RR pathways.} Relative risks from the literature are valid for a specific increment in the exposure, in case of air pollution often 10 or 5 \eqn{µg/m^3}).
#' @details
#' \code{bhd_central}, \code{bhd_lower}, \code{bhd_upper}
#' @details
#' \emph{Only applicable in RR pathways.} Baseline health data for each exposure category must be entered.
#' @details
#' \code{prop_pop_exp}
#' @details
#' \emph{Only applicable in RR pathways.} In RR pathways indicates the fraction(s) (value(s) from 0 until and including 1) of the total population exposed to the exposure categories. Only to be specified if \code{pop_exp} is not specified. See equation of the population attributable fraction for categorical exposure below.
#' @details
#' \code{geo_id_aggregated}, \code{geo_id_disaggregated}
#' @details
#' \emph{Only applicable in assessments with multiple geographic units.} For example, if you provide the names of the municipalities under analysis to \code{geo_id_disaggregated}, you might provide to \code{geo_id_aggregated} the corresponding region / canton / province names.
#' Consequently, the vectors fed to \code{geo_id_disaggregated} and \code{geo_id_aggregated} must be of the same length.
#' @details
#' \code{info}
#' @details
#' \emph{Optional argument.} Information entered to this argument will be added as column(s) names \code{info_1}, \code{info_2}, \code{info_...} to the results table. These additional columns can be used to further stratify the analysis in a secondary step (see example below).
#' @details
#' \code{population}
#' @details
#' \emph{Optional argument.} The total population entered here is used to determine impact rate per 100 000 population.
#' @details
#' \code{duration_central}, \code{duration_lower}, \code{duration_upper}
#' @details
#' \emph{Only applicable in assessments of YLD (years lived with disability).} If the value of \code{duration_central} is 1 year, it refers to the prevalence-based approach, while a value above 1 year to the incidence-based approach (Kim, 2022, https://doi.org/10.3961/jpmph.21.597).

#' @details
#' \strong{Equations (relative risk)}
#' @details
#' The most general equation describing the population attributable fraction (PAF) mathematically is an integral form
#' \deqn{PAF = \frac{\int RR(x)PE(x)dx - 1}{\int RR(x)PE(x)dx}}
#' @details Where:
#' @details x     = exposure level
#' @details PE(x) = population distribution of exposure
#' @details RR(x) = relative risk at exposure level compared to the reference level
#' @details
#' If the population exposure is described as a categorical rather than continuous exposure, the integrals in this equation may be converted to sums, resulting in the following equation for the PAF
#' \deqn{PAF = \frac{\sum RR_i \times PE_i dx - 1}{\sum RR_i \times PE_i dx}}
#' @details Where:
#' @details i     = is the exposure category (e.g. in bins of 1 \eqn{µg/m^3} PM2.5 or 5 dB noise exposure)
#' @details \eqn{PE_i} = fraction of population in exposure category i
#' @details \eqn{RR_i} = relative risk associated with the mean exposure level in exposure category i compared to the reference level
#' @details
#' There is one alternative for the PAF for categorical exposure distribution that is commonly used. It is mathematically equivalent to the equation right above, meaning that numerical estimates based on these equations are identical.
#' \deqn{PAF = \frac{\sum PE_i(RR_i - 1)}{\sum PE_i(RR_i - 1) + 1}}
#' @details Where:
#' @details i     = is the exposure category (e.g. in bins of 1 \eqn{µg/m^3} PM2.5 or 5 dB noise exposure)
#' @details \eqn{PE_i} = fraction of population in exposure category i
#' @details \eqn{RR_i} = relative risk associated with the mean exposure level in exposure category i compared to the reference level
#' @details
#' Finally, if the exposure is provided as the population weighted mean concentration (PWC), the equation for the PAF is reduced to
#' \deqn{PAF = \frac{RR_{PWC} - 1}{RR_{PWC}}}
#' Where \eqn{RR_PWC} is the relative risk associated with the population weighted mean exposure.

#' @details
#' \strong{Equation (absolute risk)}
#' \deqn{N = \sum AR_i\times PE_i}
#' @details Where:
#' @details N = the number of cases of the exposure-specific health outcome that are attributed to the exposure
#' @details \eqn{AR_i} = absolute risk associated with the mean exposure level of exposure category i
#' @details \eqn{PE_i} = population exposed (absolute number) to exposure levels of exposure category i

#' @details
#' \strong{Conversion of alternative risk measures to relative risks}
#' For conversion of hazard ratios and/or odds ratios to relative risks refer to https://doi.org/10.1111/biom.13197 and/or use the conversion tool for hazard ratios (https://ebm-helper.cn/en/Conv/HR_RR.html) and/or odds ratios (https://ebm-helper.cn/en/Conv/OR_RR.html).

# VALUE ########################################################################
#' @inherit attribute_master return

# EXAMPLES #####################################################################
#' @examples
#' # Goal: attribute lung cancer cases to population-weighted PM2.5 exposure
#' # using relative risk
#'
#' results <- attribute_health(
#'   erf_shape = "log_linear",
#'   rr_central = 1.369,            # Central relative risk estimate
#'   rr_increment = 10,             # per μg / m^3 increase in PM2.5 exposure
#'   exp_central = 8.85,            # Central exposure estimate in μg / m^3
#'   cutoff_central = 5,            # μg / m^3
#'   bhd_central = 30747            # Baseline health data: lung cancer incidence
#'  )
#'
#' results$health_main$impact_rounded # Attributable cases
#'
#' @examples
#' # Goal: attribute cases of high annoyance to (road traffic) noise exposure
#' # using absolute risk
#'
#' results <- attribute_health(
#'   approach_risk = "absolute_risk",
#'   exp_central = c(57.5, 62.5, 67.5, 72.5, 77.5),
#'   pop_exp = c(387500, 286000, 191800, 72200, 7700),
#'   erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
#' )
#'
#' results$health_main$impact_rounded # Attributable high annoyance cases
#'
#' @examples
#' # Goal: attribute disease cases to PM2.5 exposure in multiple geographic
#' # units, such as municipalities, provinces, countries, …
#'
#' results <- attribute_health(
#'   geo_id_disaggregated = c("Zurich", "Basel", "Geneva", "Ticino"),
#'   geo_id_aggregated = c("Ger","Ger","Fra","Ita"),
#'   rr_central = 1.369,
#'   rr_increment = 10,
#'   cutoff_central = 5,
#'   erf_shape = "log_linear",
#'   exp_central = c(11, 11, 10, 8),
#'   bhd_central = c(4000, 2500, 3000, 1500)
#' )
#'
#' # Attributable cases (aggregated)
#' results$health_main$impact_rounded
#'
#' # Attributable cases (disaggregated)
#' results$health_detailed$results_raw |> dplyr::select(
#'   geo_id_disaggregated,
#'   geo_id_aggregated,
#'   impact_rounded
#' )
#'
#' @examples
#' # Goal: determine attributable YLD (years lived with disability)
#' results  <- attribute_health(
#'   exp_central = 8.85,
#'   prop_pop_exp = 1,
#'   cutoff_central = 5,
#'   bhd_central = 1000,
#'   rr_central = 1.1,
#'   rr_increment = 10,
#'   erf_shape = "log_linear",
#'   duration_central = 100,
#'   dw_central = 1,
#'   info = "pm2.5_yld"
#' )
#'
#' results$health_main$impact
#'
#' @examples
#' # Goal: determine mean attributable health impacts by education level
#' info <- data.frame(
#'   education = rep(c("secondary", "bachelor", "master"), each = 5) # education level
#' )
#' output_attribute <- attribute_health(
#'   rr_central = 1.063,
#'   rr_increment = 10,
#'   erf_shape = "log_linear",
#'   cutoff_central =  0,
#'   exp_central = sample(6:10, 15, replace = TRUE),
#'   bhd_central = sample(100:500, 15, replace = TRUE),
#'   geo_id_disaggregated = c(1:nrow(info)), # a vector of (random) unique IDs must be entered
#'   info = info
#' )
#' output_stratified <- output_attribute$health_detailed$results_agg_cat_exp |>
#'   dplyr::group_by(info_1) |>
#'   dplyr::summarize(mean_impact = mean(impact)) |>
#'   print()

#' @author Alberto Castro & Axel Luyten

#' @export



attribute_health <-
  function(
    # RR & AR
    approach_risk = "relative_risk",
    exp_central, exp_lower = NULL, exp_upper = NULL,
    cutoff_central = 0, cutoff_lower = NULL, cutoff_upper = NULL,
    pop_exp = NULL,
    erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
    # RR ONLY
    rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
    rr_increment = NULL,
    erf_shape = NULL,
    bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
    prop_pop_exp = 1,
    # ITERATION (OPTIONAL)
    geo_id_disaggregated = "a", geo_id_aggregated = NULL,
    age_group = "all",
    sex = "all",
    ## YLD (OPTIONAL)
    dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
    duration_central = NULL, duration_lower = NULL, duration_upper = NULL,
    # META (OPTIONAL)
    info = NULL,
    population = NULL){



    # Get input_args
    # i.e. a list with all argument values and characteristics
    input_args <-
      healthiar:::get_input_args(environment = base::environment(),
                                 call = match.call())

    output <-
      healthiar:::attribute_master(
        # RR & AR
        approach_risk = approach_risk,
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        pop_exp = pop_exp,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        # RR ONLY
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        bhd_central = bhd_central, bhd_lower = bhd_lower, bhd_upper = bhd_upper,
        prop_pop_exp = prop_pop_exp,
        # ITERATION (OPTIONAL)
        geo_id_disaggregated = geo_id_disaggregated , geo_id_aggregated = geo_id_aggregated,
        age_group = age_group,
        sex = sex,
        # META (OPTIONAL)
        info = info,
        population = population,
        # YLD (OPTIONAL)
        dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
        duration_central = duration_central, duration_lower =  duration_lower, duration_upper = duration_upper,
        # LIFE TABLE (OPTIONAL)
        is_lifetable = FALSE,
        first_age_pop = NULL, last_age_pop = NULL,
        population_midyear_male = NULL, population_midyear_female = NULL,
        deaths_male = NULL, deaths_female = NULL,
        min_age = NULL, max_age = NULL,
        approach_exposure = NULL,
        approach_newborns = NULL,
        year_of_analysis = NULL,
        # INTERNAL ARGUMENTS
        input_args = input_args)

    return(output)


  }
