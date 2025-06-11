#' Attribute health impacts to an environmental stressor

#' @description
#' This function calculates the health impacts (mortality or morbidity)
#' of exposure to an environmental stressor (air pollution or noise), using either relative or absolute risk.
#' @description
#' For examples please see the vignette and/or below.

#' @inheritParams attribute_master

#' @details
#' What you put in is what you get out
#' @details
#' Generally, the health metric you put in (e.g. absolute disease cases, deaths per 100'000 population, DALYs, prevalence, incidence, ...) is the one you get out. Exception: if a disability weight is inputted alongside a morbidity health outcome, then the main output will be YLD.
#' @details
#' Equations (relative risk)
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
#' @details \eqn{RR_i} = relative risk for exposure category level i compared to the reference level
#' @details
#' There is one alternative for the PAF for categorical exposure distribution that is commonly used. It is mathematically equivalent to the equation right above, meaning that numerical estimates based on these equations are identical.
#' \deqn{PAF = \frac{\sum PE_i(RR_i - 1)}{\sum PE_i(RR_i - 1) + 1}}
#' @details Where:
#' @details i     = is the exposure category (e.g. in bins of 1 \eqn{µg/m^3} PM2.5 or 5 dB noise exposure)
#' @details \eqn{PE_i} = fraction of population in exposure category i
#' @details \eqn{RR_i} = relative risk for exposure category level i compared to the reference level
#' @details
#' Finally, if the exposure is provided as the population weighted mean concentration (PWC), the equation for the PAF is reduced to
#' \deqn{PAF = \frac{RR_{PWC} - 1}{RR_{PWC}}}
#' Where \eqn{RR_PWC} is the relative risk associated with the population weighted mean exposure.
#' @details
#' Equation (absolute risk)
#' \deqn{N = \sum AR_i\times PE_i}
#' @details Where:
#' @details N = the number of cases of the exposure-specific health outcome that are attributed to the exposure
#' @details \eqn{AR_i} = absolute risk at the mean of exposure bin i
#' @details \eqn{PE_i} = fraction of the population exposed to exposure levels of the exposure category i
#' @details
#' Conversion of alternative risk measures to relative risks
#' @details
#' For conversion of hazard ratios and/or odds ratios to relative risks refer to https://doi.org/10.1111/biom.13197 and/or use the conversion tool for hazard ratios (https://ebm-helper.cn/en/Conv/HR_RR.html) and/or odds ratios (https://ebm-helper.cn/en/Conv/OR_RR.html).

#' @details
#' Function arguments
#' @details
#' \code{erf_eq_...}
#' @details
#' If you have x-axis (exposure) and y axis (relative risk) value pairs of multiple points lying on the the exposure-response function, you could use a cubic spline natural interpolation to derive the exposure-response function using, e.g. \code{stats::splinefun(x, y, method="natural")}.
#' @details
#' \code{bhd_...}
#' @details
#' text
#' @details
#' \code{duration_...}
#' @details
#' The default of \code{duratoin_central} is 1 year, which is aligned with the prevalence-based approach , while a value above 1 year corresponds to the incidence-based approach (Kim, 2022, https://doi.org/10.3961/jpmph.21.597).
#' @details
#' \code{geo_id_aggregated}
#' @details
#' For example, if you provide the municipality names to \code{geo_id_disaggregated}, you might provide to \code{geo_id_aggregated} the corresponding region / canton / province.
#' Consequently, the vectors fed to \code{geo_id_disaggregated} and \code{geo_id_aggregated} must be of the same length..
#'
#'
#'
#'

#' @inherit attribute_master return

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
#' print(results$health_main$impact_rounded) # Attributable cases
#' @examples
#' # Goal: attribute cases of high annoyance to (road traffic) noise exposure
#'
#' results <- attribute_health(
#'   approach_risk = "absolute_risk",
#'   exp_central = c(57.5, 62.5, 67.5, 72.5, 77.5),
#'   pop_exp = c(387500, 286000, 191800, 72200, 7700),
#'   erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
#' )
#'
#' print(results$health_main$impact_rounded) # Attributable cases
#' @examples
#' # Goal: attribute disease cases to PM2.5 exposure in multiple geographic
#' # units, such as municipalities, provinces, countries, …
#' @examples
#' results <- attribute_health(
#'   geo_id_disaggregated = c("Zurich", "Basel", "Geneva", "Ticino", "Valais"),
#'   geo_id_aggregated = c("Ger","Ger","Fra","Ita","Fra"),
#'   rr_central = 1.369,
#'   rr_increment = 10,
#'   cutoff_central = 5,
#'   erf_shape = "log_linear",
#'   exp_central = c(11, 11, 10, 8, 7),
#'   bhd_central = c(4000, 2500, 3000, 1500, 500)
#' )
#'
#' # Attributable cases (aggregated)
#' results$health_main$impact_rounded
#'
#' # Attributable cases (disaggregated)
#' results$health_detailed$impact_raw |> select(
#'   geo_id_disaggregated,
#'   geo_id_aggregated,
#'   impact_rounded
#' )

#' @author Alberto Castro & Axel Luyten

#' @export



attribute_health <-
  function(approach_risk = "relative_risk",
           erf_shape = NULL,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           rr_increment = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           exp_central,
           exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           pop_exp = NULL,
           cutoff_central = 0, cutoff_lower = NULL, cutoff_upper = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           geo_id_disaggregated = "a", geo_id_aggregated = NULL,
           population = NULL,
           info = NULL,
           # Only for for YLD
           dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
           duration_central = 1, duration_lower = NULL, duration_upper = NULL){

    # Get what the arguments that the user passed
    used_args <- base::names(base::as.list(base::match.call()))[-1] # drop function name
    # Get all arguments
    all_args <- base::names(base::formals(attribute_health))
    # Get unused arguement as the difference
    unused_args <- base::setdiff(all_args, used_args)

    output <-
      healthiar:::attribute_master(
        is_lifetable = FALSE,
        approach_risk = approach_risk,
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        pop_exp,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central = bhd_central, bhd_lower = bhd_lower, bhd_upper = bhd_upper,
        population = population,
        approach_exposure = NULL,
        approach_newborns = NULL,
        first_age_pop = NULL, last_age_pop = NULL,
        population_midyear_male = NULL, population_midyear_female = NULL,
        year_of_analysis = NULL,
        min_age = NULL, max_age = NULL,
        dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
        duration_central = duration_central, duration_lower =  duration_lower, duration_upper = duration_upper,
        geo_id_disaggregated = geo_id_disaggregated , geo_id_aggregated = geo_id_aggregated,
        info = info,
        .unused_args = unused_args)

    return(output)


  }
