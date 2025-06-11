#' Attributable premature deaths or YLL using a life table approach

#' @description
#' This function assesses premature deaths or years of life lost (YLL) attributable to exposure to an environmental stressor using a life table approach.

#' @inheritParams attribute_master

#' @details
#' The life table methodology follows the one implemented in the WHO tool AirQ+, and is described in more detail by Miller & Hurley (2003): https://doi.org/10.1136/jech.57.3.200
#' @details
#' A more expansive life table case study by Miller is available here: https://cleanair.london/app/uploads/CAL-098-Mayors-health-study-report-June-2010-1.pdf (accessed April 2025)
#' @details
#' See the AirQ+ manual "Health impact assessment of air pollution: AirQ+ life table manual" for guidance on how to convert larger age groups to 1 year age groups ("section "Estimation of yearly values"): https://iris.who.int/bitstream/handle/10665/337683/WHO-EURO-2020-1559-41310-56212-eng.pdf (accessed April 2025):
#' @details
#' Conversion of alternative risk measures to relative risks
#' For conversion of hazard ratios and/or odds ratios to relative risks refer to https://doi.org/10.1111/biom.13197 and/or use the conversion tool for hazard ratios (https://ebm-helper.cn/en/Conv/HR_RR.html) and/or odds ratios (https://ebm-helper.cn/en/Conv/OR_RR.html).

#' @details
#' Function arguments
#' @details
#' \code{time_horizon}
#' @details
#' For example, would be 10 if one is interested in the impacts of exposure during the year of analysis and the next 9 years (= 10 years in total).
#' @details
#' \code{min_age}, \code{max_age}
#' The \code{min_age} default value 30 implies that all adults aged 30 or older will be affected by the exposure. Values for \code{max_age} analogeously specify the age above which no health effects of the exposure are considered.

#' @inherit attribute_master return

#' @examples
#' TBD

#' @author Alberto Castro & Axel Luyten

#' @export



attribute_lifetable <-
  function(first_age_pop, last_age_pop,
           population_midyear_male, population_midyear_female,
           deaths_male = NULL, deaths_female = NULL,
           min_age = NULL, max_age = NULL,
           approach_exposure = "single_year",
           approach_newborns = "without_newborns",
           health_outcome = NULL,
           year_of_analysis,
           erf_shape = NULL,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           rr_increment = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           exp_central = 0, exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
           geo_id_disaggregated = "a", geo_id_aggregated = NULL,
           info = NULL
           ) {

    # Get what the arguments that the user passed
    used_args <- base::names(base::as.list(base::match.call()  ))[-1] # drop function name
    # Get all arguments
    all_args <- base::names(base::formals(attribute_lifetable))
    # Get unused arguement as the difference
    unused_args <- base::setdiff(all_args, used_args)

    output <-
      healthiar:::attribute_master(
        is_lifetable = TRUE,
        health_outcome = health_outcome,
        approach_risk = "relative_risk",
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
        # Lifetable arguments
        approach_exposure = approach_exposure,
        approach_newborns = approach_newborns,
        first_age_pop = first_age_pop, last_age_pop = last_age_pop,
        deaths_male = deaths_male,
        deaths_female = deaths_female,
        population_midyear_male = population_midyear_male, population_midyear_female = population_midyear_female,
        year_of_analysis = year_of_analysis,
        min_age = min_age, max_age = max_age,
        geo_id_disaggregated = geo_id_disaggregated, geo_id_aggregated = geo_id_aggregated,
        info = info,
        .unused_args = unused_args)

    return(output)

  }
