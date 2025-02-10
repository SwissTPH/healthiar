#' Attribute years lived with disability (YLD) from a specific morbidity health outcome that is due to an environmental stressor

#' @description
#' This function first attributes disease cases of a specific morbidity health outcome to an environmental exposure.
#' Then this disease burden is then expressed in YLDs, using associated disability weight and duration.
#' @description
#' For calculation of YLDs using life tables use \code{attribute_yld_from_lifetable}.
#' Assumption: cases happen at the start of the year.

#' @return
#' This function returns two lists: 1) \code{health_main}, which contains a tibble with the main results and
#' 2) \code{health_detailed}, which contains detailed (and interim) results.
#' The result tibbles include columns such as:
#' \itemize{
#'  \item pop_fraction (population attributable fraction)
#'  \item impact (health impact)
#'  \item And many more.
#'  }
#' @examples
#' To be added
#' @inheritParams attribute
#' @import dplyr
#' @import purrr

#' @author Axel Luyten
#' @note Experimental function
#' @export
attribute_yld <-
  function(approach_risk = "relative_risk",
           erf_shape = NULL,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           rr_increment = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           exp_central,
           exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           dw_central, dw_lower = NULL, dw_upper = NULL,
           duration_central, duration_lower = NULL, duration_upper = NULL,
           geo_id_disaggregated = NULL, geo_id_aggregated = NULL,
           approach_multiexposure = NULL,
           population = NULL,
           info = NULL
           ){


    output <-
      healthiar::attribute(
        health_outcome = "yld",
        approach_risk = approach_risk,
        approach_multiexposure = approach_multiexposure,
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        population = population,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        bhd_central = bhd_central, bhd_lower = bhd_lower, bhd_upper = bhd_upper,
        first_age_pop = NULL, last_age_pop = NULL,
        population_midyear_male = NULL, population_midyear_female = NULL,
        year_of_analysis = NULL,
        min_age = NULL, max_age = NULL,
        dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
        duration_central = duration_central, duration_lower = duration_lower, duration_upper = duration_upper,
        geo_id_disaggregated = NULL , geo_id_aggregated = NULL,
        info = info)


    return(output)

  }
