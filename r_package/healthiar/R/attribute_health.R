#' Attribute health impacts to an environmental stressor

#' @description This function calculates the health impacts (mortality or morbidity)
#' of exposure to an environmental stressor (air pollution or noise), using either relative or absolute risk.
#' @details
#' The health metric inputted and outputted are the same, e.g. if the baseline health data are mortalities then the result will be mortalities as well. Analogeously for disease cases, DALYs, etc.

#' @usage
#' Relative risk case:
#' attribute_health(
#'   approach_risk = "relative_risk",
#'   erf_shape,
#'   rr_central, rr_lower = NULL, rr_upper = NULL,
#'   rr_increment,
#'   exp_central, exp_lower = NULL, exp_upper = NULL
#'   cutoff_central, cutoff_lower = NULL, cutoff_upper = NULL,
#'   bhd_central
#' )
#'
#' Absolute risk case:
#' healthiar::attribute_health(
#'   approach_risk = "absolute_risk",
#'   erf_eq_central, erf_eq_lower = NULL, erf_eq_upper = NULL,
#'   exp_central, exp_lower = NULL, exp_upper = NULL,
#'   population,
#'   prop_pop_exp
#')
#'
#' All available arguments:
#' attribute_health(
#'   approach_risk = "relative_risk", # Default
#'   erf_shape = NULL,
#'   rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
#'   rr_increment = NULL,
#'   erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
#'   exp_central, exp_lower = NULL, exp_upper = NULL,
#'   prop_pop_exp = 1,
#'   cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
#'   bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
#'   geo_id_disaggregated = NULL,
#'   geo_id_aggregated = NULL,
#'   approach_multiexposure = NULL,
#'   population = NULL,
#'   info = NULL
#')
#' @inherit attribute return
#' @inheritParams attribute
#' @examples
#' TBD
#' @author Alberto Castro
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
           cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           geo_id_disaggregated = NULL, geo_id_aggregated = NULL,
           approach_multiexposure = NULL,
           population = NULL,
           info = NULL){

    output <-
      healthiar::attribute(
        health_outcome = "same_input_output",
        approach_risk = approach_risk,
        approach_multiexposure = approach_multiexposure,
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
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
        dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
        duration_central = NULL,
        geo_id_disaggregated = geo_id_disaggregated , geo_id_aggregated = geo_id_aggregated,
        info = info)

    return(output)


  }
