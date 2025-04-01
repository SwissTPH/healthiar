#' Attribute health impacts to an environmental stressor

#' @description This function calculates the health impacts (mortality or morbidity)
#' of exposure to an environmental stressor (air pollution or noise), using either relative or absolute risk.
#' @details
#' Generally, the health metric inputted and outputted are the same, e.g. if the baseline health data are mortalities then the result will be mortalities as well. Analogeously for disease cases, DALYs, etc.
#' @details
#' Exception: if a disability weight is inputted alongside a morbidity health outcome, then the main output will be YLD.

#' @usage
#' Relative risk case:
#' attribute_health(
#'   approach_risk = "relative_risk",
#'   erf_shape,
#'   rr_central, rr_lower = NULL, rr_upper = NULL,
#'   rr_increment,
#'   exp_central, exp_lower = NULL, exp_upper = NULL,
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
#'   population = NULL,
#'   info = NULL
#')
#' @inherit attribute_master return
#' @inheritParams attribute_master
#' @examples
#' # Goal: attribute lung cancer cases to population-weighted PM2.5 exposure using relative risk
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
#' # Attributable cases
#' print(results$health_main$impact_rounded)
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
           pop_exp = NULL,
           cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           geo_id_disaggregated = NULL, geo_id_aggregated = NULL,
           population = NULL,
           info = NULL,
           # Only for for YLD
           dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
           duration_central = 1, duration_lower = NULL, duration_upper = NULL){

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
        info = info)

    return(output)


  }
