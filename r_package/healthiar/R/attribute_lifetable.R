#' Deaths and YLL based on life table approach
#'
#' @description Assesses the premature deaths or years of life lost attributable to the exposure to an environmental stressor using a life table approach.
#' @inheritParams attribute_master
#' @returns
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central, lower and upper bound confidence interval.
#' Moreover, the data frame include columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export

attribute_lifetable <-
  function(health_outcome = NULL,
           approach_multiexposure = NULL,
           exp_central, exp_lower = NULL, exp_upper = NULL,
           prop_pop_exp = 1,
           cutoff_central, cutoff_lower = NULL, cutoff_upper = NULL,
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           rr_increment = NULL, erf_shape = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           approach_exposure = "single_year",
           approach_newborns = "without_newborns",
           deaths_male = NULL,
           deaths_female = NULL,
           first_age_pop, last_age_pop,
           population_midyear_male, population_midyear_female,
           year_of_analysis,
           min_age = NULL, max_age = NULL,
           geo_id_disaggregated = NULL, geo_id_aggregated = NULL,
           info = NULL){

    output <-
      healthiar:::attribute_master(
        is_lifetable = TRUE,
        health_outcome = health_outcome,
        approach_risk = "relative_risk",
        approach_multiexposure = approach_multiexposure,
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
        info = info)

    return(output)

  }
