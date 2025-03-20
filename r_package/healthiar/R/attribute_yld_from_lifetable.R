#' Attributable years lived with disability based on life tables

#' @description
#' Calculates the years lived with disability (YLD) attributable to exposure to an environmental stressor using a life table approach.
#' @inheritParams attribute
#' @inherit attribute_yll_from_lifetable return
#' @author Axel Luyten
#' @note Experimental function
#' @export
attribute_yld_from_lifetable <-
  function(
    ## Calculation specification
    approach_multiexposure = NULL,
    approach_exposure = "single_year",
    approach_newborns = "without_newborns",
    year_of_analysis,
    min_age = NULL, max_age = NULL,
    time_horizon = NULL,
    dw_central, dw_lower = NULL, dw_upper = NULL,
    duration_central, duration_lower = NULL, duration_upper = NULL,
    ## Lifetable arguments
    first_age_pop, last_age_pop,
    population_midyear_male, population_midyear_female,
    deaths_male = NULL, deaths_female = NULL,
    ## Risk and shape arguments
    rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
    rr_increment = NULL, erf_shape = NULL,
    erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
    ## Other central input
    exp_central, exp_lower = NULL, exp_upper = NULL,
    prop_pop_exp = 1,
    cutoff_central, cutoff_lower = NULL, cutoff_upper = NULL,
    #geo iterations
    geo_id_disaggregated = NULL, geo_id_aggregated = NULL,
    ## Meta-info
    info = NULL
  ) {

    output<-
      healthiar::attribute(
        health_outcome = "yld_from_lifetable",
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
        approach_exposure = approach_exposure,
        approach_newborns = approach_newborns,
        first_age_pop = first_age_pop, last_age_pop = last_age_pop,
        deaths_male = deaths_male,
        deaths_female = deaths_female,
        population_midyear_male = population_midyear_male, population_midyear_female = population_midyear_female,
        year_of_analysis = year_of_analysis,
        time_horizon = time_horizon,
        min_age = min_age, max_age = max_age,
        dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
        duration_central = duration_central, duration_lower = duration_lower, duration_upper = duration_upper,
        geo_id_disaggregated = geo_id_disaggregated, geo_id_aggregated = geo_id_aggregated,
        info = info)

    return(output)

  }
