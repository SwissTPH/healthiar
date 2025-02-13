#' Attributable years of life lost based on life tables
#'
#' @description Calculates the years of life lost (YLL) attributable to the exposure to an environmental stressor using a life table approach.
#' @details Function defined for relative risk approach; absolute risk is not supported due to methodological restrictions.
#' @usage
#' attribute_yll_from_lifetable(
#'   ## Calculation specification
#'   approach_multiexposure = NULL,
#'   approach_exposure = "single_year",
#'   approach_newborns = "without_newborns",
#'   year_of_analysis,
#'   min_age = NULL,
#'   max_age = NULL,
#'   time_horizon = NULL,
#'   ## Lifetable arguments
#'   first_age_pop,
#'   last_age_pop,
#'   population_midyear_male, population_midyear_female,
#'   deaths_male = NULL, deaths_female = NULL,
#'   ## Risk and shape arguments
#'   rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
#'   rr_increment = NULL,
#'   erf_shape = NULL,
#'   erf_eq_central = NULL,
#'   erf_eq_lower = NULL,
#'   erf_eq_upper = NULL,
#'   duration_central = NULL,
#'   ## Other central input
#'   exp_central, exp_lower = NULL, exp_upper = NULL,
#'   prop_pop_exp = 1,
#'   cutoff_central, cutoff_lower = NULL, cutoff_upper = NULL,
#'   ## Meta-info
#'   info = NULL
#' )
#' @inheritParams attribute
#' @returnss
#' This function returns two lists: 1) \code{health_main}, which contains a tibble with the main results and
#' 2) \code{health_detailed}, which contains detailed (and interim) results.
#' @returnss
#' The main results tibble include columns such as:
#' \itemize{
#'  \item pop_fraction (population attributable fraction; only for assessments using relative risk)
#'  \item impact (health impact, in this case YLL)
#'  \item impact_per_100k_inhab (impact per 100'000 inhabitants)
#'  \item And many more.
#'  }
#' @returnss
#' The detailed results tibble additionally provides information on the year- and age-specific impacts for males and females seperately.
#' @examples
#' TBD
#' @author Alberto Castro
#' @note Experimental function
#' @export
attribute_yll_from_lifetable <-
  function(
    ## Calculation specification
    approach_multiexposure = NULL,
    approach_exposure = "single_year",
    approach_newborns = "without_newborns",
    year_of_analysis,
    min_age = NULL, max_age = NULL,
    time_horizon = NULL,
    ## Lifetable arguments
    first_age_pop, last_age_pop,
    population_midyear_male, population_midyear_female,
    deaths_male = NULL, deaths_female = NULL,
    ## Risk and shape arguments
    rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
    rr_increment = NULL, erf_shape = NULL,
    erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
    duration_central = NULL, # AL 2025-02-12: Applicable to YLL calculations?
    ## Other central input
    exp_central, exp_lower = NULL, exp_upper = NULL,
    prop_pop_exp = 1,
    cutoff_central, cutoff_lower = NULL, cutoff_upper = NULL,
    ## Meta-info
    info = NULL
  ) {

    output <-
      healthiar::attribute(
        ## Calculation specification
        health_outcome = "yll_from_lifetable", # Set outcome metric
        approach_risk = "relative_risk",
        approach_exposure = approach_exposure,
        approach_newborns = approach_newborns,
        approach_multiexposure = approach_multiexposure,
        year_of_analysis = year_of_analysis,
        min_age = min_age, max_age = max_age,
        ## Lifetable arguments
        first_age_pop = first_age_pop, last_age_pop = last_age_pop,
        population_midyear_male = population_midyear_male, population_midyear_female = population_midyear_female,
        deaths_male = deaths_male, deaths_female = deaths_female,
        time_horizon = time_horizon,
        ## Risk and shape arguments
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        ## Other central input
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        prop_pop_exp = prop_pop_exp,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        ## Meta-info
        geo_id_disaggregated = NULL, geo_id_aggregated = NULL,
        info = info
        )

    return(output)

  }
