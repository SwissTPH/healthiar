#' Attributable health cases based on relative risk

#' @description
#' Calculates the health impacts, mortality or morbidity, of an environmental stressor using a single value for baseline heath data, i.e. without life table.
#' @details It provides as a result the mean as well as the lower and the higher bound of the impact based on the confidence interval of the concentration-response function.
#' @param approach_risk \code{String} specifying the risk risk method. Options: "relative_risk" (default) or "absolute_risk".
#' @param erf_shape \code{String} specifying the shape of the exposure-response function to be assumed. Options: "linear", log_linear", "linear_log", "log_log".
#' @param rr_central,rr_lower,rr_upper \code{Numeric value(s)} specifying the central estimate of the relative risk and (optionally) the corresponding lower and upper 95\% confidence interval bounds.
#' @param rr_increment \code{Numeric value} specifying the increment of the exposure-response function in \eqn{µg/m^3} (usually 10 or 5 \eqn{µg/m^3}).
#' @param erf_eq_central,erf_eq_lower,erf_eq_upper \code{String} or \code{function} specifying the equation of the user-defined exposure-response function and (optionally) the corresponding lower and upper 95\% confidence interval functions.
#' If a \code{string} is entered, the function must contains only one variable: x (exposure), e.g. "3+x+x^2".
#' If a \code{function} is fed to the argument, it has be of the class function class, e.g. output from the functions \code{stats::splinefun()} or \code{stats::approxfun()}.
#' If you have x-axis (exposure) and y axis (relative risk) value pairs of multiple points lying on the the exposure-response function, you could use a cubic spline natural interpolation to derive the exposure-response function using, e.g. \code{stats::splinefun(x, y, method="natural")}.
#' @param exp_central,exp_lower,exp_upper \code{Numeric value(s)} specifying the exposure level(s) to the environmental stressor and (optionally) to lower and upper bound of the 95\% confidence interval. If only one value is provided, it will be assumed that it refers to population-weighted mean exposure in \eqn{µg/m^3}. If a {vector} is provided, it will be assumed that it refers to the exposure categories (average exposure in the category) in a exposure distribution (this information is linked to the proportion of population exposed).
#' @param cutoff_central,cutoff_lower,cutoff_upper \code{Numeric value} showing the central exposure cut-off in \eqn{µg/m^3} and (optionally) the corresponding lower and upper 95\% confidence interval bounds.
#' The cutoff level refers to the exposure level below which no health effects occur.
#' @param population code{Vector} providing the population (in each geographical unit).
#' @param prop_pop_exp \code{Numeric value} or \code{Numeric vector} specifying the fraction(s) (value from 0 until and including 1) of the total population exposed to each exposure categories.
#' If exposure categories are used, the dimension of this input must be the same as in the \code{exp_...} argument(s). Default: 1 for a single exposure value.
#' @param bhd_central,bhd_lower,bhd_upper \code{Numeric value(s)} providing the basline (incidence) level of the health outcome in the study population and (optionally) the corresponding lower bound and the upper 5\% confidence interval bounds.
#' @param dw_central,dw_lower,dw_upper \code{Numeric value(s)} providing the disability weight associated with the morbidity health outcome and (optionally) the corresponding lower bound and the upper 5\% confidence interval bounds.
#' @param duration_central,duration_lower,duration_upper \code{Numeric value(s)} providing the disease duration in years and (optionally) the corresponding lower and upper bounds of the 95\% confidence interval (default of \code{duratoin_central} is 1 year, which is aligned with the prevalence-based approach , while a value above 1 year corresponds to the incidence-based approach (Kim, 2022, https://doi.org/10.3961/jpmph.21.597).
#' @param geo_id_disaggregated \code{Numeric vector} providing the unique ID codes of each geographic area (e.g. municipalities) considered in the assessment.
#' If a vector is entered here, the data for geo-specific input data (e.g. \code{bhd_...}), \code{exp_...}) has to be provided as a list.
#' @param geo_id_aggregated \code{Numeric vector} an ID for each geographic area that specifies which geographic areas should be aggregated together.
#' E.g. if you provide the municipality names to \code{geo_id_disaggregated}, you might provide here the corresponding region / canton / province.
#' Consequently The vector has to have the same length as the one fed to \code{geo_id_disaggregated}.
#' @param approach_multiexposure \code{String} specifying the approach that has to be used in assessments with multiple exposures. To choose among: "additive", "multiplicative" or "combined". Default: \code{NULL} (i.e. no other exposure is considered besides the one specified in the \code{exp.._ argument(s)})
#' @param info \code{String} or {data.frame} providing additional information linked with the assessment.
#' The suffix "info" will be added to the column name.
#' Default value = \code{NULL}.
# Lifetable parameters
#' @param approach_exposure \code{String} specifying whether exposure is constant or only in one year. Options: "single_year" (default), "constant".
#' @param approach_newborns \code{String} specifying whether newborns are to be considered in the years after the year of analysis. Options: "without_newborns" (default), "with_newborns". If "with_newborns" is selected, it is assumed that for each year after the year of analysis n babies are born, with n being equal to the (male and female) population aged 0 that is provided in the arguments \code{population_midyear_male} and \code{population_midyear_female}.
#' @param first_age_pop \code{Numeric value} specifying the age of the youngest age group from population and life table data (age interval = 1 year).
#' @param last_age_pop \code{Numeric value} specifying the age of the oldest age group from population and life table data (age interval = 1 year).
#' @param population_midyear_male,population_midyear_female \code{Numeric vector} containing the mid-year population for the year of analysis for female and male, respectively.
#' @param year_of_analysis \code{Numeric value} providing the year of analysis.
#' @param time_horizon \code{Numeric value} corresponding to time horizon (number of years) for which the impacts of exposure are to be considered.
#' For example, would be 10 if one is interested in the impacts of exposure during the year of analysis and the next 9 years.
#' @param min_age \code{Numberic value} specifying the minimum age for which the exposure will affect the exposed population. By default 30, which implies that all adults aged 30 or older will be affected by the exposure.
#' @param max_age \code{Numberic value} specifying the maximum age until which age the population will be affected by the environmental exposure.
#' @param health_outcome \code{String} specifying the (input and) outcome metric to assess attributable health impacts. To choose between "same_input_output" (default), "yld", "deaths_from_lifetable", "yll_from_lifetable", "yld_from_lifetable" and "daly_from_lifetable".
#' @returns
#' This function returns two lists: 1) \code{health_main}, which contains a tibble with the main results and
#' 2) \code{health_detailed}, which contains detailed (and interim) results.
#' The result tibbles include columns such as:
#' \itemize{
#'  \item pop_fraction (population attributable fraction; only for assessments using relative risk)
#'  \item impact (health impact)
#'  \item And many more.
#'  }
#'
#' @author Alberto Castro
#' @export

attribute <-
  function(approach_risk = "relative_risk",
           ## Risk and shape arguments
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           rr_increment = NULL, erf_shape = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           prop_pop_exp = 1,
           ## Other central input
           exp_central, exp_lower = NULL, exp_upper = NULL,
           cutoff_central, cutoff_lower = NULL, cutoff_upper = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           ## Iteration arguments
           geo_id_disaggregated = NULL,
           geo_id_aggregated = NULL,
           ## Multiexposure
           approach_multiexposure = NULL,
           ## Lifetable arguments
           population_midyear_male = NULL, population_midyear_female = NULL,
           deaths_male = NULL, deaths_female = NULL, # For AirQ+ method for lifetable
           first_age_pop = NULL, last_age_pop = NULL,
           min_age = NULL, max_age = NULL,
           year_of_analysis = NULL,
           dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
           duration_central = NULL, duration_lower = NULL, duration_upper = NULL,
           approach_exposure = NULL,
           approach_newborns = NULL,
           time_horizon = NULL,
           ## Meta-information
           population = NULL,
           health_outcome = "same_input_output",
           info = NULL){

    # Check input data
    #stopifnot(exprs = {
    #length(exp) == length(prop_pop_exp)
    #})


    # Capture all arguments and values
    input_args <- as.list(environment())

    # Compile input data
    input_table <-
      healthiar:::compile_input(
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
        geo_id_disaggregated = geo_id_disaggregated,
        geo_id_aggregated = geo_id_aggregated,
        info = info,
        health_outcome = health_outcome,
        population = population,
        # YLD
        dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
        duration_central = duration_central, duration_lower = duration_lower, duration_upper = duration_upper,
        # Lifetable arguments if needed
        approach_exposure = approach_exposure,
        approach_newborns = approach_newborns,
        year_of_analysis = year_of_analysis,
        time_horizon = time_horizon,
        min_age = min_age,
        max_age = max_age,
        first_age_pop =  first_age_pop,
        last_age_pop = last_age_pop,
        population_midyear_male = population_midyear_male,
        population_midyear_female =  population_midyear_female,
        deaths_male = deaths_male,
        deaths_female = deaths_female)

    # Calculate the health impacts for each case (uncertainty, category, geo area...)
    impact_raw <-
      healthiar:::get_impact(input_table = input_table,
                             pop_fraction_type = "paf")

    # Get the main and detailed output by aggregating and/or filtering cases (rows)
    output <-
      healthiar:::get_output(input_args = input_args,
                             input_table = input_table,
                             impact_raw = impact_raw)

    return(output)
  }
