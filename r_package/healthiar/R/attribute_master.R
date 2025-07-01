#' Attributabe health impact to an environmental stressor

#' @description
#' This INTERNAL function calculates the health impacts, mortality or morbidity, of an environmental stressor using a single value for baseline heath data, i.e. without life table.

# RR & AR
#' @param approach_risk
#' \code{String value} specifying the \strong{risk method}. Options: \code{"relative_risk"} (default) or \code{"absolute_risk"}.

#' @param exp_central,exp_lower,exp_upper
#' \code{Numeric value} or \code{numeric vector} specifying the \strong{exposure level(s)} to the environmental stressor and (optionally) the corresponding lower and upper bound of the 95\% confidence interval. See Details for more info.

#' @param cutoff_central,cutoff_lower,cutoff_upper
#' \code{Numeric value} specifying the \strong{exposure cut-off value} and (optionally) the corresponding lower and upper 95\% confidence interval bounds. Default: 0. See Details for more info.

#' @param pop_exp
#' \code{Numeric vector} specifying the absolute size of the \strong{population(s) exposed} to each exposure category. See Details for more info. \emph{Required in AR pathways; optional in RR pathways.}

#' @param erf_eq_central,erf_eq_lower,erf_eq_upper
#' \code{String} or \code{function} specifying the \strong{exposure-response function} and (optionally) the corresponding lower and upper 95\% confidence interval functions. See Details for more info. \emph{Required in AR pathways; in RR pathways required only if \code{rr_...} argument(s) not specified.}

# RR only
#' @param rr_central,rr_lower,rr_upper
#' \code{Numeric value} specifying the \strong{central relative risk} estimate and (optionally) the corresponding lower and upper 95\% confidence interval bounds. \emph{Only applicable in RR pathways; not required if \code{erf_eq_...} argument(s) already specified.}

#' @param rr_increment
#' \code{Numeric value} specifying the \strong{exposure increment} for which the provided relative risk is valid. See Details for more info. \emph{Only applicable in RR pathways; not required if \code{erf_eq_...} argument(s) already specified.}

#' @param erf_shape
#' \code{String value} specifying the \strong{exposure-response function shape} to be assumed. Options (no default): \code{"linear"}, \code{log_linear}", \code{"linear_log"}, \code{"log_log"}. \emph{Only applicable in RR pathways; not required if \code{erf_eq_...} argument(s) already specified.}

#' @param bhd_central,bhd_lower,bhd_upper
#' \code{Numeric value} or \code{numeric vector} providing the \strong{baseline health data} of the health outcome of interest in the study population and (optionally) the corresponding lower bound and the upper 95\% confidence interval bounds. See Details for more info. \emph{Only applicable in RR pathways; always required.}

#' @param prop_pop_exp
#' \code{Numeric value} or \code{numeric vector} specifying the \strong{population fraction(s) exposed} for each exposure (category). Default: 1. See Details for more info. \emph{Only applicable in RR pathways.}

# ITERATION (OPTIONAL)
#' @param geo_id_disaggregated,geo_id_aggregated
#' \code{Numeric vector} or \code{string vector} providing \strong{unique IDs of the geographic area} considered in the assessment (\code{geo_id_disaggregated}) and (optionally) providing higher-level IDs to aggregate the geographic areas (\code{geo_id_disaggregated}). See Details for more info. \emph{Only applicable in assessments with multiple geographic units.}

# META (OPTIONAL)
#' @param info
#' \code{String}, \code{data frame} or \code{tibble} providing \strong{information about the assessment}. See Details for more info. \emph{Optional argument.}

#' @param population
#' \code{Numeric value} or \code{numeric vector} specifying the total population (exposed + non-exposed), \strong{to calculate attributable impacts rate} per 100 000 population. See Details for more info. \emph{Optional argument.}

# YLD (OPTIONAL)
#' @param dw_central,dw_lower,dw_upper
#' \code{Numeric value} or \code{numeric vector} providing the \strong{disability weight} associated with the morbidity health outcome of interest and (optionally) the corresponding lower bound and the upper 95\% confidence interval bounds. \emph{Only applicable in assessments of YLD (years lived with disability).}

#' @param duration_central,duration_lower,duration_upper
#' \code{Numeric value} or \code{numeric vector} providing the \strong{duration} associated with the morbidity health outcome of interest in years and (optionally) the corresponding lower and upper bounds of the 95\% confidence interval. Default: 1. See Details for more info. \emph{Only applicable in assessments of YLD (years lived with disability).}

# Life table parameters

#' @param health_outcome
#' \code{String} specifying the desired result of the life table assessment. Options: \code{"deaths"} (premature deaths), \code{"yll"} (years of life lost).

#' @param first_age_pop,last_age_pop
#' \code{Numeric value} specifying the age of the youngest and oldest age group (age interval = 1 year) of the data, respectively.

#' @param population_midyear_female,population_midyear_male
#' \code{Numeric vector} containing the mid-year populations per age (age interval = 1 year) for the year of analysis for females and males, respectively. See Details for more info.

#' @param deaths_female,deaths_male
#' \code{Numeric vector} containing the deaths for the year of analysis per age (age interval = 1 year) for females and males, respectively.

#' @param min_age,max_age
#' \code{Numberic value} specifying the minimum and maximum age for which the exposure will affect the exposed population, respectively. Default \code{min_age}: 30. Default \code{max_age}: none. See Details for more info.

#' @param approach_exposure
#' \code{String} specifying whether exposure is constant or only in one year. Options: \code{"single_year"} (default), \code{"constant"}.

#' @param approach_newborns
#' \code{String} specifying whether newborns are to be considered in the years after the year of analysis. Options: "without_newborns" (default), "with_newborns". See Details for more info.

#' @param year_of_analysis
#' \code{Numeric value} providing the first with exposure to the environmental stressor.

#' @param time_horizon
#' \code{Numeric value} specifying the time horizon (number of years) for which the impacts of exposure are to be considered. See Details for more info.

#' @param is_lifetable
#' \code{Boolean} INTERNAL argument specifying if the life table approach is applied (TRUE) or not (FALSE)

#' @returns
#' This function returns two \code{lists}:
#' @returns
#' 1) \code{health_main}, which contains a \code{tibble} with the main results and
#' @returns
#' 2) \code{health_detailed}, which contains detailed (and interim) results.
#' @returns
#' The results contain columns such as:
#' \itemize{
#'  \item \code{pop_fraction} population attributable fraction; only applicable in relative risk assessments
#'  \item \code{impact} attributable health burden/impact
#'  \item And many more
#'  }

#' @author Alberto Castro & Axel Luyten

#' @keywords internal



attribute_master <-
  function(
    # RR & AR
    approach_risk = NULL,
    exp_central, exp_lower = NULL, exp_upper = NULL,
    cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
    pop_exp = NULL,
    erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
    # RR ONLY
    rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
    rr_increment = NULL,
    erf_shape = NULL,
    bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
    prop_pop_exp = NULL,
    # ITERATION (OPTIONAL)
    geo_id_disaggregated = NULL,
    geo_id_aggregated = NULL,
    # META (OPTIONAL)
    population = NULL,
    info = NULL,
    # YLD
    dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
    duration_central = NULL, duration_lower = NULL, duration_upper = NULL,
    # LIFE TABLE
    is_lifetable = NULL,
    health_outcome = NULL,
    first_age_pop = NULL, last_age_pop = NULL,
    population_midyear_male = NULL, population_midyear_female = NULL,
    deaths_male = NULL, deaths_female = NULL,
    min_age = NULL, max_age = NULL,
    approach_newborns = NULL,
    approach_exposure = NULL,
    year_of_analysis = NULL,
    time_horizon = NULL,
    # INTERNAL ARGUMENTS
    input_args = NULL){

    # Check input data
    healthiar:::validate_input_attribute(input_args = input_args)


    # Compile input data
    input_table <-
      healthiar:::compile_input(input_args)

    # Calculate the health impacts for each case (uncertainty, category, geo area...)
    results_raw <-
      healthiar:::get_impact(input_table = input_table,
                             pop_fraction_type = "paf")

    # Get the main and detailed output by aggregating and/or filtering cases (rows)
    output <-
      healthiar:::get_output(input_args = input_args,
                             input_table = input_table,
                             results_raw = results_raw)

    return(output)
  }
