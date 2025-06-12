#' Attributabe health impact to an environmental stressor

#' @description
#' This INTERNAL function calculates the health impacts, mortality or morbidity, of an environmental stressor using a single value for baseline heath data, i.e. without life table.

#' @param approach_risk
#' \code{String value} specifying the risk method. Options: \code{"relative_risk"} (default) or \code{"absolute_risk"}.

#' @param erf_shape
#' \code{String value} specifying the shape of the exposure-response function to be assumed. Options (no default): \code{"linear"}, \code{log_linear}", \code{"linear_log"}, \code{"log_log"}. Argument is applicable only in RR pathway.

#' @param rr_central,rr_lower,rr_upper
#'  \code{Numeric value} specifying the central estimate of the relative risk and (optionally) the corresponding lower and upper 95\% confidence interval bounds. Argument is applicable only in RR pathway.

#' @param rr_increment
#' \code{Numeric value} specifying the concentration increment for which the provided relative risk is valid. Often 10 or 5 \eqn{µg/m^3}) (no default). Argument is applicable only in RR pathway.

#' @param erf_eq_central,erf_eq_lower,erf_eq_upper
#' \code{String} or \code{function} specifying the exposure-response function and (optionally) the corresponding lower and upper 95\% confidence interval functions. See Details for more info.

#' @param exp_central,exp_lower,exp_upper
#' \code{Numeric value} or \code{numeric vector} specifying the exposure level(s) to the environmental stressor and (optionally) the corresponding lower and upper bound of the 95\% confidence interval. See Details for more info.

#' @param pop_exp
#' \code{Numeric vector} referring to the population exposed for each exposure category. For absolute risk, the value of this argument must be entered, for relative risk it is optional.

#' @param prop_pop_exp
#' \code{Numeric value} or \code{numeric vector} specifying the fraction(s) (value from 0 until and including 1) of the total population exposed to the exposure (categories). Default: 1. See Details for more info.

#' @param cutoff_central,cutoff_lower,cutoff_upper
#' \code{Numeric value} specifying the exposure cut-off in \eqn{µg/m^3} and (optionally) the corresponding lower and upper 95\% confidence interval bounds. Default: 0. Only applicable in relative risk pathway. See Details for more info.

#' @param bhd_central,bhd_lower,bhd_upper
#' \code{Numeric value} or \code{numeric vector} providing the baseline health data of the health outcome of interest in the study population and (optionally) the corresponding lower bound and the upper 5\% confidence interval bounds. Argument is only applicable in RR pathway. See Details for more info.

#' @param geo_id_disaggregated,geo_id_aggregated
#' \code{Numeric vector} or \code{string vector} providing the unique ID codes of each geographic area considered in the assessment (\code{geo_id_disaggregated}) and (optionally) providing a higher-level ID at which the geographic areas will be aggregated (\code{geo_id_disaggregated}). Argument must be entered for iterations. See Details for more info.

#' @param population
#' \code{Numeric value} or \code{numeric vector} specifying the total population, including both exposed and non-exposed. See Details for more info.

#' @param dw_central,dw_lower,dw_upper
#' \code{Numeric value} or \code{numeric vector} providing the disability weight associated with the morbidity health outcome of interest and (optionally) the corresponding lower bound and the upper 5\% confidence interval bounds.

#' @param duration_central,duration_lower,duration_upper
#' \code{Numeric value} or \code{numeric vector} providing the duration associated with the morbidity health outcome of interest in years and (optionally) the corresponding lower and upper bounds of the 95\% confidence interval. Default: 1. Optional argument. See Details for more info.

#' @param info
#' \code{String} or \code{data frame} specifying information to be linked with the assessment. Optional argument. See Details for more info.

# Life table parameters
#' @param health_outcome
#' \code{String} specifying if the result of the assessment with life table is premature deaths ("deaths") or years of life lost ("yll")

#' @param approach_exposure
#' \code{String} specifying whether exposure is constant or only in one year. Options: "single_year" (default), "constant".

#' @param approach_newborns
#' \code{String} specifying whether newborns are to be considered in the years after the year of analysis. Options: "without_newborns" (default), "with_newborns". If "with_newborns" is selected, it is assumed that for each year after the year of analysis n babies are born, with n being equal to the (male and female) population aged 0 that is provided in the arguments \code{population_midyear_male} and \code{population_midyear_female}.

#' @param first_age_pop
#' \code{Numeric value} specifying the age of the youngest age group from population and life table data (age interval = 1 year).

#' @param last_age_pop
#' \code{Numeric value} specifying the age of the oldest age group from population and life table data (age interval = 1 year).

#' @param population_midyear_female,population_midyear_male
#' \code{Numeric vector} containing the mid-year populations per age (age interval = 1 year) for the year of analysis for females and males, respectively. See the Details section for more info.

#' @param deaths_female,deaths_male
#' \code{Numeric vector} containing the deaths for the year of analysis per age (age interval = 1 year) for females and males, respectively. See the Details section for more info.

#' @param year_of_analysis
#' \code{Numeric value} providing the year of analysis.

#' @param time_horizon
#' \code{Numeric value} corresponding to the time horizon (number of years) for which the impacts of exposure are to be considered. See the Details section for more info.

#' @param min_age
#' \code{Numberic value} specifying the minimum age for which the exposure will affect the exposed population. Default: 30. See the Details section for more info.

#' @param max_age
#' \code{Numberic value} specifying the maximum age until which age the population will be affected by the environmental exposure. No default. See the Details section for more info.

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
#'  \item \code{pop_fraction} population attributable fraction; only for assessments using relative risk
#'  \item \code{impact} attributable health burden/impact
#'  \item And many more
#'  }

#' @author Alberto Castro & Axel Luyten

#' @keywords internal



attribute_master <-
  function(is_lifetable = NULL,
           approach_risk = NULL,
           ## Risk and shape arguments
           rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
           rr_increment = NULL, erf_shape = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           pop_exp = NULL,
           prop_pop_exp = NULL,
           ## Other central input
           exp_central, exp_lower = NULL, exp_upper = NULL,
           cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           ## Iteration arguments
           geo_id_disaggregated = NULL,
           geo_id_aggregated = NULL,
           ## Lifetable arguments
           health_outcome = NULL,
           population_midyear_male = NULL, population_midyear_female = NULL,
           deaths_male = NULL, deaths_female = NULL, # For AirQ+ method for lifetable
           first_age_pop = NULL, last_age_pop = NULL,
           min_age = NULL, max_age = NULL,
           year_of_analysis = NULL,
           approach_newborns = NULL,
           time_horizon = NULL,
           # YLD arguments
           dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
           duration_central = NULL, duration_lower = NULL, duration_upper = NULL,
           approach_exposure = NULL,
           ## Meta-information
           population = NULL,
           info = NULL,
           .unused_args = NULL){

    # Capture all arguments and values
    input_args <- base::as.list(base::environment())

    # Identify the unused arguments
    # (i.e. those that were not explicitly passed by the user)
    # This is a hidden argument in attribute_master() to pass the information
    # from attribute_health() and attribute_lifetable()
    # without "polluting" input_args with a variable
    # that is not relevant for the calculation
    unused_args <- .unused_args

    # Check input data
    healthiar:::validate_input_attribute(input_args = input_args,
                                         unused_args = unused_args)


    # Compile input data
    input_table <-
      healthiar:::compile_input(input_args)

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
