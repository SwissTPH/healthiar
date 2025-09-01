#' Attributable premature deaths or YLL using a life table approach

# DESCRIPTION ##################################################################
#' @description
#' This function assesses premature deaths or years of life lost (YLL) attributable to exposure to an environmental stressor using a life table approach.

# ARGUMENTS ####################################################################
#' @inheritParams attribute_master

# DETAILS ######################################################################
#' @details
#' \strong{Life table methodology}
#' @details
#' The life table methodology of \code{attribute_lifetable()} follows that of the WHO tool AirQ+, and is described in more detail by Miller & Hurley (2003): https://doi.org/10.1136/jech.57.3.200.
#' @details
#' A more expansive life table case study by Miller is available here: https://cleanair.london/app/uploads/CAL-098-Mayors-health-study-report-June-2010-1.pdf (accessed April 2025)
#' @details
#' See the AirQ+ manual "Health impact assessment of air pollution: AirQ+ life table manual" for guidance on how to convert larger age groups to 1 year age groups ("section "Estimation of yearly values"): https://iris.who.int/bitstream/handle/10665/337683/WHO-EURO-2020-1559-41310-56212-eng.pdf (accessed April 2025):
#' @details
#' \strong{Conversion of alternative risk measures to relative risks}
#' For conversion of hazard ratios and/or odds ratios to relative risks refer to https://doi.org/10.1111/biom.13197 and/or use the conversion tool for hazard ratios (https://ebm-helper.cn/en/Conv/HR_RR.html) and/or odds ratios (https://ebm-helper.cn/en/Conv/OR_RR.html).

#' @details
#' \strong{Function arguments}
#' @details
#' \code{population_midyear_female},\code{population_midyear_male}
#' @details
#' Mid-year population of year x can be approximated as the mean of either end-year populations of years x-1 and x or start-of-year populations of years x and x+1. In each age group population must be ≥ 1 to avoid problems in the calculation.
#' @details
#' \code{approach_newborns}
#' @details
#' If \code{"with_newborns"} is selected, it is assumed that for each year after the year of analysis n babies (population aged 0) are born.
#' @details
#' \code{time_horizon}
#' @details
#' For example, would be 10 if one is interested in the impacts of exposure during the year of analysis and the next 9 years (= 10 years in total).
#' @details
#' \code{min_age}, \code{max_age}
#' The \code{min_age} default value 30 implies that all adults aged 30 or older will be affected by the exposure; \code{max_age} analogeously specifies the age above which no health effects of the exposure are considered.

# VALUE ########################################################################
#' @inherit attribute_master return

# EXAMPLES #####################################################################
#' @examples
#' # Goal: determine YLL attributable to air pollution exposure during one year using the life table approach
#' results <- attribute_lifetable(
#'   health_outcome = "yll",
#'   approach_exposure = "single_year",
#'   approach_newborns = "without_newborns",
#'   exp_central = 8.85,
#'   prop_pop_exp = 1,
#'   cutoff_central = 5,
#'   rr_central =  1.118,
#'   rr_increment = 10,
#'   erf_shape = "log_linear",
#'   age_group = exdat_lifetable$age_group,
#'   sex = exdat_lifetable$sex,
#'   bhd_central = exdat_lifetable$deaths,
#'   population = exdat_lifetable$midyear_population,
#'   year_of_analysis = 2019,
#'   min_age = 20
#' )
#' results$health_main$impact # Attributable YLL
#'
#' @examples
#' # Goal: determine attributable premature deaths due to air pollution exposure during one year using the life table approach
#' results_pm_deaths <- attribute_lifetable(
#'   health_outcome = "deaths",
#'   approach_exposure = "single_year",
#'   exp_central = 8.85,
#'   prop_pop_exp = 1,
#'   cutoff_central = 5,
#'   rr_central =  1.118,
#'   rr_increment = 10,
#'   erf_shape = "log_linear",
#'   age_group = exdat_lifetable$age_group,
#'   sex = exdat_lifetable$sex,
#'   bhd_central = exdat_lifetable$deaths,
#'   population = exdat_lifetable$midyear_population,
#'   year_of_analysis = 2019,
#'   min_age = 20
#' )
#' results_pm_deaths$health_main$impact # Attributable premature deaths
#'
#'
#' @examples
#' # Goal: determine YLL attributable to air pollution exposure (exposure distribution) during one year using the life table approach
#' results <- attribute_lifetable(
#'   health_outcome = "yll",
#'   exp_central = rep(c(8, 9, 10), each = 100*2), # each = length of sex or age_group vector
#'   prop_pop_exp = rep(c(0.2, 0.3, 0.5), each = 100*2), # each = length of sex or age_group vector
#'   cutoff_central = 5,
#'   rr_central = 1.118,
#'   rr_lower = 1.06,
#'   rr_upper = 1.179,
#'   rr_increment = 10,
#'   erf_shape = "log_linear",
#'   age_group = rep(
#'     exdat_lifetable$age_group,
#'     times = 3), # times = number of exposure categories
#'   sex = rep(
#'     exdat_lifetable$sex,
#'     times = 3), # times = number of exposure categories
#'   population = rep(
#'     exdat_lifetable$midyear_population,
#'     times = 3), # times = number of exposure categories
#'   bhd_central = rep(
#'     exdat_lifetable$deaths,
#'     times = 3), # times = number of exposure categories
#'   year_of_analysis = 2019,
#'   info = PM2.5,
#'   min_age = 20
#' )
#' results$health_main$impact_rounded # Attributable YLL

#' @author Alberto Castro & Axel Luyten

#' @export

attribute_lifetable <-
  function(
    # Life table
    age_group,
    sex,
    bhd_central, bhd_lower = NULL, bhd_upper = NULL,
    population,
    health_outcome = NULL,
    min_age = NULL, max_age = NULL,
    approach_exposure = "single_year",
    approach_newborns = "without_newborns",
    year_of_analysis,
    time_horizon = NULL,
    # AR & RR
    exp_central = NULL, exp_lower = NULL, exp_upper = NULL,
    cutoff_central = 0, cutoff_lower = NULL, cutoff_upper = NULL,
    erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
    pop_exp = NULL,
    # RR ONLY
    rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
    rr_increment = NULL,
    erf_shape = NULL,
    prop_pop_exp = 1,
    # ITERATION (OPTIONAL)
    geo_id_micro = "a", geo_id_macro = NULL,
    # META (OPTIONAL)
    info = NULL
  ) {

    # Get input_args
    # i.e. a list with all argument values and characteristics
    input_args <-
      healthiar:::get_input_args(environment = base::environment(),
                                 call = match.call())

    output <-
      healthiar:::attribute_master(
        # RR & AR
        approach_risk = "relative_risk",
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        pop_exp = pop_exp,
        erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
        # RR ONLY
        rr_central = rr_central, rr_lower = rr_lower, rr_upper = rr_upper,
        rr_increment = rr_increment,
        erf_shape = erf_shape,
        bhd_central = bhd_central, bhd_lower = bhd_lower, bhd_upper = bhd_upper,
        prop_pop_exp = prop_pop_exp,
        # Life table
        health_outcome = health_outcome,
        is_lifetable = TRUE,
        population = population,
        sex = sex,
        min_age = min_age, max_age = max_age,
        approach_exposure = approach_exposure,
        approach_newborns = approach_newborns,
        year_of_analysis = year_of_analysis,
        time_horizon = time_horizon,
        # ITERATION (OPTIONAL)
        geo_id_micro = geo_id_micro, geo_id_macro = geo_id_macro,
        # META (OPTIONAL)
        info = info,
        # INTERNAL ARGUMENTS
        input_args = input_args)

    return(output)

  }

