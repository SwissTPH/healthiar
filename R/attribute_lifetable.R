#' Attribute premature deaths or YLL to an environmental stressor using a life table approach

# DESCRIPTION ##################################################################
#' @description
#' This function assesses premature deaths or years of life lost (YLL) attributable to exposure to an environmental stressor using a life table approach.

# ARGUMENTS ####################################################################
#' @inheritParams attribute_master

# DETAILS ######################################################################
#' @details
#'
#' \strong{Function arguments}
#'
#' \code{age_group}
#' The numeric values must refer to 1 year age groups, e.g. \code{c(0:99)}. To convert multi-year/larger age groups to 1 year age groups use the function \code{prepare_lifetable()} (see its function documentation for more info).
#'
#' \code{bhd_central,bhd_lower,bhd_upper}
#' Deaths per age must be inputted with 1 value per age (i.e. age group size = 1 year). There must be greater than or equal to 1 deaths per age to avoid issues during the calculation of survival probabilities. If zeros show up in the last ages (e.g. age 98 = 0 deaths, 99 years old = 1), please sum the values and condensate last category (e.g. age 98 = 1).
#'
#' \code{population}
#' The population data must be inputted with 1 value per age (i.e. age group size = 1 year). The values must be greater than or equal to 1 per age to avoid issues during the calculation of survival probabilities.
#' Mid-year population of year x can be approximated as the mean of either end-year populations of years x-1 and x or start-of-year populations of years x and x+1. For each age, the inputted values must be greater than or equal to 1 to avoid issues during the calculation of survival probabilities.
#'
#' \code{approach_newborns}
#' If \code{"with_newborns"} is selected, it is assumed that for each year after the year of analysis n babies (population aged 0) are born.
#'
#' \code{time_horizon}
#' Applicable for the following cases:
#' #' \itemize{
#'  \item YLL: \code{single_year} or \code{constant} exposure
#'  \item premature deaths: \code{constant} exposure
#' }
#' For example, if 10 is entered one is interested in the impacts of exposure during the year of analysis and the next 9 years (= 10 years in total). Default value: length of the numeric vector specified in the \code{age_group} argument.
#' \code{min_age}, \code{max_age}
#' The \code{min_age} default value 30 implies that all adults aged 30 or older will be affected by the exposure; \code{max_age} analogeously specifies the age above which no health effects of the exposure are considered.
#'
#' \strong{Methodology}
#'
#' Information on methodology (including equations and literature) is available in the vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#relative-risk}{relative risk}
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#absolute-risk}{absolute risk}}
#'
#'
#' \strong{Conversion of multi-year to single year age groups}
#'
#' To convert multi-year/larger age groups to 1 year age groups,
#' use the \code{healthiar} function \code{prepare_lifetable()}.
#'
# VALUE ########################################################################
#' @inherit attribute_master return
#' @note
#' For this specific function, the return object \code{health_detailed} also
#' contains \code{intermediate_calculations}. This is a nested \code{tibble}
#' containing intermediate results, such as population projections and
#' impact by age/year.
#'
# EXAMPLES #####################################################################
#' @examples
#' # Goal: determine YLL attributable to air pollution exposure during one year
#' # using the life table approach
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
#'
#' # Goal: determine attributable premature deaths due to air pollution exposure
#' # during one year using the life table approach
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
#' # Goal: determine YLL attributable to air pollution exposure (exposure distribution)
#' # during one year using the life table approach
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
      get_input_args(environment = base::environment(),
                     call = match.call())

    output <-
      attribute_master(
        # RR & AR
        approach_risk = "relative_risk",
        exp_central = exp_central, exp_lower = exp_lower, exp_upper = exp_upper,
        cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
        # pop_exp can only be used with absolute risk
        # and this is not compatible with life table method
        pop_exp = NULL,
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

