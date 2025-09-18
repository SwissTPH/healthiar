#' Attributable premature deaths or YLL using a life table approach

# DESCRIPTION ##################################################################
#' @description
#' This function assesses premature deaths or years of life lost (YLL) attributable to exposure to an environmental stressor using a life table approach.

# ARGUMENTS ####################################################################
#' @inheritParams attribute_master

# DETAILS ######################################################################
#' @details
#' \strong{Function arguments}
#' @details
#' \code{age_group}
#' @details
#' The numeric values must refer to 1 year age groups, e.g. \code{c(0:99)}. To convert multi-year/larger age groups to 1 year age groups use the function \code{prepare_lifetable()} (see its function documentation for more info).
#' @details
#' \code{bhd_central,bhd_lower,bhd_upper}
#' @details
#' Deaths per age must be inputted with 1 value per age (i.e. age group size = 1 year). There must be ≥ 1 deaths per age to avoid issues during the calculation of survival probabilities.
#' @details
#' \code{population}
#' @details
#' The population data must be inputted with 1 value per age (i.e. age group size = 1 year). The values must be ≥ 1 per age to avoid issues during the calculation of survival probabilities.
#' @details
#' Mid-year population of year x can be approximated as the mean of either end-year populations of years x-1 and x or start-of-year populations of years x and x+1. For each age, the inputted values must be ≥ 1 to avoid issues during the calculation of survival probabilities.
#' @details
#' \code{approach_newborns}
#' @details
#' If \code{"with_newborns"} is selected, it is assumed that for each year after the year of analysis n babies (population aged 0) are born.
#' @details
#' \code{time_horizon}
#' @details
#' Applicable for the following cases:
#' #' \itemize{
#'  \item YLL: \code{single_year} or \code{constant} exposure
#'  \item premature deaths: \code{constant} exposure
#' }
#' For example, if 10 is entered one is interested in the impacts of exposure during the year of analysis and the next 9 years (= 10 years in total). Default value: length of the numeric vector specified in the \code{age_group} argument.
#' @details
#' \code{min_age}, \code{max_age}
#' The \code{min_age} default value 30 implies that all adults aged 30 or older will be affected by the exposure; \code{max_age} analogeously specifies the age above which no health effects of the exposure are considered.

#' @details
#' \strong{Conversion of multi-year to single year age groups}
#' @details
#' To convert multi-year/larger age groups to 1 year age groups use the function \code{prepare_lifetable()} and see its function documentation for more info.

#' @details
#' \strong{Life table methodology}
#' @details
#' The life table methodology of \code{attribute_lifetable()} follows that of the WHO tool AirQ+, and is described in more detail by Miller & Hurley (2003; https://doi.org/10.1136/jech.57.3.200).
#' @details
#' In short, two scenarios are compared: 1) a scenario with the exposure level specified in the function ("exposed scenario") and 2) a scenario with no exposure ("unexposed scenario"). First, the entry and mid-year populations of the (first) year of analysis in the unexposed scenario is determined using modified survival probabilities. Second, age-specific population projections using scenario-specific survival probabilities are done for both scenarios. Third, by subtracting the populations in the unexposed scenario from the populations in the exposed scenario the premature deaths/years of life lost attributable to the exposure are determined.
#' @details
#' An expansive life table case study by Miller (2010) is available here: https://cleanair.london/app/uploads/CAL-098-Mayors-health-study-report-June-2010-1.pdf (accessed April 2025)

#' @details
#' \emph{\strong{Determination of populations in the (first) year of analysis}}
#' @details
#' The entry (i.e. start of year) populations in both scenarios is determined as follows:
#' \deqn{entry\_population_{year_1} = midyear\_population_{year_1} + \frac{deaths_{year_1}}{2}}
#' @details
#' \emph{\strong{Exposed scenario}} The survival probabilities in the exposed scenario from start of year i to start of year i+1  are calculated as follows:
#' \deqn{prob\_survival = \frac{midyear\_population_i - \frac{deaths_i}{2}}{midyear\_population_i + \frac{deaths_i}{2}}}
#' Analogously, the probability of survival from start of year i to mid-year i:
#' \deqn{prob\_survival\_until\_midyear = 1 - \frac{1 - prob\_survival}{2}}
#' @details
#' \emph{\strong{Unexposed scenario}} The survival probabilities in the unexposed scenario are calculated as follows:
#' @details
#' First, the age-group specific hazard rate in the exposed scenario is calculated using the inputted age-specific mid-year populations and deaths.
#' \deqn{hazard\_rate = \frac{deaths}{mid\_year\_population}}
#' Second, the hazard rate is multiplied with the modification factor (\eqn{= 1 - PAF}) to obtain the age-specific hazard rate in the unexposed scenario.
#' \deqn{hazard\_rate\_mod = hazard\_rate \times modification\_factor}
#' Third, the the age-specific survival probabilities (from the start until the end in a given age group) in the unexposed scenario are calculated as follows (cf. Miller & Hurley 2003):
#' \deqn{prob\_survival\_mod = \frac{2-hazard\_rate\_mod}{2+hazard\_rate\_mod}}
#' Then the mid-year populations of the (first) year of analysis (year_1) in the unexposed scenario are determined as follows:
#' @details
#' First, the survival probabilities from start of year i to mid-year i in the unexposed scenario is calculated as:
#' \deqn{prob\_survival\_until\_midyear\_{mod} = 1 - \frac{1 - prob\_survival\_mod}{2}}
#' Second, the mid-year populations of the (first) year of analysis (year_1) in the unexposed scenario is calculated:
#' \deqn{midyear\_population\_unexposed_{year_1} = entry\_population_{year_1} \times prob\_survival\_until\_midyear_{mod}}

#' @details
#' \emph{\strong{Population projection}}
#' @details
#' Using the age group-specific and scenario-specific survival probabilities calculated above, future populations of each age-group under each scenario are calculated.
#' @details
#' \emph{\strong{Unexposed scenario}} The entry and mid-year population projections of in the exposed scenario is done as follows:
#' @details
#' First, the entry population of year i+1 is calculated (which is the same as the end of year population of year i) by multiplying the entry population of year i and the modified survival probabilities.
#' \deqn{entry\_population_{i+1} = entry\_population_i \times prob\_survival\_mod}
#' Second, the mid-year population of year i+1 is calculated.
#' \deqn{midyear\_population_{i+1} = entry\_population_{i+1} \times prob\_survival\_until\_midyear}
#' @details
#' \emph{\strong{Exposed scenario}} The population projections for the two possible options of \code{approach_exposure} (\code{"single_year"} and \code{"constant"}) for the unexposed scenario are different. In the case of \code{"single_year"} exposure, the population projection for the years after the year of exposure is the same as in the unexposed scenario.
#' @details
#' In the case of \code{"constant"} the population projection is done as follows:
#' @details
#' First, the entry population of year i+1 is calculated (which is the same as the end of year population of year i) using the entry population of year i.
#' \deqn{entry\_population_{i+1} = entry\_population_i \times prob\_survival}
#' Second, the mid-year population of year i+1 is calculated.
#' \deqn{midyear\_population_{i+1} = entry\_population_{i+1} \times prob\_survival\_until\_midyear}

#' @details
#' \strong{Conversion of alternative risk measures to relative risks}
#' @details
#' For conversion of hazard ratios and/or odds ratios to relative risks refer to https://doi.org/10.1111/biom.13197 and/or use the conversion tool for hazard ratios (https://ebm-helper.cn/en/Conv/HR_RR.html) and/or odds ratios (https://ebm-helper.cn/en/Conv/OR_RR.html).

# VALUE ########################################################################
#' @inherit attribute_master return

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
#' @examples
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
#' @examples
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

