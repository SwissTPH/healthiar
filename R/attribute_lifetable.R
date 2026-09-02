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
#' The numeric values must refer to 1 year age groups, e.g. \code{c(0:99)}.
#' To convert multi-year/larger age groups to 1 year age groups use the function \code{prepare_lifetable()}
#' (see its function documentation for more info).
#'
#' \code{bhd_central,bhd_lower,bhd_upper}
#' Deaths per age must be inputted with 1 value per age (i.e. age group size = 1 year).
#' There must be greater than or equal to 1 deaths per age to avoid issues during the calculation of survival probabilities.
#' If zeros show up in the last ages (e.g. age 98 = 0 deaths, 99 years old = 1),
#' please sum the values and condensate last category (e.g. age 98 = 1).
#'
#' \code{population}
#' The population data must be inputted with 1 value per age (i.e. age group size = 1 year).
#' The values must be greater than or equal to 1 per age to avoid issues during the calculation of survival probabilities.
#' Mid-year population of year x can be approximated as the mean of
#' either end-year populations of years x-1 and x or start-of-year populations of years x and x+1.
#' For each age, the inputted values must be greater than or equal to 1
#' to avoid issues during the calculation of survival probabilities.
#'
#' \code{approach_newborns}
#' If \code{"with_newborns"} is selected, it is assumed that
#' for each year after the year of analysis n babies (population aged 0) are born.
#' 
#' \code{approach_exposure}
#' Strategy for modeling exposure over time:
#'   \itemize{
#'     \item \code{"single_year"}: Air pollution exposure is evaluated for a single year (year of analysis).
#'       Attributable premature deaths are calculated for this year only.
#'     \item \code{"constant"}: Air pollution exposure is sustained across the full projection horizon.
#'       Attributable premature deaths and YLLs are accumulated across all years within \code{time_horizon}.
#'   }
#'
#' \code{time_horizon}
#' Applicable for the following cases:
#' \itemize{
#'  \item YLL: \code{single_year} or \code{constant} exposure
#'  \item premature deaths: \code{constant} exposure
#' }
#' For example, if 10 is entered one is interested in the impacts of exposure
#' during the year of analysis and the next 9 years (= 10 years in total).
#' Default value: the number of age groups entered in the \code{age_group} argument.
#'
#' \code{min_age}, \code{max_age}
#' Both bounds are inclusive, e.g. \code{min_age = 30} implies that all adults
#' aged 30 or older will be affected by the exposure and \code{max_age = 69}
#' that no health effects are considered above the age of 69.
#' By default the exposure affects all age groups, i.e. \code{min_age} is the
#' youngest and \code{max_age} the oldest age group entered in \code{age_group}.
#'
#' \code{fraction_lived} is by default 0.5 for all age groups. However,
#' in low-mortality settings, infant deaths are heavily concentrated
#' in the first weeks of life.
#' Therefore, \code{fraction_lived} can be lower e.g. 0.1 for the first age group.
#'
#' \strong{Methodology}
#'
#' The life table approach to obtain YLL and deaths requires population and
#' baseline mortality data to be stratified by one year age groups.
#' This function applies the same approach as the on applied in the WHO tool AirQ+
#' \insertCite{WHO2020_report}{healthiar},
#' which is described in previous literature \insertCite{Miller2003_jech}{healthiar}.
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#yll-deaths-with-life-table}{YLL and deaths with life table}
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#cutoff-vs-threshold}{Cut-off vs. threshold}}
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
#'
#'
#' @seealso
#' \itemize{
#'   \item Upstream:
#'     \code{\link{prepare_exposure}} (only if no exposure data available)
#'   \item Alternative: \code{\link{attribute_health}},
#'     \code{\link{get_paf}}, \code{\link{get_risk}}
#'   \item Downstream: \code{\link{attribute_mod}}, \code{\link{compare}},
#'     \code{\link{daly}}, \code{\link{multiexpose}},
#'     \code{\link{standardize}}, \code{\link{monetize}}, \code{\link{socialize}}
#' }
#'
#' @references
#'
#' \insertAllCited{}
#'
#'
#' @author Alberto Castro & Axel Luyten
#'
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
    fraction_lived = 0.5,
    # AR & RR
    exp_central = NULL, exp_lower = NULL, exp_upper = NULL,
    cutoff_central = 0, cutoff_lower = NULL, cutoff_upper = NULL,
    threshold = NULL,
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

    # input_args$value already contains ALL arguments of this function
    # (with the same names as in attribute_master), so they are forwarded with
    # do.call() instead of one by one. In this way a new argument only has to be
    # added to the signature and not to the call below, too.
    output <-
      base::do.call(
        what = attribute_master,
        args =
          c(input_args$value,
            base::list(
              # Life table
              is_lifetable = TRUE,
              # approach_risk cannot be entered by the user 
              # in the life table approach, 
              # so it is set here
              approach_risk = "relative_risk",
              # pop_exp can only be used with absolute risk
              # and this is not compatible with life table method
              pop_exp = NULL,
              # INTERNAL ARGUMENTS
              input_args = input_args)),
        # quote = TRUE so that the argument values (e.g. a function in
        # erf_eq_central) are not evaluated a second time
        quote = TRUE)

    return(output)

  }

