#' Attribute health impacts to an environmental stressor

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the attributable health impacts (mortality or morbidity) due to
#' exposure to an environmental stressor (air pollution or noise), using either relative risk (\strong{RR}) or absolute risk (\strong{AR}).
#'
#' Arguments for both \strong{RR & AR} pathways
#' \itemize{
#'  \item \code{approach_risk}
#'  \item \code{exp_central}, \code{exp_lower}, \code{exp_upper}
#'  \item \code{cutoff_central}, \code{cutoff_lower}, \code{cutoff_upper}
#'  \item \code{erf_eq_central}, \code{erf_eq_lower}, \code{erf_eq_upper}
#'  }
#'
#' Arguments only for \strong{RR} pathways
#' \itemize{
#'  \item \code{rr_central}, \code{rr_lower}, \code{rr_upper}
#'  \item \code{rr_increment}
#'  \item \code{erf_shape}
#'  \item \code{bhd_central}, \code{bhd_lower}, \code{bhd_upper}
#'  \item \code{prop_pop_exp}
#'  }
#'
#'  Argument for \strong{AR} pathways
#' \itemize{
#'  \item \code{pop_exp}
#'  }
#'
#'  \strong{Optional} arguments for both \strong{RR & AR} pathways
#' \itemize{
#'  \item \code{geo_id_micro}, \code{geo_id_macro},
#'  \item \code{age_group}, \code{sex}, \code{info}, \code{population}
#'  \item \code{dw_central}, \code{dw_lower}, \code{dw_upper}
#'  \item \code{duration_central}, \code{duration_lower}, \code{duration_upper}
#'  }
#'
# ARGUMENTS ####################################################################
#' @inheritParams attribute_master
#'
# DETAILS ######################################################################
#' @details
#' \strong{What you put in is what you get out}
#'
#' The health metric you put in (e.g. absolute disease cases, deaths per 100 000 population, DALYs, prevalence, incidence, ...) is the one you get out.
#'
#' \emph{Exception}: if you enter a disability weight (via the \code{dw_...} arguments) the attributable impact will be in YLD.
#'
#'
#' \strong{Function arguments}
#'
#' \code{exp_central}, \code{exp_lower}, \code{exp_upper}
#' In case of exposure bands enter only one exposure value per band (e.g. the means of the lower and upper bounds of the exposure bands).
#'
#' \code{cutoff_central}, \code{cutoff_lower}, \code{cutoff_upper}
#' The cutoff level refers to the exposure level below which no health effects occur in the same unit as the exposure. If exposure categories are used, the length of this input must be the same as in the \code{exp_...} argument(s).
#'
#' \code{pop_exp}
#' \emph{Only applicable in AR pathways; always required.} In AR pathways the population exposed per exposure category is multiplied with the corresonding category-specific risk to obtain the absolute number of people affected by the health outcome.
#'
#' \code{erf_eq_central}, \code{erf_eq_lower}, \code{erf_eq_upper}
#' \emph{Required in AR pathways; in RR pathways required only if rr_... arguments not specified.} Enter the exposure-response function as a \code{function}, e.g. output from \code{stats::splinefun()} or \code{stats::approxfun()}, or as a \code{string} formula, e.g. \code{"3+c+c^2"} (with the \emph{c} representing the concentration/exposure).
#' If you have x-axis (exposure) and y-axis (relative risk) value pairs of multiple points lying on the the exposure-response function, you could use e.g. \code{stats::splinefun(x, y, method="natural")} to derive the exposure-response function (in this example using a cubic spline natural interpolation).
#' \code{rr_increment}
#' \emph{Only applicable in RR pathways.} Relative risks from the literature are valid for a specific increment in the exposure, in case of air pollution often 10 or 5 \eqn{\mu g/m^3}).
#'
#' \code{bhd_central}, \code{bhd_lower}, \code{bhd_upper}
#' \emph{Only applicable in RR pathways.} Baseline health data for each exposure category must be entered.
#'
#' \code{prop_pop_exp}
#' \emph{Only applicable in RR pathways.} In RR pathways indicates the fraction(s) (value(s) from 0 until and including 1) of the total population exposed to the exposure categories. See equation of the population attributable fraction for categorical exposure below.
#'
#' \code{geo_id_macro}, \code{geo_id_micro}
#' \emph{Only applicable in assessments with multiple geographic units.} For example, if you provide the names of the municipalities under analysis to \code{geo_id_micro}, you might provide to \code{geo_id_macro} the corresponding region / canton / province names.
#' Consequently, the vectors fed to \code{geo_id_micro} and \code{geo_id_macro} must be of the same length.
#'
#' \code{age_group}
#' Can be either \code{numeric} or \code{character}. If it is numeric, it refers to the first age of the age group. E.g. \code{c(0, 40, 80)} means age groups \code{[0, 40), [40, 80), >=80]}.
#'
#' \code{info}
#' \emph{Optional argument.} Information entered to this argument will be added as column(s) names \code{info_1}, \code{info_2}, \code{info_...} to the results table. These additional columns can be used to further stratify the analysis in a secondary step (see example below).
#'
#' \code{population}
#' \emph{Optional argument.} The population entered here is used to determine impact rate per 100 000 population. Note the requirement for the vector length in the paragraph \emph{Assessment of multiple geographic units} below.
#'
#' \code{duration_central}, \code{duration_lower}, \code{duration_upper}
#' \emph{Only applicable in assessments of YLD (years lived with disability).} Measured in years. A value of 1 (year) refers to the prevalence-based approach, while values above 1 to the incidence-based approach.
#'
#'
#' \strong{Methodology}
#'
#' This function can quantify the attributable health impacts by means of a
#' relative risk or an absolute risk (depending on the health outcome).
#' \itemize{
#'  \item{Relative risk: The comparative risk assessment approach
#'  \insertCite{Murray2003_e}{healthiar} is applied by
#' obtaining the population attributable fraction
#' (percent of cases that are attributable to the exposure)
#' based on the relative risk \insertCite{WHO2003_report,Steenland2006-e,GBD2020_tl,Soares2020_report,Pozzer2023_gh,Lehtomaki_2025_eh}{healthiar}. }
#'  \item{Absolute risk: The attributable cases are
#'  directly derived from population exposed \insertCite{WHO2011_report}{healthiar}.}
#'  }
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#relative-risk}{Relative risk}
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#absolute-risk}{Absolute risk}}
#'
#'
# VALUE ########################################################################
#' @inherit attribute_master return
#'
#'
# EXAMPLES #####################################################################
#' @examples
#' # Goal: attribute lung cancer cases to population-weighted PM2.5 exposure
#' # using relative risk
#'
#' results <- attribute_health(
#'   erf_shape = "log_linear",
#'   rr_central = 1.369,            # Central relative risk estimate
#'   rr_increment = 10,             # per \\mu g / m^3 increase in PM2.5 exposure
#'   exp_central = 8.85,            # Central exposure estimate in \\mu g / m^3
#'   cutoff_central = 5,            # \\mu g / m^3
#'   bhd_central = 30747            # Baseline health data: lung cancer incidence
#'  )
#'
#' results$health_main$impact_rounded # Attributable cases
#'
#'
#' # Goal: attribute cases of high annoyance to (road traffic) noise exposure
#' # using absolute risk
#'
#' results <- attribute_health(
#'   approach_risk = "absolute_risk",
#'   exp_central = c(57.5, 62.5, 67.5, 72.5, 77.5),
#'   pop_exp = c(387500, 286000, 191800, 72200, 7700),
#'   erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
#' )
#'
#' results$health_main$impact_rounded # Attributable high annoyance cases
#'
#'
#' # Goal: attribute disease cases to PM2.5 exposure in multiple geographic
#' # units, such as municipalities, provinces, countries, ...
#'
#' results <- attribute_health(
#'   geo_id_micro = c("Zurich", "Basel", "Geneva", "Ticino"),
#'   geo_id_macro = c("Ger","Ger","Fra","Ita"),
#'   rr_central = 1.369,
#'   rr_increment = 10,
#'   cutoff_central = 5,
#'   erf_shape = "log_linear",
#'   exp_central = c(11, 11, 10, 8),
#'   bhd_central = c(4000, 2500, 3000, 1500)
#' )
#'
#' # Attributable cases (aggregated)
#' results$health_main$impact_rounded
#'
#' # Attributable cases (disaggregated)
#' results$health_detailed$results_raw |> dplyr::select(
#'   geo_id_micro,
#'   geo_id_macro,
#'   impact_rounded
#' )
#'
#'
#' # Goal: determine attributable YLD (years lived with disability)
#' results  <- attribute_health(
#'   exp_central = 8.85,
#'   prop_pop_exp = 1,
#'   cutoff_central = 5,
#'   bhd_central = 1000,
#'   rr_central = 1.1,
#'   rr_increment = 10,
#'   erf_shape = "log_linear",
#'   duration_central = 100,
#'   dw_central = 1,
#'   info = "pm2.5_yld"
#' )
#'
#' results$health_main$impact
#'
#'
#' @seealso
#' \itemize{
#'   \item Upstream: \code{\link{prepare_exposure}} (only if no exposure data available)
#'   \item Alternative: \code{\link{attribute_lifetable}},
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
#' @export



attribute_health <-
  function(
    # RR & AR
    approach_risk = "relative_risk",
    exp_central, exp_lower = NULL, exp_upper = NULL,
    cutoff_central = 0, cutoff_lower = NULL, cutoff_upper = NULL,
    pop_exp = NULL,
    erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
    # RR ONLY
    rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
    rr_increment = NULL,
    erf_shape = NULL,
    bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
    prop_pop_exp = 1,
    # ITERATION (OPTIONAL)
    geo_id_micro = "a", geo_id_macro = NULL,
    age_group = "all",
    sex = "all",
    ## YLD (OPTIONAL)
    dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
    duration_central = NULL, duration_lower = NULL, duration_upper = NULL,
    # META (OPTIONAL)
    info = NULL,
    population = NULL){



    # Get input_args
    # i.e. a list with all argument values and characteristics
    input_args <-
      get_input_args(environment = base::environment(),
                     call = match.call())

    output <-
      attribute_master(
        # RR & AR
        approach_risk = approach_risk,
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
        # ITERATION (OPTIONAL)
        geo_id_micro = geo_id_micro , geo_id_macro = geo_id_macro,
        age_group = age_group, # Obligatory for life table approach
        sex = sex,
        # META (OPTIONAL)
        info = info,
        population = population, # Obligatory for life table approach
        # YLD (OPTIONAL)
        dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
        duration_central = duration_central, duration_lower =  duration_lower, duration_upper = duration_upper,
        # LIFE TABLE (OPTIONAL)
        is_lifetable = FALSE,
        min_age = NULL, max_age = NULL,
        approach_exposure = NULL,
        approach_newborns = NULL,
        year_of_analysis = NULL,
        # INTERNAL ARGUMENTS
        input_args = input_args)

    return(output)


  }
