#' Get population attributable fraction

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the population attributable fraction (PAF)
#' of a health outcome due to exposure to an environmental stressor

# ARGUMENTS ####################################################################
#' @inheritParams attribute_master
#' @param rr_at_exp \code{Numerical value} Risk estimate of the concentration response function for a specific concentration. The population attributable fraction is normally calculated using the risk estimate that refers to the concentration that reflects the population exposure and the cut-off. This risk estimate is obtained after re-scaling from the epidemiological study with a particular increment (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.

# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' This function is called internally inside other \code{healthiar} functions, e.g. \code{attribute_health()}.
#' The function calculates the  population attributable fraction
#' (i.e. the percent of health cases that are attributable to the exposure)
#' based on the relative risk as described in the extensive existing literature
#' \insertCite{WHO2003_report, Steenland2006-e, Lehtomaki_2025_eh}{healthiar}.
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#relative-risk}{relative risk}}
#'
# VALUE ########################################################################
#' @returns
#' This function returns the population attributable fraction as a \code{numeric value}.

# EXAMPLES #####################################################################
#' @examples
#'
#' # Goal: calculate PAF based on RR and the proportion of population exposed
#' get_paf(rr = 1.062, prop_pop_exp = 1)
#'
#'
#' @seealso
#' \itemize{
#'   \item Alternative: \code{\link{attribute_health}}, \code{\link{attribute_lifetable}}
#' }
#'
#'
#' @references
#'
#' \insertAllCited{}
#'
#'
#' @author Alberto Castro & Axel Luyten
#'
#' @export

get_paf <-
  function(rr_at_exp, prop_pop_exp){
    # paf <- (sum(prop_pop_exp * (rr_at_exp-1))) / (1+(sum(prop_pop_exp *(rr_at_exp-1))))

    # Instead of calculating PAF as above, just use the complete equations
    # (i.e. PIF form)
    # but with no effect in the second scenario
    # (same result using paf and pif for comparison with no effect)

    paf <-
      get_pop_fraction(
        rr_at_exp_1 = rr_at_exp,
        rr_at_exp_2 = base::rep(1, base::length(rr_at_exp)),
        prop_pop_exp_1 = prop_pop_exp,
        prop_pop_exp_2 = base::rep(1, base::length(prop_pop_exp)))



    return(paf)
  }


