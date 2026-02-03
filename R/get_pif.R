#' Get population impact fraction (PIF)

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the population impact fraction of a health outcome due to exposure to an environmental stressor

# ARGUMENTS ####################################################################
#' @param rr_at_exp_1 \code{Numerical value} showing the risk estimate of the concentration response function for a specific concentration in the scenario 1. The population attributable fraction is normally calculated using the risk estimate that refers to the concentration that reflects the population exposure and the cut-off. This risk estimate is obtained after re-scaling from the epidemiological study with a particular increment (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.
#' @param rr_at_exp_2 \code{Numerical value} showing the risk estimate of the concentration response function for a specific concentration in the scenario 2. The population attributable fraction is normally calculated using the risk estimate that refers to the concentration that reflects the population exposure and the cut-off. This risk estimate is obtained after re-scaling from the epidemiological study with a particular increment (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.
#' @param prop_pop_exp_1 \code{Numerical value} showing the fraction ([0,1]) of population exposed to the environmental stressor in the scenario 1. Per default = 1 (i.e. 100\% of population is exposed).
#' @param prop_pop_exp_2 \code{Numerical value} showing the fraction ([0,1]) of population exposed to the environmental stressor in the scenario 1. Per default = 1 (i.e. 100\% of population is exposed).

# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' This function is internally used inside the \code{healthiar} function \code{compare()}.
#' The population impact fraction (PIF) is calculated as
#' described in \insertCite{WHO2003_report;textual}{healthiar} and
#' \insertCite{Murray2003_spbm;textual}{healthiar}.
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#comparison-of-two-health-scenarios}{comparison of two health scenarios}}
#'
#'
#'
# VALUE ########################################################################
#' @returns
#' This function returns the population impact fraction as a \code{numeric value}.

# EXAMPLES #####################################################################
#' @examples
#' # Goal: calculate the population impact fraction (PIF)
#' results <- get_pif(
#'   rr_at_exp_1 = 1.043879,
#'   rr_at_exp_2 = 1.011217,
#'   prop_pop_exp_1 = 1,
#'   prop_pop_exp_2 = 1
#' )
#' print(results)
#'
#' @references
#'
#' \insertAllCited{}
#'
#'
#' @author Alberto Castro & Axel Luyten

#' @export


get_pif <-
  function(rr_at_exp_1, rr_at_exp_2, prop_pop_exp_1, prop_pop_exp_2){
    # Just use get_pop_fraction() with all arguments and the result is the PIF
    pif <-
      get_pop_fraction(
        rr_at_exp_1 = rr_at_exp_1,
        rr_at_exp_2 = rr_at_exp_2,
        prop_pop_exp_1 = prop_pop_exp_1,
        prop_pop_exp_2 = prop_pop_exp_2)

    return(pif)
  }


