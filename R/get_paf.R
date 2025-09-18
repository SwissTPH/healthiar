#' Get population attributable fraction

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the population attributable fraction (PAF) of a health outcome due to exposure to an environmental stressor

# ARGUMENTS ####################################################################
#' @inheritParams attribute_master
#' @param rr_at_exp \code{Numerical value} Risk estimate of the concentration response function for a specific concentration. The population attributable fraction is normally calculated using the risk estimate that refers to the concentration that reflects the population exposure and the cut-off. This risk estimate is obtained after re-scaling from the epidemiological study with a particular increment (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.

# VALUE ########################################################################
#' @returns
#' This function returns a \code{value} corresponding to the population attributable fraction

# EXAMPLES #####################################################################
#' @examples
#' # Goal: calculate PAF based on RR and the proportion of population exposed
#' get_paf(rr = 1.062, prop_pop_exp = 1)

#' @author Alberto Castro & Axel Luyten

#' @export

get_paf <-
  function(rr_at_exp, prop_pop_exp){
    # Sources:
    # WHO 2003 a: Prüss-Üstün_2003_Assessing the environmental burden of disease at national and local levels)
    # WHO 2003 b: Murray_2003_Comparative quantification of health risks Conceptual framework and methodological issues
    # GBD 2019
    # paf <- (sum(prop_pop_exp * (rr_at_exp-1))) / (1+(sum(prop_pop_exp *(rr_at_exp-1))))

    # Instead of calculating PAF as above, just use the PIF
    # but with no effect in the second scenario
    # (same result using paf and pif for comparison with no effect)

    paf <-
      get_pop_fraction(rr_at_exp_1 = rr_at_exp,
                                 rr_at_exp_2 = rep(1, length(rr_at_exp)),
                                 prop_pop_exp_1 = prop_pop_exp,
                                 prop_pop_exp_2 = rep(1, length(prop_pop_exp)))



    return(paf)
  }


