#' Get population attributable or impact fraction

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the population attributable fraction or potential impact fraction
#' of a health outcome due to exposure to an environmental stressor

# ARGUMENTS ####################################################################
#' @param rr_at_exp_1 \code{Numerical value} showing the risk estimate of the concentration response function for a specific concentration in the scenario 1. The population attributable fraction is normally calculated using the risk estimate that refers to the concentration that reflects the population exposure and the cut-off. This risk estimate is obtained after re-scaling from the epidemiological study with a particular increment (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.
#' @param rr_at_exp_2 \code{Numerical value} showing the risk estimate of the concentration response function for a specific concentration in the scenario 2. The population attributable fraction is normally calculated using the risk estimate that refers to the concentration that reflects the population exposure and the cut-off. This risk estimate is obtained after re-scaling from the epidemiological study with a particular increment (e.g. for PM2.5 10 or 5 ug/m3) to the aimed concentration.
#' @param prop_pop_exp_1 \code{Numerical value} showing the fraction ([0,1]) of population exposed to the environmental stressor in the scenario 1. Per default = 1 (i.e. 100\% of population is exposed).
#' @param prop_pop_exp_2 \code{Numerical value} showing the fraction ([0,1]) of population exposed to the environmental stressor in the scenario 1. Per default = 1 (i.e. 100\% of population is exposed).

# DETAILS ######################################################################
#' @details
#' For more information about the equations used please see the function documentation of \code{attribute_health}.

# VALUE ########################################################################
#' @returns
#' This function returns a \code{value} corresponding to the population attributable fraction

#' @author Alberto Castro & Axel Luyten

#' @keywords internal



get_pop_fraction <-
  function(rr_at_exp_1, rr_at_exp_2, prop_pop_exp_1, prop_pop_exp_2){

    ## Sources:
    ## https://www.ncbi.nlm.nih.gov/pmc/articles/PMC156894/
    ## ETC HE Report 2023/11 (Environmental noise health risk assessment:
    ## methodology for assessing health risks using data reported under the
    ## Environmental Noise Directive), PART III: Calculation Methods,
    ## Formula 3:
    ##   AFtot = sum(p_i * (RR[Ni] - 1)) / (1 + sum(p_i * (RR[Ni] - 1)))
    ## where p_i is the proportion of the population in each exposure category.
    ## The report applies AFtot to health data of the total population

    ## pop_fraction =
    ## ( sum of the pairwise products of the vector containing the proportion of
    ## the population exposed per exposure band and the corresponding vector
    ## containing the excess risk at each exposure band, i.e. rr_at_exp - 1,
    ## in the scenario 1 ) minus ( the same sum in the scenario 2 ), divided by
    ## ( 1 plus the sum of the scenario 1 ).
    ## The 1 in the denominator accounts for the part of the population that
    ## prop_pop_exp does not cover. In air pollution assessments everybody is
    ## exposed, so the proportions add up to 1. E.g., in noise assessments they do
    ## not: only a part of the population is exposed, and the rest is unexposed
    ## and therefore has the relative risk of the reference level (1).
    ## Whenever the proportions do add up to 1, this is algebraically identical
    ## to the form used before, which divided by
    ## sum(prop_pop_exp_1 * rr_at_exp_1) instead. That form implicitly
    ## re-scaled the proportions, i.e. it dropped the unexposed part of the
    ## population instead of counting it at the reference level
    pop_fraction <-
      (sum(prop_pop_exp_1 * (rr_at_exp_1 - 1)) -
         sum(prop_pop_exp_2 * (rr_at_exp_2 - 1))) /
      (1 + sum(prop_pop_exp_1 * (rr_at_exp_1 - 1)))

    return(pop_fraction)
  }


