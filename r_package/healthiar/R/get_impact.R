#' Attributable health cases based on relative risk

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the health impacts for each uncertainty and geo area.

# ARGUMENTS ####################################################################
#' @inheritParams attribute_master
#' @param input_table \code{Data frame} containing all input data.

# VALUE ########################################################################
#' @returns
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central, lower and upper bound confidence interval.
#' Moreover, the data frame includes columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }

# EXAMPLES #####################################################################
#' @examples
#' # TODO

#' @author Alberto Castro & Axel Luyten

#' @keywords internal



get_impact <-
  function(input_table,
           pop_fraction_type){

    # Useful Variables ######
    # To be used in the if statements below
    is_relative_risk <- base::unique(input_table$approach_risk) == "relative_risk"
    is_absolute_risk <- base::unique(input_table$approach_risk) == "absolute_risk"

    is_lifetable <- base::unique(input_table$is_lifetable)
    is_not_lifetable <- !is_lifetable

    population_is_available <- "population" %in% base::names(input_table)
    dw_is_available <- "dw" %in% base::names(input_table)


    # * Relative risk ############################################################

    if(is_relative_risk){

      # Get pop_fraction and add to the input_table data frame
      input_with_risk_and_pop_fraction <-
        healthiar:::get_risk_and_pop_fraction(input_table = input_table,
                                              pop_fraction_type = pop_fraction_type)



      if(is_not_lifetable) {
        ## ** No life table #################################################

        # Get pop_fraction and add it to the input data frame
        results_raw <- input_with_risk_and_pop_fraction |>
          # Build the result table adding the impact to the input table
          dplyr::mutate(impact = pop_fraction * bhd)

        } else if (is_lifetable) {
          ## ** With life table ##########################################################

        pop_impact <-
          healthiar:::get_pop_impact(
            input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)


        results_raw <-
          healthiar:::get_deaths_yll_from_lifetable(
            pop_impact = pop_impact,
            input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)

        }

      } else if (is_absolute_risk) {

        # * Absolute risk ##########################################################

        # Calculate absolute risk for each exposure category
        results_raw <-
          input_table |>
          dplyr::mutate(
            absolute_risk_as_percent = healthiar::get_risk(exp = exp, erf_eq = erf_eq),
            impact = absolute_risk_as_percent/100 * pop_exp)}

    if (dw_is_available &&
        is_not_lifetable) {

      # * YLD ################################################################
      # If dw is a column in input_table
      # it means that the user entered a value for this argument
      # and he/she wants to have YLD
      # Then convert impact into impact with dw and duration

      results_raw <-
        results_raw |>
        dplyr::mutate(impact = impact * dw * duration)

      }



    # * If relative risk ##############
    if ( is_relative_risk ) {
      results_raw <- results_raw |>
        dplyr::mutate(impact_rounded = round(impact, 0))
    }

    # * Calculate impact per 100K inhabitants ##################################

    if(population_is_available){
      results_raw <-
        results_raw |>
        dplyr::mutate(
          impact_per_100k_inhab = (impact / population) *1E5
        )
    }


  return(results_raw)

  }
