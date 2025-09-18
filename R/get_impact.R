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

    # Default value of interim results
    # If there are interim results from the calculation (e.g. life table method)
    # then overwrite the value of the variable
    intermediate_calculations <- NULL

    # Impacts ###########

    # * If is_relative_risk ############################################################

    if(is_relative_risk){

      # Get pop_fraction and add it to the input_table data frame
      input_with_risk_and_pop_fraction <-
        get_risk_and_pop_fraction(input_table = input_table,
                                              pop_fraction_type = pop_fraction_type)

      if(is_not_lifetable) {
        ## ** If is_not_life_table #################################################

        # Calculate impact
        # directly with pop_fraction and bhd
        results_raw <- input_with_risk_and_pop_fraction |>
          dplyr::mutate(impact = pop_fraction * bhd)

        } else if (is_lifetable) {
          ## ** If is_lifetable ##########################################################

          # Calculate impact
          # using get_impact_with_lifetable().
          # The function is not used somewhere else
          # so itcould be integrated here, but it is kept separated
          # because it is very long and
          # would make this code not very reader friendly
          impact_with_lifetable <-
            get_impact_with_lifetable(
              input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)

          results_raw <- impact_with_lifetable$results_raw
          intermediate_calculations <- impact_with_lifetable$intermediate_calculations

        }

      } else if (is_absolute_risk) {

        # * If is_absolute_risk ##########################################################

        # Calculate absolute risk for each exposure category
        # Absolute risk is only possible without life table method
        results_raw <- input_table |>
          # To calculate health impacts with absolute risk
          # no pop_fraction is used,
          # therefore it is based on input_table instead of input_with_risk_and_pop_fraction
          dplyr::mutate(
            absolute_risk_as_percent = get_risk(exp = exp, erf_eq = erf_eq),
            impact = absolute_risk_as_percent/100 * pop_exp)
      }



    if (dw_is_available) {
      # * If YLD ################################################################
      # If dw is a column in input_table
      # it means that the user entered a value for this argument
      # and he/she wants to have YLD.
      # dw is not available as argument in life table method,
      # so no need of if condition.

      results_raw <- results_raw |>
        # Calculate the new impact multiplying by dw and duration
        dplyr::mutate(impact = impact * dw * duration)

      }

    # Rounded impact ##############
    results_raw <- results_raw |>
      dplyr::mutate(impact_rounded = base::round(impact, 0))

    # Impact per 100K inhabitants ##################################

    if(population_is_available){

      results_raw <- results_raw |>
        dplyr::mutate(impact_per_100k_inhab = (impact / population) *1E5
        )
    }

    out <- base::list(
      results_raw = results_raw,
      intermediate_calculations = intermediate_calculations
    )


  return(out)

  }
