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

    is_exposure_distribution <-
      base::unique(input_table$exposure_type) == "exposure_distribution"

    population_is_available <- "population" %in% base::names(input_table)
    dw_is_available <- "dw" %in% base::names(input_table)


    # Relative risk ############################################################

    if(is_relative_risk){

      # Get pop_fraction and add to the input_table data frame
      input_with_risk_and_pop_fraction <-
        healthiar:::get_risk_and_pop_fraction(input_table = input_table,
                                              pop_fraction_type = pop_fraction_type)

      # * Without life table #################################################

      if(is_not_lifetable) {

        # Get pop_fraction and add it to the input data frame
        results_raw <- input_with_risk_and_pop_fraction |>
          # Build the result table adding the impact to the input table
          dplyr::mutate(impact = pop_fraction * bhd)

      # * With lifetable ##########################################################
      } else if (is_lifetable) {

        pop_impact <-
          healthiar:::get_pop_impact(
            input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)


        results_raw <-
          healthiar:::get_deaths_yll_from_lifetable(
            pop_impact = pop_impact,
            input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)

      }


        # Absolute risk ##########################################################

      } else if (is_absolute_risk) {

        # Calculate absolute risk for each exposure category
        results_raw <-
          input_table |>
          dplyr::mutate(
            absolute_risk_as_percent = healthiar::get_risk(exp = exp, erf_eq = erf_eq),
            impact = absolute_risk_as_percent/100 * pop_exp)}




      # * YLD ################################################################
      # If dw is a column in input_table
      # it means that the user entered a value for this argument
      # and he/she wants to have YLD
      # Then convert impact into impact with dw and duration

      if (dw_is_available &&
          is_not_lifetable) {

        results_raw <-
          results_raw |>
          dplyr::mutate(impact = impact * dw * duration)

      }



    # Store results ############################################################

    ## Note: column is called prop_pop_exp (rr case) or pop_exp (ar case)

    # * If exposure distribution ########################################################
    if ( is_relative_risk &&
         is_exposure_distribution &&
         is_not_lifetable ) {

      # Define your dynamic vectors
      group_vars <-
        names(input_table)[names(input_table) %in%
                             c("exp_ci", "erf_ci", "bhd_ci", "cutoff_ci", "geo_id_disaggregated")]

      group_vars_except_geo <-
        names(input_table)[names(input_table) %in%
                             c("exp_ci", "erf_ci", "bhd_ci", "cutoff_ci")]

      summary_vars <-
        names(input_table)[grepl("exp|prop_pop_exp|exposure_dimension", names(input_table))]
      summary_vars <- summary_vars[!summary_vars %in% group_vars]


      # Build the grouped summarization dynamically
      input_table_to_join <- input_table |>
        dplyr::group_by(dplyr::across(dplyr::any_of(group_vars))) |>
        dplyr::summarize(
          dplyr::across(
            .cols = dplyr::any_of(summary_vars),
            .fns = list,  # Wrap values into a list per group
            .names = "{.col}"  # Keep original column names
          ),
          .groups = "drop")|>
        base::unique()

        results_raw <- results_raw |>
          dplyr::select(-dplyr::any_of(summary_vars)) |>
          dplyr::left_join(
            input_table_to_join,
            by = group_vars)|>
          dplyr::mutate(exposure_type = base::unique(input_table$exposure_type))
        }


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
