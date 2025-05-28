#' Attributable health cases based on relative risk

#' @description
#' This function calculates the health impacts for each uncertainty and geo area.

#' @inheritParams attribute_master
#' @param input_table \code{Data frame} containing all input data.

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

#' @examples
#' TBD

#' @author Alberto Castro & Axel Luyten

#' @keywords internal



get_impact <-
  function(input_table,
           pop_fraction_type){

    # Relative risk ############################################################

    if(unique(input_table$approach_risk) == "relative_risk"){

      if("pop_exp" %in% names(input_table) ){

        input_with_prop_pop_exp <- input_table |>
          dplyr::group_by(geo_id_disaggregated) |>
          dplyr::mutate(prop_pop_exp = pop_exp/sum(pop_exp))

      } else{
        input_with_prop_pop_exp <- input_table
      }


      # Get pop_fraction and add to the input_table data frame
      input_with_risk_and_pop_fraction <-
        healthiar:::get_risk_and_pop_fraction(input_table = input_with_prop_pop_exp ,
                                              pop_fraction_type = pop_fraction_type)

      # * Without life table #################################################

      if(!unique(input_table$is_lifetable)) {

        # Get pop_fraction and add it to the input data frame
        impact_raw <-
          input_with_risk_and_pop_fraction |>
          # Build the result table adding the impact to the input table
          dplyr::mutate(impact = pop_fraction * bhd) |>
          # Order columns
          dplyr::select(dplyr::any_of(
            c("exp_ci", "bhd_ci", "erf_ci", "pop_fraction", "impact")),
            dplyr::everything())

      # * Lifetable ##########################################################
      } else if (unique(input_table$is_lifetable)) {

        pop_impact <-
          healthiar:::get_pop_impact(
            input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)


        impact_raw <-
          healthiar:::get_deaths_yll_from_lifetable(
            pop_impact = pop_impact,
            input_with_risk_and_pop_fraction = input_with_risk_and_pop_fraction)

      }


        # Absolute risk ##########################################################

      } else if (
        unique(input_table$approach_risk) == "absolute_risk" &
        ( !unique(input_table$is_lifetable)) ) {

        # Calculate absolute risk for each exposure category
        impact_raw <-
          input_table |>
          dplyr::rowwise() |>
          dplyr::mutate(
            absolute_risk_as_percent = healthiar::get_risk(exp = exp, erf_eq = erf_eq),
            impact = absolute_risk_as_percent/100 * pop_exp) |>
          # Remove the grouping of rowwise
          dplyr::ungroup()}




      # * YLD ################################################################
      # If dw is a column in input_table
      # it means that the user entered a value for this argument
      # and he/she wants to have YLD
      # Then convert impact into impact with dw and duration

      if ("dw" %in% names(input_table) &
          "duration" %in% names(input_table) &
          !unique(input_table$is_lifetable)) {

        impact_raw <-
          impact_raw |>
          dplyr::mutate(impact = impact * dw * duration)

      }



    # Store results ############################################################

    ## Note: column is called prop_pop_exp (rr case) or pop_exp (ar case)

    # * If exposure distribution ########################################################
    if ( ( unique(impact_raw$approach_risk) == "relative_risk" ) &
         ( unique(impact_raw$exposure_type) == "exposure_distribution" ) &
         ( !unique(impact_raw$is_lifetable) ) ) {

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

        impact_raw <- impact_raw |>
          dplyr::select(-dplyr::any_of(summary_vars)) |>
          dplyr::left_join(
            input_table_to_join,
            by = group_vars)|>
          dplyr::mutate(exposure_type = base::unique(input_table$exposure_type))
        }


    if ( ( unique(impact_raw$approach_risk) == "relative_risk" ) ) {
      impact_raw <- impact_raw |>
        dplyr::mutate(impact_rounded = round(impact, 0))
    }

    # * Calculate impact per 100K inhabitants ##################################

    if("population" %in% colnames(impact_raw)){
      impact_raw <-
        impact_raw |>
        dplyr::mutate(
          impact_per_100k_inhab = (impact / population) *1E5
        )
    }


  return(impact_raw)

  }
