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
          dplyr::select(exp_ci, bhd_ci, erf_ci,
                        pop_fraction, impact,
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

    # * Single geo unit ########################################################
    if ( ( unique(impact_raw$approach_risk) == "relative_risk" ) &
         ( unique(impact_raw$exposure_type) == "exposure_distribution" ) &
         ( !unique(impact_raw$is_lifetable) ) &
         ( max(impact_raw$geo_id_disaggregated) == 1 ) ) {


      input_table_to_join <-
        input_table |>
        dplyr::group_by(
          dplyr::across(
            # if cutoff argument not specified in attribute argument call then "cutoff_ci" is not used to group
            dplyr::any_of(c("exp_ci", "erf_ci", "bhd_ci", "cutoff_ci",
                            "geo_id_disaggregated")))) |>
        dplyr::summarize(exp = list(exp),
                         prop_pop_exp = list(prop_pop_exp),
                         exposure_dimension = list(exposure_dimension),
                         .groups = "drop") |>
        dplyr::select(c(exp_ci, erf_ci, exp, prop_pop_exp, exposure_dimension)) |>
        base::unique()

        impact_raw <- impact_raw |>
          dplyr::select(-c(exp, prop_pop_exp, exposure_dimension)) |>
          dplyr::left_join(
            x = _,
            y = input_table_to_join,
            by = c("exp_ci", "erf_ci"))|>
          dplyr::mutate(exposure_type = base::unique(input_table$exposure_type))

      # * Multiple geo units ###################################################

    } else if ( ( unique(impact_raw$approach_risk) == "relative_risk" ) &
                ( unique(impact_raw$exposure_type) == "exposure_distribution" ) &
                ( !unique(impact_raw$is_lifetable) ) &
                ( max(impact_raw$geo_id_disaggregated) > 1 ) ) {

      input_table_to_join <-
        input_table |>
        dplyr::group_by(dplyr::across(dplyr::any_of(c("exp_ci", "erf_ci", "bhd_ci", "cutoff_ci", "geo_id_disaggregated")))) |> # if cutoff argument not specified in attribute argument call then "cutoff_ci" is not used to group
        dplyr::summarize(exp = list(exp),
                         prop_pop_exp = list(prop_pop_exp), # Introduced error in ar pathway
                         exposure_dimension = list(exposure_dimension),
                         .groups = "drop") |>
        dplyr::select(c(exp_ci, erf_ci, exp, prop_pop_exp, exposure_dimension, geo_id_disaggregated)) |>
        base::unique()


      impact_raw <- impact_raw |>
        dplyr::select(-c(exp, prop_pop_exp, exposure_dimension)) |>
        dplyr::left_join(
          x = _,
          y = input_table_to_join,
          by = c("exp_ci", "erf_ci", "geo_id_disaggregated")
        )|>
        dplyr::mutate(exposure_type = input_table$exposure_type |> dplyr::first())

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
