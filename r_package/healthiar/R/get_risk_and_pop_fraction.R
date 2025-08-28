#' Get input data and PAF

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the population attributable fraction (PAF) based on the input data and puts the results in additional columns joined to the input data frame.

# ARGUMENTS ####################################################################
#' @param input_table \code{Data frame} with the input data
#' @param pop_fraction_type \code{String} indicating the type of the population fraction. Options: "paf" or "pif"

# VALUE ########################################################################
#' @returns
#' This function returns a \code{data.frame} with the input data adding a column for the population attributable fraction
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



get_risk_and_pop_fraction <-
  function(input_table,
           pop_fraction_type){

    # Define useful variables #################
    # To be used below
    ci_cols <-
      c("erf_ci", "exp_ci", "bhd_ci", "cutoff_ci",
        "dw_ci", "duration_ci", "erf_eq_ci")

    names_input_table <- base::names(input_table)

    ci_cols_available <-
      base::intersect(ci_cols, names_input_table)


    is_multiexposure <-
      "approach_multiexposure" %in% names_input_table

    is_multiexposure_multiplicative <-
      is_multiexposure &&
      base::unique(input_table$approach_multiexposure) %in% "multiplicative"

    is_multiexposure_combined <-
      is_multiexposure &&
      base::unique(input_table$approach_multiexposure) %in% "combined"

    grouping_cols <-
      c(ci_cols,
        "geo_id_micro", "exp_name", "sex", "age_group", "erf_eq")

    grouping_cols_available <-
      base::intersect(grouping_cols, names_input_table)

    # Remove exp_name from grouping_cols_available
    # because they have to be merged
    grouping_cols_available_multiexposure <-
      base::setdiff(grouping_cols_available, c("exp_name"))



    # Define helper function ###################################################

    # This function enables the collapse of the data frame to have only one row
    # The columns with the same values inside will be condensed: e.g. c(1,1,1) = 1
    # The values in columns with different values are pasted: e.g. c(1,2,3) = "1, 2, 3"
    # The variable columns_for_group refers to the column that is used to group the collapse

    collapse_df_by_columns <-
      function(df, columns_for_group){

        collapsed_df <-
          df |>
          dplyr::summarize(
            .by = dplyr::all_of(columns_for_group),
            dplyr::across(
              .cols = dplyr::everything(),
              .fns = ~ if (base::length(base::unique(.x)) == 1) {
                dplyr::first(.)
                } else {
                  base::toString(.)}))
      }


    # Determine risk at observed exposures #####################################

    # Check if erf_eq is NULL before going into get_risk
    # Otherwise the variable is created without value and cannot be evaluated
    # We need to know erf_eq is NULL if statements within get_risk
    if ( !base::any(base::grepl("erf_eq", names_input_table)) ) {
      erf_eq <- NULL }

    input_with_risk_and_pop_fraction <-
      input_table |>
      ## Add pop fraction type
      dplyr::mutate(pop_fraction_type = pop_fraction_type)

      ## If PAF
    if (pop_fraction_type == "paf" ) {

      input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        ## Obtain the relative risk for the relevant concentration
        dplyr::mutate(rr_at_exp =
                        healthiar::get_risk(rr = rr,
                                            exp = exp,
                                            cutoff = cutoff,
                                            rr_increment = rr_increment,
                                            erf_shape = erf_shape,
                                            erf_eq = erf_eq))

      ## If PIF
    } else {
      input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        dplyr::mutate(rr_at_exp_scen_1 =
                        healthiar::get_risk(rr = rr,
                                            exp = exp_scen_1,
                                            cutoff = cutoff,
                                            rr_increment = rr_increment,
                                            erf_shape = erf_shape,
                                            erf_eq = erf_eq),
                      rr_at_exp_scen_2 =
                        healthiar::get_risk(rr = rr,
                                            exp = exp_scen_2,
                                            cutoff = cutoff,
                                            rr_increment = rr_increment,
                                            erf_shape = erf_shape,
                                            erf_eq = erf_eq))
      }

    # * If multi-exposure with multiplicative approach ###############################################
    if (is_multiexposure_multiplicative) {

      # In the multiplicative approach, relative risks have to be merged
      # by multiplying across different exposures
      # if PAF
      if(pop_fraction_type == "paf"){

        input_with_risk_and_pop_fraction <-
          input_with_risk_and_pop_fraction |>
          ## group by columns that define diversity
          ## Only combine pm2.5 and no2 for rr_at_exp in the same ci |>
          # prod() multiplies all elements in a vector
          dplyr::mutate(
            .by = dplyr::all_of(ci_cols_available),
            rr_at_exp_before_multiplying = rr_at_exp,
            rr_at_exp = base::prod(rr_at_exp))

        # if PIF
        } else {
        input_with_risk_and_pop_fraction <-
          input_with_risk_and_pop_fraction |>
          ## group by columns that define diversity
          ## Only combine pm2.5 and no2 for rr_at_exp in the same ci
          ## prod() multiplies all elements in a vector
          dplyr::mutate(
            .by = dplyr::all_of(ci_cols_available),
            rr_at_exp_scen_1_before_multiplying = rr_at_exp_scen_1,
            rr_at_exp_scen_2_before_multiplying = rr_at_exp_scen_2,
            rr_at_exp_scen_1 = base::prod(rr_at_exp_scen_1),
            rr_at_exp_scen_2 = base::prod(rr_at_exp_scen_2))
        }

      # Data wrangling for multiple exposures
      # Collapse data frame pasting the columns with different values


      input_with_risk_and_pop_fraction <-
        input_with_risk_and_pop_fraction |>
        collapse_df_by_columns(columns_for_group = grouping_cols_available_multiexposure)

    }

    # Calculate PAF/PIF ########################################################

    # * PAF ####################################################################

    if ( pop_fraction_type == "paf" ) {

      input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        dplyr::mutate(
          .by = dplyr::all_of(grouping_cols_available),
          pop_fraction =
            healthiar:::get_pop_fraction(rr_at_exp_1 = rr_at_exp,
                                         rr_at_exp_2 = 1,
                                         prop_pop_exp_1 = prop_pop_exp,
                                         prop_pop_exp_2 = prop_pop_exp))

    # * PIF ####################################################################

      } else {
        input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        dplyr::mutate(
          .by = dplyr::all_of(grouping_cols_available),
          pop_fraction =
            healthiar:::get_pop_fraction(rr_at_exp_1 = rr_at_exp_scen_1,
                                         rr_at_exp_2 = rr_at_exp_scen_2,
                                         prop_pop_exp_1 = prop_pop_exp_scen_1,
                                         prop_pop_exp_2 = prop_pop_exp_scen_2)) }
    # * If multiexposure with combined approach #################################

    if(is_multiexposure_combined){

      input_with_risk_and_pop_fraction <-
        input_with_risk_and_pop_fraction |>
        ## group by columns that define diversity
        ## Only combine pm2.5 and no2 for rr_at_exp in the same ci
        dplyr::mutate(
          .by = dplyr::all_of(ci_cols_available),
          pop_fraction_before_combining = pop_fraction,
          ## Multiply with prod() across all pollutants
          pop_fraction = 1-(prod(1-pop_fraction)))

      ## Data wrangling for multiple exposures
      ## Collapse data frame pasting the columns with different values
      input_with_risk_and_pop_fraction <-
        input_with_risk_and_pop_fraction |>
        collapse_df_by_columns(
          columns_for_group = grouping_cols_available_multiexposure)
      }


    # Prepare output ###########################################################

    ## Only if exposure distribution (multiple exposure categories)
    ## then reduce the number of rows to keep the same number as in rr
    if(base::unique(input_table$exp_type) == "exposure_distribution"){

      input_with_risk_and_pop_fraction <-
        collapse_df_by_columns(df = input_with_risk_and_pop_fraction,
                               columns_for_group = grouping_cols_available)
    }

    return(input_with_risk_and_pop_fraction)

  }
