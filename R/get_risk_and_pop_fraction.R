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

    names_input_table <- names(input_table)

    is_multiexposure <-
      "approach_multiexposure" %in% names_input_table

    # any() because unique() can return more than one value, e.g. when several
    # exposure-outcome pairs are assessed in one call. && errors on a length > 1
    # right-hand side in R >= 4.3
    is_multiexposure_multiplicative <-
      is_multiexposure &&
      any(unique(input_table$approach_multiexposure) %in% "multiplicative")

    is_multiexposure_combined <-
      is_multiexposure &&
      any(unique(input_table$approach_multiexposure) %in% "combined")

    # add_info() names the column just "info" if the user entered a vector,
    # while a data frame gives info_<name entered by the user>. The bare "info"
    # is included here so that a vector-valued info identifies the subgroups in
    # the population attributable fraction below, as it already does in
    # compile_input().
    # Not in multiexposure though: there each exposure comes from its own
    # attribute_health() call and can carry its own info (e.g. "pm2.5" and
    # "no2"). That info identifies the exposures that are being merged, just
    # like exp_name does, so it must not keep them apart
    info_cols <-
      if (is_multiexposure) {
        grep("^info_", names_input_table, value = TRUE)
      } else {
        grep("^info", names_input_table, value = TRUE)
      }

    grouping_cols <-
      c(ci_cols,
        "geo_id_micro", "exp_name", "sex", "age_group", "erf_eq",
        info_cols)

    grouping_cols_available <-
      intersect(grouping_cols, names_input_table)

    # Remove exp_name from grouping_cols_available
    # because they have to be merged
    grouping_cols_available_multiexposure <-
      setdiff(grouping_cols_available, c("exp_name"))


    # Determine risk at observed exposures #####################################

    # Check if erf_eq is NULL before going into get_risk
    # Otherwise the variable is created without value and cannot be evaluated
    # We need to know erf_eq is NULL if statements within get_risk
    if ( !any(grepl("erf_eq", names_input_table)) ) {
      erf_eq <- NULL }

    # Same for threshold, which is only a column in input_table
    # if the user entered a value for it.
    # If NULL, get_risk() anchors the exposure-response function at the cutoff
    if ( ! "threshold" %in% names_input_table ) {
      threshold <- NULL }

    input_with_type <-
      input_table |>
      ## Add pop fraction type
      dplyr::mutate(pop_fraction_type = pop_fraction_type)

      ## If PAF
    if (pop_fraction_type == "paf" ) {

      input_with_risk <- input_with_type |>
        ## Obtain the relative risk for the relevant concentration
        dplyr::mutate(rr_at_exp =
                        get_risk(
                          rr = rr,
                          exp = exp,
                          cutoff = cutoff,
                          threshold = threshold,
                          rr_increment = rr_increment,
                          erf_shape = erf_shape,
                          erf_eq = erf_eq))

      ## If PIF
    } else {
      input_with_risk <- input_with_type |>
        dplyr::mutate(rr_at_exp_scen_1 =
                        get_risk(
                          rr = rr,
                          exp = exp_scen_1,
                          cutoff = cutoff,
                          threshold = threshold,
                          rr_increment = rr_increment,
                          erf_shape = erf_shape,
                          erf_eq = erf_eq),
                      rr_at_exp_scen_2 =
                        get_risk(
                          rr = rr,
                          exp = exp_scen_2,
                          cutoff = cutoff,
                          threshold = threshold,
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

        input_with_risk <-
          input_with_risk |>
          # Group by every column that identifies a row, i.e. the same key that
          # collapses the exposures below. Only exp_name is left out, because
          # that is the dimension being merged: the relative risks of pm2.5 and
          # no2 must be multiplied within one geo unit, sex, age group and ci,
          # never across them.
          # prod() multiplies all elements in a vector
          dplyr::mutate(
            .by = dplyr::all_of(grouping_cols_available_multiexposure),
            rr_at_exp_before_multiplying = rr_at_exp,
            rr_at_exp = prod(rr_at_exp))

        # if PIF
        } else {
        input_with_risk <-
          input_with_risk |>
          # Group by every column that identifies a row (see the PAF branch above)
          # prod() multiplies all elements in a vector
          dplyr::mutate(
            .by = dplyr::all_of(grouping_cols_available_multiexposure),
            rr_at_exp_scen_1_before_multiplying = rr_at_exp_scen_1,
            rr_at_exp_scen_2_before_multiplying = rr_at_exp_scen_2,
            rr_at_exp_scen_1 = prod(rr_at_exp_scen_1),
            rr_at_exp_scen_2 = prod(rr_at_exp_scen_2))
        }

      # Data wrangling for multiple exposures
      # Collapse data frame pasting the columns with different values
      input_with_risk <-
        collapse_df_by_group(
          df = input_with_risk,
          group_col_names = grouping_cols_available_multiexposure)

    }

    # Calculate PAF/PIF ########################################################

    # * PAF ####################################################################

    if ( pop_fraction_type == "paf" ) {

      input_with_risk_and_pop_fraction <- input_with_risk |>
        dplyr::mutate(
          .by = dplyr::all_of(grouping_cols_available),
          pop_fraction =
            get_pop_fraction(
              rr_at_exp_1 = rr_at_exp,
              rr_at_exp_2 = 1,
              prop_pop_exp_1 = prop_pop_exp,
              prop_pop_exp_2 = prop_pop_exp))

    # * PIF ####################################################################

      } else {
        input_with_risk_and_pop_fraction <- input_with_risk |>
        dplyr::mutate(
          .by = dplyr::all_of(grouping_cols_available),
          pop_fraction =
            get_pop_fraction(rr_at_exp_1 = rr_at_exp_scen_1,
                             rr_at_exp_2 = rr_at_exp_scen_2,
                             prop_pop_exp_1 = prop_pop_exp_scen_1,
                             prop_pop_exp_2 = prop_pop_exp_scen_2)) }
    # * If multiexposure with combined approach #################################

    if(is_multiexposure_combined){

      input_with_risk_and_pop_fraction <-
        input_with_risk_and_pop_fraction |>
        # Group by every column that identifies a row, so that the population
        # attributable fractions of pm2.5 and no2 are combined within one geo
        # unit, sex, age group and ci, never across them
        dplyr::mutate(
          .by = dplyr::all_of(grouping_cols_available_multiexposure),
          pop_fraction_before_combining = pop_fraction,
          ## Multiply with prod() across all pollutants
          pop_fraction = 1-(prod(1-pop_fraction)))

      # Data wrangling for multiple exposures
      # Collapse data frame pasting the columns with different values
      input_with_risk_and_pop_fraction <-
        collapse_df_by_group(
          df = input_with_risk_and_pop_fraction,
          group_col_names = grouping_cols_available_multiexposure)
      }


    # Prepare output ###########################################################

    # Only if exposure distribution (multiple exposure categories)
    # then reduce the number of rows to keep the same number as in rr
    # any() and not unique() because exp_type is determined per geo unit, sex,
    # age group and info, so it can differ across rows, e.g. when one
    # exposure-outcome pair has an exposure distribution and another a
    # population weighted mean. collapse_df_by_group() is a no-op for the
    # single-row groups, so widening the condition is safe
    if(any(input_table$exp_type == "exposure_distribution")){

      input_with_risk_and_pop_fraction <-
        collapse_df_by_group(
          df = input_with_risk_and_pop_fraction,
          group_col_names = grouping_cols_available)
    }

    return(input_with_risk_and_pop_fraction)

  }
