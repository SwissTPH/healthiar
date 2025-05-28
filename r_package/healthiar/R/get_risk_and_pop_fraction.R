#' Get input data and PAF

#' @description
#' This function calculates the population attributable fraction (PAF) based on the input data and puts the results in additional columns joined to the input data frame.

#' @param input_table \code{Data frame} with the input data
#' @param pop_fraction_type \code{String} indicating the type of the population fraction. Options: "paf" or "pif"

#' @returns
#' This function returns a \code{data.frame} with the input data adding a column for the population attributable fraction
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

#' @note Experimental function

#' @keywords internal



get_risk_and_pop_fraction <-
  function(input_table,
           pop_fraction_type){


    # Define helper function ###################################################

    # This function enables the collapse of the data frame to have only one row
    # The columns with the same values inside will be condensed: e.g. c(1,1,1) = 1
    # The values in columns with different values are pasted: e.g. c(1,2,3) = "1, 2, 3"
    # The variable columns_for_group refers to the column that is used to group the collapse
    # The variable sep refers to the string to be used to collapse different values
    collapse_df_by_columns <-
      function(df, columns_for_group, sep){

        df <-
          df |>
          # group_by requires across() to use any_of()
          dplyr::group_by(dplyr::across(dplyr::any_of(columns_for_group)))|>
          dplyr::summarize(
            dplyr::across(dplyr::everything(),
                   ~ if (base::length(base::unique(.)) == 1) {
                     dplyr::first(.)
                   } else {
                     base::paste(collapse = sep)}),
            .groups = "drop")
      }


    # Determine risk at observed exposures #####################################

    # Check if erf_eq is NULL before going into get_risk
    # Otherwise the variable is created without value and cannot be evaluated
    # We need to know erf_eq is NULL if statements within get_risk
    if ( !base::any(base::grepl("erf_eq", base::names(input_table))) ) {
      erf_eq <- NULL }

    input_with_risk_and_pop_fraction <-
      input_table |>
      ## Add pop fraction type
      dplyr::mutate(pop_fraction_type = pop_fraction_type)

      ## If PAF
    if ( {{pop_fraction_type}} == "paf" ) {

      # browser()
      input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        ## Obtain the relative risk for the relevant concentration
        dplyr::rowwise() |>
        dplyr::mutate(rr_at_exp =
                        healthiar::get_risk(rr = rr,
                                           exp = exp,
                                           cutoff = cutoff,
                                           rr_increment = rr_increment,
                                           erf_shape = erf_shape,
                                           erf_eq = erf_eq)) |>
        dplyr::ungroup()

      ## If PIF
    } else {
      input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        dplyr::rowwise() |>
        dplyr::mutate(rr_at_exp_1 =
                        healthiar::get_risk(rr = rr,
                                           exp = exp_1,
                                           cutoff = cutoff,
                                           rr_increment = rr_increment,
                                           erf_shape = erf_shape,
                                           erf_eq = erf_eq),
                      rr_at_exp_2 =
                        healthiar::get_risk(rr = rr,
                                           exp = exp_2,
                                           cutoff = cutoff,
                                           rr_increment = rr_increment,
                                           erf_shape = erf_shape,
                                           erf_eq = erf_eq)) |>
        dplyr::ungroup()
      }

    # * Correction for multiexposure  ###############################################
    if ( "approach_multiexposure" %in% base::names(input_table) ) {

      # * * Multiplicative approach ############################################


      if ( base::unique(input_table$approach_multiexposure) %in% "multiplicative" ) {

        ## In the multiplicative approach, relative risks have to be merged
        ## by multiplying across different exposures
        if({{pop_fraction_type}} == "paf"){
          input_with_risk_and_pop_fraction <-
            input_with_risk_and_pop_fraction |>
            ## group by columns that define diversity
            ## Only combine pm2.5 and no2 for rr_at_exp in the same ci
            dplyr::group_by(
              dplyr::across(dplyr::any_of(c(
                "erf_ci",
                "exp_ci",
                "bhd_ci",
                "cutoff_ci",
                "dw_ci",
                "duration_ci",
                "erf_eq_ci")))) |>
            # prod() multiplies all elements in a vector
            dplyr::mutate(
              rr_at_exp_before_multiplying = rr_at_exp,
              rr_at_exp = base::prod(rr_at_exp))

          } else { ## if PIF
          input_with_risk_and_pop_fraction <-
            input_with_risk_and_pop_fraction |>
            ## group by columns that define diversity
            ## Only combine pm2.5 and no2 for rr_at_exp in the same ci
            dplyr::group_by(
              dplyr::across(dplyr::any_of(c(
                "erf_ci",
                "exp_ci",
                "bhd_ci",
                "cutoff_ci",
                "dw_ci",
                "duration_ci",
                "erf_eq_ci")))) |>
            ## prod() multiplies all elements in a vector
            dplyr::mutate(
              rr_at_exp_1_before_multiplying = rr_at_exp_1,
              rr_at_exp_2_before_multiplying = rr_at_exp_2,
              rr_at_exp_1 = base::prod(rr_at_exp_1),
              rr_at_exp_2 = base::prod(rr_at_exp_2))
          }

        ## Data wrangling for multiple exposures
        ## Collapse data frame pasting the columns with different values
        input_with_risk_and_pop_fraction <-
          input_with_risk_and_pop_fraction |>
          dplyr::mutate(exposure_name = base::paste(base::unique(exposure_name), collapse = ", ")) |>
          collapse_df_by_columns(
            columns_for_group = c(
              "geo_id_disaggregated",
              "sex",
              "lifetable_with_pop_nest",
              "rr_at_exp"),
            sep = ", ")
      }
    }

    # Calculate PAF/PIF ########################################################

    ## Calculate population (attributable or impact) fraction (PAF or PIF)
    cols_uncertainty <-
      names(input_with_risk_and_pop_fraction)[base::grepl("_ci", names(input_with_risk_and_pop_fraction))]

    likely_columns_to_group_input <-
      c("geo_id_disaggregated",
        "exposure_name",
        cols_uncertainty)

    available_columns_to_group_input <-
      likely_columns_to_group_input[likely_columns_to_group_input %in%
                                      base::names(input_with_risk_and_pop_fraction)]



    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>

      ## Group by different pathways and geo_units
      dplyr::group_by(dplyr::across(dplyr::any_of(available_columns_to_group_input)))


    # * PAF ####################################################################

    if ( {{pop_fraction_type}} == "paf" ) {

      input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        dplyr::mutate(
          pop_fraction =
            healthiar:::get_pop_fraction(rr_at_exp_1 = rr_at_exp,
                                       rr_at_exp_2 = 1,
                                       prop_pop_exp_1 = prop_pop_exp,
                                       prop_pop_exp_2 = prop_pop_exp))

    # * PIF ####################################################################

      } else {
        input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
        dplyr::mutate(
          pop_fraction =
            healthiar:::get_pop_fraction(rr_at_exp_1 = rr_at_exp_1,
                                       rr_at_exp_2 = rr_at_exp_2,
                                       prop_pop_exp_1 = prop_pop_exp_1,
                                       prop_pop_exp_2 = prop_pop_exp_2)) }

    ## Ungroup
    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
      dplyr::ungroup()

    # * Correction for multiexposure ###########################################

    if("approach_multiexposure" %in% base::names(input_table)){
      if(base::unique(input_table$approach_multiexposure) %in% "combined"){

        input_with_risk_and_pop_fraction <-
          input_with_risk_and_pop_fraction |>
          ## group by columns that define diversity
          ## Only combine pm2.5 and no2 for rr_at_exp in the same ci
          dplyr::group_by(
            dplyr::across(dplyr::any_of(c(
              "erf_ci",
              "exp_ci",
              "bhd_ci",
              "cutoff_ci",
              "dw_ci",
              "duration_ci",
              "erf_eq_ci")))) |>
          dplyr::mutate(pop_fraction_before_combining = pop_fraction,
                        ## Multiply with prod() across all pollutants
                        pop_fraction = 1-(prod(1-pop_fraction)))


        ## Data wrangling for multiple exposures
        ## Collapse data frame pasting the columns with different values
        input_with_risk_and_pop_fraction <-
          input_with_risk_and_pop_fraction |>
          dplyr::mutate(exposure_name = base::paste(base::unique(exposure_name), collapse = ", "),
                        exp = base::paste(base::unique(exp), collapse = ", "),
                        rr_at_exp = base::paste(base::unique(rr_at_exp), collapse = ", "),
                        pop_fraction_before_combining = base::paste(base::unique(pop_fraction_before_combining), collapse = ", ")) |>
          collapse_df_by_columns(
            columns_for_group = c(
              "geo_id_disaggregated",
              "sex",
              "lifetable_with_pop_nest",
              "pop_fraction"),
            sep = ", ")
        }
      }

    # Prepare output ###########################################################

    ## Only if exposure distribution (multiple exposure categories)
    ## then reduce the number of rows to keep the same number as in rr
    if(base::unique(input_table$exposure_type) == "exposure_distribution"){

      pop_fraction_by_exp_category <- input_with_risk_and_pop_fraction

      input_with_risk_and_pop_fraction <-
        collapse_df_by_columns(df = input_with_risk_and_pop_fraction,
                               columns_for_group = c(
                                 "geo_id_disaggregated",
                                 "exposure_name",
                                 "sex",
                                 "lifetable_with_pop_nest",
                                 "erf_ci",
                                 "exp_ci",
                                 "bhd_ci",
                                 "cutoff_ci",
                                 "dw_ci",
                                 "duration_ci",
                                 "erf_eq"),
                               sep = ", ")|>
        #Add the paf or pif by exposure category as nested tibble
        dplyr::mutate(
          pop_fraction_by_exp_category = list(pop_fraction_by_exp_category))
    }

    return(input_with_risk_and_pop_fraction)

  }
