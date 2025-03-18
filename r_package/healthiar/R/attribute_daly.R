#' Attributable disability-adjusted life years based on life tables

#' @description
#' Calculates the disability-adjusted life years attributable to the exposure to an environmental stressor using a life table approach.
#' @inheritParams attribute
#' @inherit attribute_yll_from_lifetable return
#' @author Alberto Castro & Axel Luyten
#' @note Experimental function
#' @export
attribute_daly <-
  function(
    # ## Risk and shape arguments
    # approach_risk = "relative_risk",
    # rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
    # rr_increment = NULL, erf_shape = NULL,
    # erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
    # #geo iterations
    # geo_id_disaggregated = NULL, geo_id_aggregated = NULL,
    # ## Exposure arguments
    # exp_central, exp_lower = NULL, exp_upper = NULL,
    # prop_pop_exp = 1,
    # ## Correlated exposure
    # approach_multiexposure = NULL,
    # ## Cut-off arguments
    # cutoff_central, cutoff_lower = NULL, cutoff_upper = NULL,
    # # Baseline health data
    # bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
    # ## Lifetable arguments
    # approach_exposure = "single_year", approach_newborns = "without_newborns",
    # first_age_pop, last_age_pop,
    # deaths_male = NULL, deaths_female = NULL,
    # population_midyear_male, population_midyear_female,
    # year_of_analysis,
    # min_age = NULL, max_age = NULL,
    # time_horizon = NULL,
    # ## YLD arguments
    # dw_central, dw_lower = NULL, dw_upper = NULL,
    # duration_central, duration_lower = NULL, duration_upper = NULL,
    # ## Meta-info
    # info = NULL
    output_attribute_yll,
    output_attribute_yld){

    # Capture all arguments and values
    args <- as.list(environment())

    # Store impact_raw of yll and yld
    # Shorter and handy to code
    impact_raw_yll <- output_attribute_yll[["health_detailed"]][["impact_raw"]] |>
      dplyr::filter(sex=="total")
    impact_raw_yld <- output_attribute_yld[["health_detailed"]][["impact_raw"]]

    # Capture all column names
    # They should be the same for yll and yld but just in case
    column_names_impact_raw <-
      unique(c(names(impact_raw_yll), names(impact_raw_yll)))

    common_columns <-
      column_names_impact_raw[grepl("exp|exposure|cutoff|geo",
                                    column_names_impact_raw)]
    common_columns <- common_columns[!common_columns %in% "approach_exposure"]
    common_columns <- c(common_columns, c("approach_risk", "erf_ci"))

    # Remove those containing the word impact
    column_names_impact_raw_without_impact <-
      column_names_impact_raw[!grepl("impact|lifeyears|lifetable", column_names_impact_raw)]


    # Obtain the new impact_raw for DALY
    impact_raw <-
      # Join impact_raw tables from yll and yld
      # but giving a suffix _yll and _yld to free the name "impact" to YLD
      # We need to use "impact" as final result to be consistent with the other
      # healthiar functions
      dplyr::full_join(
        impact_raw_yll,
        impact_raw_yld,
        by = common_columns,
        suffix = c("_yll", "_yld")) |>
      dplyr::mutate(
        impact = impact_yll + impact_yld,
        impact_rounded = round(impact))

    # Use args and impact to produce impact
    # input_table is not available (two branches: yll and yld) but not needed
    output <-
      healthiar:::get_output(
        args = args,
        impact_raw = impact_raw)



    # # Add argument for health_outcome
    # args_with_health_outcome <- args
    # args_with_health_outcome$health_outcome <- "same_input_output"
    #
    # # If the user enter a value for baseline health data (bhd)
    # # then this is an assessment as in attribute_health()
    # # DALY as bhd --> attributable DALY
    # if(!is.na(bhd_central)){
    #   output <-
    #     do.call(healthiar::attribute,
    #             args_with_health_outcome)
    # }


    return(output)

  }
