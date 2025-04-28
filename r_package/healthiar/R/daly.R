#' Attributable disability-adjusted life years

#' @description
#' This function calculates the disability-adjusted life years (DALY) attributable to the exposure to an environmental stressor by adding the two DALY components YLL and YLD.

#' @inheritParams attribute_master

#' @inherit attribute_master return

#' @author Alberto Castro & Axel Luyten

#' @note Experimental function

#' @export

daly <-
  function(
    output_attribute_yll,
    output_attribute_yld){

    # Capture all arguments and values
    input_args <- as.list(environment())


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
      column_names_impact_raw[grepl("exp|exposure|cutoff|geo|approach_risk",
                                    column_names_impact_raw)]
    common_columns <- common_columns[!common_columns %in% "approach_exposure"]
    common_columns_for_join <- c(common_columns, "erf_ci")


    common_columns_identical <-
      healthiar:::check_if_args_identical(
        args_a = input_args$output_attribute_yld,
        args_b = input_args$output_attribute_yld,
        names_to_check = common_columns)


    if(!all(common_columns_identical))
    {stop("The arguments ",
          paste(names(common_columns_identical)[common_columns_identical]
                , collapse = ", "),
          " must be identical in both scenarios")}



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
        by = common_columns_for_join,
        suffix = c("_yll", "_yld")) |>
      dplyr::mutate(
        # Add metric
        outcome_metric = "daly",
        # Add impact as sum of yll and yld (including rounded impact)
        impact = impact_yll + impact_yld,
        impact_rounded = round(impact))

    # Add impact per 100k inhabitants if population is available
    if("population" %in% names(impact_raw)){
      impact_raw <-
        impact_raw |>
        dplyr::mutate(
          impact_per_100k = (impact / population) * 1E5)
    }

    # Use args and impact to produce impact
    # input_table is not available (two branches: yll and yld) but not needed
    output <-
      healthiar:::get_output(
        input_args = input_args,
        impact_raw = impact_raw)

    return(output)

  }
