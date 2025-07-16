#' Attributable health cases based on relative risk

#' @description
#' This function distributes and store outputs by level of detail by aggregating or filtering impacts.

#' @param input_args \code{List} containingall arguments and values entered in attribute().
#' @param input_table \code{List} containing the input_table data compiled and packed in a data frame.
#' @param results_raw \code{List} containing all the calculation of health impacts.

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



get_output <-
  function(input_args = NULL,
           input_table = NULL,
           results_raw){

    # Store set of columns ###################################
    # Variables to be used below

    id_columns <- c("geo_id_aggregated", "geo_id_disaggregated",
                    "exposure_name",
                    "age_group", "sex",
                    "erf_ci","exp_ci", "bhd_ci", "cutoff_ci", "dw_ci", "duration_ci")

    column_names <- base::names(results_raw)

    column_names_wo_lifetable_impact <-
      column_names[! base::grepl("nest|modification_factor|impact", column_names)]

    # This includes all columns names except age_group and sex
    group_columns_for_sex_aggregation <-
      column_names_wo_lifetable_impact[! base::grepl("sex", column_names_wo_lifetable_impact)]

    group_columns_for_age_aggregation <-
      column_names_wo_lifetable_impact[! base::grepl("age", column_names_wo_lifetable_impact)]
    # This include only the id_columns except geo_id_disaggregated
    # Not all columns like above to avoid geo_id_disaggregated variables
    # that make the summary at lower (disaggregated) geo level
    group_columns_for_geo_aggregation <-
      id_columns[! id_columns %in% c("geo_id_disaggregated")]

    group_columns_for_multiexposure_aggregation <-
      c("exposure_name", "geo_id_aggregated", "exp_ci", "bhd_ci", "erf_ci","dw_ci", "cutoff_ci", "duration_ci")

    # Deactivated code
    # It gives errors but something similar could be implemented to get the column names in a more efficient way
    # group_columns_for_absolute_risk_aggregation <-  results_raw |>
    #   dplyr::summarise(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.) == 1)) |>
    #   dplyr::select(dplyr::where(~ .x)) |>
    #   base::names()

    group_columns_for_absolute_risk_aggregation <-
      column_names[!column_names %in%
                     c(c("geo_id_disaggregated", "age_group", "sex",
                         paste0("exp", c("", "_1", "_2")),
                         paste0("population", c("", "_1", "_2")),
                         paste0("prop_pop_exp", c("", "_1", "_2")),
                         paste0("pop_exp", c("", "_1", "_2")),
                         paste0("rr_at_exp", c("", "_1", "_2")),
                         paste0("pop_fraction", c("", "_1", "_2")),
                         paste0("absolute_risk_as_percent", c("", "_1", "_2")),
                         paste0("impact", c("", "_1", "_2")),
                         paste0("impact_per_100k_inhab", c("", "_1", "_2"))))]

    impact_columns <-paste0(c("impact", "impact_rounded", "impact_per_100k_inhab",
                              "monetized_impact", "monetized_impact_rounded"),
                            rep(c("", "_1", "_2"), each = 3))

    # Get main results from detailed results ###################################

    health_detailed_from_impact  <-
      list(input_args = input_args,
           input_table = input_table,
           results_raw = results_raw)

    if(any(grepl("nest", names(results_raw)))){
      impact_main <-
        results_raw |>
        dplyr::select(-dplyr::contains("nest"))

      if ("duration_ci" %in% names(impact_main)){impact_main <- impact_main |> dplyr::filter(duration_ci %in% "central")}
      if ("dw_ci" %in% names(impact_main)){impact_main <- impact_main |> dplyr::filter(dw_ci %in% "central")}


      ## Classify results in main and detailed
      output <-
        list(health_main = impact_main,
             health_detailed = health_detailed_from_impact)


    } else {

      # Store output so far
      # The main will change below that we give a first value
      output <-
        list(health_main = results_raw,
             health_detailed = health_detailed_from_impact)
    }


    # Keep the last version
    output_last <- output[["health_main"]]

    # Create function to aggregate impacts
    # To be used multiple times below

    sum_round_and_relative_impact <- function(df, grouping_cols, col_total){

      # Sum impact columns (keep original names)
      impact_agg <- df |>
        dplyr::summarise(
          dplyr::across(
            # Important: across() because this is to be done in all impact columns
            # In attribute_health() only one impact colum
            # but get_output is also used by monetize()
            # this function also have other columns with impact discounted and monetized
            # and even comparison scenarios
            # which also have to be included in this aggregation
            # The use of matches() is important.
            # It works as contains() but allowing regex | (OR)
            .cols = dplyr::matches("impact|absolute_risk_as_percent"),
            .fns = ~ sum(.x, na.rm = TRUE),
            .names = "{.col}"
          ),
          .by = dplyr::any_of(grouping_cols)
        ) |>
        # Add ..._rounded columns
        dplyr::mutate(
          dplyr::across(
            .cols = "impact",
            .fns = ~ round(.x),
            .names = "{.col}_rounded"
          )
        )

      # If population is available, recompute with population and normalized metrics
      if ("population" %in% names(df)) {
        impact_agg <- df |>
          dplyr::summarise(
            dplyr::across(
              .cols = dplyr::matches("impact|absolute_risk_as_percent"),
              .fns = ~ sum(.x, na.rm = TRUE),
              .names = "{.col}"
            ),
            population = sum(population, na.rm = TRUE),
            .by = dplyr::any_of(grouping_cols)
          ) |>
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::matches("impact"),
              .fns = list(
                rounded = ~ round(.x),
                per_100k_inhab = ~ (.x / population) * 1e5
              ),
              .names = "{.col}_{.fn}"
            )
          )
      }

      # Add column showing that this is the total after summing
      impact_agg[, col_total] <- "total"

      return(impact_agg)

    }



    # Absolute risk ############

    if(unique(results_raw$approach_risk) == "absolute_risk") {

    output[["health_detailed"]][["results_agg_exp_cat"]] <-
        output_last |>
        # Remove all impact rounded because
        # we have to round final results
        # not summing rounded results ("too rounded")
        dplyr::select(-dplyr::any_of(paste0("impact_rounded", c("", "_1", "_2")))) |>
        dplyr::group_by(geo_id_disaggregated, age_group, sex) |>
        # Collapse the exposure categories to have only a vector
        dplyr::mutate(dplyr::across(dplyr::any_of(
          c(paste0("exp", c("", "_1", "_2")),
            paste0("pop_exp", c("", "_1", "_2")),
            paste0("prop_pop_exp", c("", "_1", "_2")),
            "exposure_dimension")),
          ~ paste(., collapse = ", "))) |>
        dplyr::ungroup()

      output[["health_detailed"]][["results_agg_exp_cat"]] <-
        sum_round_and_relative_impact(
          df = output[["health_detailed"]][["results_agg_exp_cat"]],
          grouping_cols = group_columns_for_absolute_risk_aggregation,
          col_total = "ar_exp_cat_aggregation")

      output_last <- output[["health_detailed"]][["results_agg_exp_cat"]]
    }

    output[["health_detailed"]][["results_disaggregated"]]  <-
      output_last

    # sex #####
    # Aggregate results by sex

    output[["health_detailed"]][["results_agg_sex"]] <-
      sum_round_and_relative_impact(
        df = output_last,
        grouping_cols = group_columns_for_sex_aggregation,
        col_total = "sex")


    output_last <- output[["health_detailed"]][["results_agg_sex"]]

    # age_group #####
    # Aggregate results by age_group
    output[["health_detailed"]][["results_agg_age"]] <-
      sum_round_and_relative_impact(
        df = output_last,
        grouping_cols = group_columns_for_age_aggregation,
        col_total = "age")

    output_last <- output[["health_detailed"]][["results_agg_age"]]


    # geo_id_aggregated #####
    # Aggregate results by higher geo_level
    # only if geo_id_aggregated is defined

    if("geo_id_aggregated" %in% names(output_last)){

      output[["health_detailed"]][["results_agg_geo"]] <-
        sum_round_and_relative_impact(
          df = output_last,
          grouping_cols = group_columns_for_geo_aggregation,
          col_total = "geo_aggregation")

      output_last <- output[["health_detailed"]][["results_agg_geo"]]

    }

    # Aggregate results across pollutants (exposures)
    if("approach_multiexposure" %in% names(results_raw)){
      if(unique(results_raw$approach_multiexposure) %in% "additive"){

        # Paste exposure names before aggregating
        output_last <- output_last |>
          dplyr::mutate(exposure_name = base::paste(base::unique(exposure_name), collapse = ", "))

        output[["health_detailed"]][["results_agg_multiexposure"]] <-
          sum_round_and_relative_impact(
            df = output_last,
            grouping_cols = group_columns_for_multiexposure_aggregation,
            col_total = "multiexposure_aggregation")

      output_last <- output[["health_detailed"]][["results_agg_multiexposure"]]

      }
    }


    # Keep only the ci central in main output ###########

    # Store the last output in health main before starting the loop
    output[["health_main"]] <- output_last


    # Define all the ci columns have that have to be filtered to keep only central
    ci_cols <- c("exp_ci", "bhd_ci", "cutoff_ci", "dw_ci", "duration_ci")

    # Identify which of the ci_cols are present in the assessment
    available_ci_cols <- base::intersect(
      ci_cols,
      base::names(output[["health_main"]])
    )



    # Loop by the available_ci_cols to filter them keeping only central
    for (col in available_ci_cols) {

      output[["health_main"]] <-
        output[["health_main"]] |>
        # grepl instead of %in% because it needs
        # to be flexible to also accept the central_*id_ass* in the
        # summarize_uncertainty
        dplyr::filter(base::grepl("central", output[["health_main"]][[col]]))

    }

    # Order columns ############################################################
    # putting first (on the left) those that determine different results across rows

    first_columns <- c(id_columns, impact_columns)

    put_first_cols <-
      function(x, cols){
        dplyr::select(x,
                      dplyr::any_of(cols),
                      dplyr::everything())
      }

    put_first_cols_recursive <-
      function(x, cols){

        # If x is a data.frame
        if(is.data.frame(x)){
          put_first_cols(x, cols)

        # If x is list and all list elements are data frames (and not lists)
        }else if (is.list(x) & all(purrr::map_lgl(x, is.data.frame))){
          purrr::map(
            .x = x,
            .f = ~ put_first_cols(.x, cols))

        }else{x}

      }

    output <-
      purrr::map(
        .x = output,
        .f = ~ put_first_cols_recursive(x = .x,
                                        cols = first_columns))




    return(output)
  }
