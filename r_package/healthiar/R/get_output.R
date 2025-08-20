#' Obtain and store output

# DESCRIPTION ##################################################################
#' @description
#' This function distributes and store outputs by level of detail by aggregating or filtering impacts.

# ARGUMENTS ####################################################################
#' @param input_args \code{List} containingall arguments and values entered in attribute().
#' @param input_table \code{Tibble} containing the input_table data compiled and packed in a data frame.
#' @param intermediate_calculations \code{Tibble} containing intermediate calculations (e.g. from life table pathway).
#' @param results_raw \code{Tibble} containing all the calculation of health impacts.

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


get_output <-
  function(input_args = NULL,
           input_table = NULL,
           intermediate_calculations = NULL,
           results_raw) {

    # Store set of columns ###################################
    # Variables to be used below


    id_columns <- c("geo_id_aggregated", "geo_id_disaggregated",
                    "exp_name",
                    "year",
                    "age_group", "sex",
                    "erf_ci","exp_ci", "bhd_ci", "cutoff_ci", "dw_ci", "duration_ci")

    colnames_results_raw <- base::names(results_raw)

    impact_columns <-
      colnames_results_raw[base::grepl("impact", colnames_results_raw)]

    columns_to_be_summed <- results_raw |>
      # The use of matches() is important.
      # It works as contains() but allowing regex | (OR)
      dplyr::select(dplyr::matches("impact|absolute_risk_as_percent|population"),
                    -dplyr::matches("_by_|_rounded|_per_100k_inhab")) |>
      base::names()

    # Only columns to be summed that include the string "impact"
    # This is used for per_100k_inhab
    # Use grepl() because there are many possible column names, no only impact
    # e.g. "monetized_impact"
    impact_columns_to_be_summed <-
      columns_to_be_summed[base::grepl("impact", columns_to_be_summed)]

    nest_cols <-
      colnames_results_raw[base::grepl("_by_", colnames_results_raw)]

    id_columns_in_results_raw <-
      colnames_results_raw[colnames_results_raw %in% id_columns]

    group_columns_for_year_aggregation <-
      base::setdiff(id_columns_in_results_raw, c("year"))

    group_columns_for_ar_exp_cat_aggregation <-
      base::setdiff(id_columns_in_results_raw, c("exp_ci"))

    group_columns_for_sex_aggregation <-
      base::setdiff(id_columns_in_results_raw, c("sex"))

    group_columns_for_age_aggregation <-
      base::setdiff(id_columns_in_results_raw, c("age_group"))

    group_columns_for_geo_aggregation <-
      base::setdiff(id_columns_in_results_raw, c("geo_id_disaggregated"))

    group_columns_for_multiexp_aggregation <-
      base::setdiff(id_columns_in_results_raw, c("exp_name"))


    # Pre-identify columns to be collapsed
    # First remove columns that are not to be collapsed
    cols_without_results_and_nest  <- base::setdiff(
      colnames_results_raw,
      # Columns to be excluded of the collapse
      # because they are results
      c(columns_to_be_summed, impact_columns, nest_cols))

    # Among those columns that could be collapsed,
    # identify the columns with multiple values.
    # This is a subset of columns to be scaned if they have multipble values
    # when grouping by the sum variables
    cols_with_multiple_values <- results_raw |>
      dplyr::select(dplyr::all_of(cols_without_results_and_nest)) |>
      dplyr::summarise(
        dplyr::across(
          .cols = dplyr::everything(),
          .fns = ~ dplyr::n_distinct(.x) > 1)) |>
      # Select columns where is TRUE
      # Use isTRUE() because it ignores NAs
      dplyr::select(dplyr::where(~ base::isTRUE(.x))) |>
      base::names()

    # Get main results from detailed results ###################################
    # Put all health detailed tables together in a list
    health_detailed  <-
      base::list(input_args = input_args,
                 input_table = input_table,
                 intermediate_calculations = intermediate_calculations,
                 results_raw = results_raw) |>
      # Remove list elemnts that are NULL
      # e.g. usually the case of intermediate_calculations
      purrr::compact()

    output <-
      base::list(health_main = results_raw,
                 health_detailed = health_detailed)


    # Create function to aggregate impacts
    # To be used multiple times below

    sum_round_and_relative_impact <- function(df, grouping_cols, col_total){

      # Identify the columns that have to be collapsed
      # i.e. columns with different values within the groups
      # (e.g. exposure categories)

      if(base::length(cols_with_multiple_values) > 0){


        cols_to_collapse <- df |>
          dplyr::select(dplyr::all_of(c(grouping_cols, cols_with_multiple_values))) |>
          dplyr::summarise(
            .by = dplyr::all_of(grouping_cols),
            dplyr::across(
              .cols = dplyr::everything(),
              .fns = ~ dplyr::n_distinct(.x) > 1)) |>
          # Select columns where is TRUE
          # Use isTRUE() because it ignores NAs
          dplyr::select(dplyr::where(~ base::isTRUE(.x[1]))) |>
          base::names()

        # Collapse columns
        # i.e. paste the values so that they do not hinder the summarize below
        if(base::length(cols_to_collapse) > 0){
          df_collapsed <-
            df |>
            dplyr::mutate(
              .by = dplyr::all_of(grouping_cols),
              dplyr::across(
                .cols = dplyr::all_of(cols_to_collapse),
                .fns = ~ base::paste(.x, collapse = ", "),
                .names = "{.col}"))
        } else { df_collapsed <- df}
      } else { df_collapsed <- df}

      # Create df_collapsed with unique values (to be used below)
      # Remove columns included in columns_to_be_summed and
      # _rounded & _per_100k_inhab.
      # Otherwise, duplicated.
      df_collapsed_and_distinct <- df_collapsed |>
        dplyr::select(-dplyr::all_of(c(columns_to_be_summed, col_total)),
                      -dplyr::matches("_rounded|_per_100k_inhab")) |>
        dplyr::distinct()

      # Sum impact columns (keep original names)
      impact_agg <- df_collapsed |>
        # Deselect columns to be summed
        # Otherwise conflict with left_join behind
        dplyr::select(- dplyr::contains("_rounded")) |>
        dplyr::summarise(
          .by = dplyr::all_of(grouping_cols),
          dplyr::across(
            # Important: across() because this is to be done in all impact columns
            # In attribute_health() only one impact column
            # but get_output is also used by monetize()
            # this function also have other columns with impact discounted and monetized
            # and even comparison scenarios
            # which also have to be included in this aggregation
            .cols = dplyr::all_of(columns_to_be_summed),
            .fns = ~ sum(.x, na.rm = TRUE),
            .names = "{.col}"))|>
        # Add the rest of columns
        dplyr::left_join(
          y = df_collapsed_and_distinct,
          by = grouping_cols) |>
        # Add ..._rounded columns
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::all_of(impact_columns_to_be_summed),
            .fns = ~ round(.x),
            .names = "{.col}_rounded"
          )
        )

      # If population is available, recompute with population and normalized metrics
      if ("population" %in% names(df)) {
        impact_agg <- impact_agg |>
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::all_of(impact_columns_to_be_summed),
              .fns = list(
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

    # Sum across #####

    # Keep the last version
    # This is important because some of the sums of health impacts below
    # do not happen and the chain of sums cannot be broken (see if statements)
    output_last <- results_raw



    ## year ############
    # This is only useful for life table
    if("year" %in% base::names(output_last) &&
       base::length(base::unique(output_last$year)) > 1) {

      output[["health_detailed"]][["results_summed_across_year"]] <-
        sum_round_and_relative_impact(
          df = output_last,
          grouping_cols = group_columns_for_year_aggregation,
          col_total = "year")

      output_last <- output[["health_detailed"]][["results_summed_across_year"]]

    }



    ## exposure categories ############
    # This is only useful for absolute risk because
    # if relative risk
    # there is no exposure category specific results (bhd is not category specific).
    if(base::unique(results_raw$approach_risk) == "absolute_risk") {

    output[["health_detailed"]][["results_summed_across_ar_exp_cat"]] <-
      sum_round_and_relative_impact(
        df = output_last,
        grouping_cols = group_columns_for_ar_exp_cat_aggregation,
        col_total = "exp_category")

    # For relative risk no need of summing impacts across exposure categories
    # because no exposure category bhd so no exposure category impact.
    # This output is anyway created because it is used by other functions
    # such as healthiar::socialize()

    output_last <- output[["health_detailed"]][["results_summed_across_ar_exp_cat"]]

    }



    ##  sex #####
    # Aggregate results by sex
    if("sex" %in% base::names(output_last) &&
       base::length(base::unique(output_last$sex)) > 1) {

      output[["health_detailed"]][["results_summed_across_sex"]] <-
        sum_round_and_relative_impact(
          df = output_last,
          grouping_cols = group_columns_for_sex_aggregation,
          col_total = "sex")


      output_last <- output[["health_detailed"]][["results_summed_across_sex"]]
    }

    output[["health_detailed"]][["results_by_age_group"]] <- output_last




    if("age_group" %in% base::names(output_last) &&
       base::length(base::unique(output_last$age_group)) > 1) {

    ## age_group #####
    # Aggregate results by age_group
    output[["health_detailed"]][["results_summed_across_age"]] <-
      sum_round_and_relative_impact(
        df = output_last,
        grouping_cols = group_columns_for_age_aggregation,
        col_total = "age_group")

    output_last <- output[["health_detailed"]][["results_summed_across_age"]]

    }



    # Aggregate results across pollutants (exposures)
    if("approach_multiexposure" %in% names(results_raw)){
      if(unique(results_raw$approach_multiexposure) %in% "additive"){

        # Paste exposure names before aggregating
        output_last <- output_last |>
          dplyr::mutate(exp_name = base::paste(base::unique(exp_name), collapse = ", "))

        output[["health_detailed"]][["results_summed_across_multiexposure"]] <-
          sum_round_and_relative_impact(
            df = output_last,
            grouping_cols = group_columns_for_multiexp_aggregation,
            col_total = "exp_name")

        output_last <- output[["health_detailed"]][["results_summed_across_multiexposure"]]

      }
    }


    # Store results_by_geo_id_disaggregated
    output[["health_detailed"]][["results_by_geo_id_disaggregated"]] <- output_last |>
      # Ensure that the column value is the same for the sum of impacts
      # Otherwise missmatch of tables in other functions e.g. daly()
      dplyr::mutate(
        sex = "total",
        age_group = "total")


    ## geo_id_aggregated #####
    # Aggregate results by higher geo_level
    # only if geo_id_aggregated is define

    if("geo_id_aggregated" %in% names(output_last)){

      output[["health_detailed"]][["results_by_geo_id_aggregated"]] <-
        sum_round_and_relative_impact(
          df = output_last,
          grouping_cols = group_columns_for_geo_aggregation,
          col_total = "geo_id_disaggregated")

      output_last <- output[["health_detailed"]][["results_by_geo_id_aggregated"]]

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

    # Choose columns to be put first
    first_columns <- c(id_columns, impact_columns)

    # Create the functions
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

    # Use the funtions above to put first the columns
    output <-
      purrr::map(
        .x = output,
        .f = ~ put_first_cols_recursive(x = .x,
                                        cols = first_columns))


    return(output)
  }
