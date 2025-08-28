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

    # ID columns
    id_cols <- c("geo_id_macro", "geo_id_micro",
                    "exp_name",
                    "erf_ci","exp_ci", "bhd_ci", "cutoff_ci", "dw_ci", "duration_ci",
                    "year", "exp_category", "sex", "age_group")

    # Store column names of results_raw
    # because it is to be used often below
    colnames_results_raw <- base::names(results_raw)

    # Store those id_columns that are present in results_raw
    id_cols_available <-
      base::intersect(id_cols, colnames_results_raw)

    # Define all the ci columns have that have to be filtered to keep only central
    ci_cols <- base::grep("_ci", id_cols, value = TRUE)

    # Identify which of the ci_cols are present in the assessment
    ci_cols_available <- base::grep("_ci", id_cols_available, value = TRUE)

    ci_cols_available_except_erf <- base::setdiff(ci_cols_available, "erf_ci")


    # Keep the larger geo_id available
    # Since intersect() keep the order, taking the first element [1] ensures
    # that it is geo_id_macro if available and otherwise geo_id_micro
    geo_id_available <-
      base::intersect(c("geo_id_macro", "geo_id_micro"),
                      id_cols_available)

    larger_geo_id_available <- geo_id_available[1]


    ## Columns to be summed
    impact_cols <-
      base::grep("impact", colnames_results_raw, value = TRUE)

    nest_cols <-
      base::grep("_by_", colnames_results_raw, value = TRUE)

    cols_to_be_summed <-
      base::setdiff(
        # Columns including these strings
        base::grep("impact|absolute_risk_as_percent|population", colnames_results_raw, value = TRUE),
        # but not including these
        base::grep("_by_|_rounded|_per_100k_inhab", colnames_results_raw, value = TRUE))

    # Only columns to be summed that include the string "impact"
    # This is used for per_100k_inhab
    # Use grep() because there are many possible column names, no only impact
    # e.g. "monetized_impact"
    impact_cols_to_be_summed <-
      base::grep("impact", cols_to_be_summed, value = TRUE)

    # Pre-identify columns to be collapsed
    # First remove columns that are not to be collapsed
    cols_without_results_and_nest  <- base::setdiff(
      colnames_results_raw,
      # Columns to be excluded of the collapse
      # because they are results
      c(cols_to_be_summed, impact_cols, nest_cols))

    # Among those columns that could be collapsed,
    # identify the columns with multiple values.
    # This is a subset of columns to be scaned if they have multipble values
    # when grouping by the sum variables
    cols_with_multiple_values <- results_raw |>
      dplyr::select(dplyr::all_of(cols_without_results_and_nest)) |>
      # No groups, i.e. for the whole data set
      #find_cols_with_multiple_values(df = _, group = NULL)
      healthiar:::find_multi_value_col_names(
        df = _,
        group_col_names = NULL)

    ## Define variable for results_by_ and
    # the columns that have to be excluded in the group columns
    results_by_vars_and_excluded_cols <- base::list(
      exp_name = c("year", "exp_category", "sex", "age_group"),
      year = c("exp_name", "exp_category", "age_group", "sex"),
      exp_category = c("exp_name", "year", "age_group", "sex"),
      sex = c("exp_name", "year", "exp_category", "age_group"),
      age_group = c("exp_name", "year", "exp_category", "sex"),
      geo_id_micro = c("exp_name", "year", "exp_category", "sex", "age_group", "geo_id_macro"),
      geo_id_macro = c("exp_name", "year", "exp_category", "sex", "age_group", "geo_id_micro"))

    results_by_vars <- base::names(results_by_vars_and_excluded_cols)

    # Identify the vars to be used for results_by
    results_by_vars_to_be_used <-
      # Only take the vars that present in results_raw
      # This avoid the steps below for not relevant vars (because not available)
      base::intersect(results_by_vars, colnames_results_raw) |>
      # Only take vars that have more than one unique value
      # Summing impacts across one category does not bring anything and
      # and makes the code slower
      base::intersect(cols_with_multiple_values) |>
      # Add all available geo_ids
      # They are needed in any case
      # because at leaset results_by_geo_id_micro must be available
      # for other healthiar functions
      base::union(geo_id_available)

    # Build list with the result_by_vars and the correponding grouping_cols
    grouping_cols_for_results_by <-
      results_by_vars_and_excluded_cols[results_by_vars_to_be_used] |>
      purrr::map(
        ~ base::setdiff(id_cols_available, .x)
      )

    # Other columns: e.g. info, scen_, pop_fraction...
    # grepl() because no fix number and names
    # scen columns only for the case of compare()
    other_cols_with_multiple_values <-
      base::intersect(cols_with_multiple_values,
                      colnames_results_raw[base::grepl("info_|scen_|pop_fraction", colnames_results_raw)])


    results_by_vars_to_be_used_except_geo_id_macro <-
      base::setdiff(results_by_vars_to_be_used, c("geo_id_macro"))

    # The _ci columns will never be collapsed
    # This step avoid unneded data processing below
    cols_eligible_for_collapse <-
      base::setdiff(cols_with_multiple_values,
                    ci_cols_available)


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

    sum_round_and_relative_impact <- function(df, var){

      grouping_cols <- grouping_cols_for_results_by[[var]]

      # Prepare df for collapse by removing columns
      # that are to be summed or obtained in steps below
      df_for_collapse <- df |>
        dplyr::select(-dplyr::any_of(c(cols_to_be_summed)),
                      -dplyr::matches("_rounded|_per_100k_inhab"))

      # Collapse df using the intern healthiar function
      df_collapsed <-
        healthiar:::collapse_df_by_group(
          df = df,
          group_col_names = grouping_cols,
          # If these two last arguments are empty the function can obtain them internally
          # but we enter them because they are the same for all results_by_vars
          # and we increase speed in this way (instead of repeating the process)
          multi_value_col_names = cols_eligible_for_collapse,
          ci_col_names = ci_cols_available,
          only_unique_rows = FALSE)

      # Create df_collapsed with unique values (to be used below)
      # Remove columns included in cols_to_be_summed and
      # _rounded & _per_100k_inhab.
      # Otherwise, duplicated.
      df_collapsed_and_distinct <- df_collapsed |>
        dplyr::select(-dplyr::any_of(c(cols_to_be_summed)),
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
            .cols = dplyr::all_of(cols_to_be_summed),
            .fns = ~ sum(.x, na.rm = TRUE),
            .names = "{.col}"))|>
        # Add the rest of columns
        dplyr::left_join(
          y = df_collapsed_and_distinct,
          by = grouping_cols) |>
        # Add ..._rounded columns
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::all_of(impact_cols_to_be_summed),
            .fns = ~ round(.x),
            .names = "{.col}_rounded"
          )
        )

      # If population is available, recompute with population and normalized metrics
      if ("population" %in% names(df)) {
        impact_agg <- impact_agg |>
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::all_of(impact_cols_to_be_summed),
              .fns = base::list(per_100k_inhab = ~ (.x / population) * 1e5),
              .names = "{.col}_{.fn}"
            )
          )
      }


      return(impact_agg)

    }

    # At least result_by_geo_id_micro is to be calculated
    # so no if statement here

    for(var in results_by_vars_to_be_used){

      output$health_detailed[[base::paste0("results_by_", var)]] <-
        sum_round_and_relative_impact(
          df = results_raw,
          var = var)

    }


    # Keep only the ci central in main output ###########

    results_by_larger_geo_id_available <-
      base::paste0("results_by_", larger_geo_id_available)


    # Store the last output in health main before starting the loop
    output[["health_main"]] <-
      # Take the larger geo_id
      output$health_detailed[[results_by_larger_geo_id_available]] |>
      # Keep only central estimates
      # grepl instead of %in% because it needs
      # to be flexible to also accept the central_*id_ass* in the
      # summarize_uncertainty
      dplyr::filter(
        dplyr::if_all(.cols = dplyr::all_of(ci_cols_available_except_erf),
                      .fns = ~ base::grepl("central", .x)))


    # Order columns ############################################################
    # putting first (on the left) those that determine different results across rows

    # Choose columns to be put first
    first_cols <- c(id_cols, impact_cols)

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
        if(base::is.data.frame(x)){
          put_first_cols(x, cols)

        # If x is list and all list elements are data frames (and not lists)
        }else if (base::is.list(x) & base::all(purrr::map_lgl(x, is.data.frame))){
          purrr::map(
            .x = x,
            .f = ~ put_first_cols(.x, cols))

        }else{x}

      }

    # Use the functions above to put first the columns
    output <-
      purrr::map(
        .x = output,
        .f = ~ put_first_cols_recursive(x = .x,
                                        cols = first_cols))


    return(output)
  }
