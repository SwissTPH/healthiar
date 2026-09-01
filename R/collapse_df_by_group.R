#' Collapse rows by grouping columns

# DESCRIPTION ##################################################################
#' @description
#' This function aggregates a data frame into one row per group, pasting the values of the columns that have different values within a group and (optionally) summing the columns specified in \code{sum_col_names}

# ARGUMENTS ####################################################################
#' @param df \code{Data frame or tibble} containing the data
#' @param group_col_names \code{String vector} containing the column names in \code{df} that serve as grouping columns.
#' @param sum_col_names \code{String vector} containing the column names in \code{df} that have to be summed within each group (e.g. impacts). Optional.
#' @param multi_value_col_names \code{String vector} containing the columns names in \code{df} that do not have a unique value (but different values).
# VALUE ########################################################################
#' @returns
#' This function returns a \code{data frame} or \code{tibble} with one row per group,
#' keeping the columns and the column order of \code{df}

#' @author Alberto Castro & Axel Luyten

#' @keywords internal




collapse_df_by_group <- function(df,
                                 group_col_names,
                                 sum_col_names = NULL,
                                 multi_value_col_names = NULL){

  col_names <- base::names(df)

  # Columns with different values in df.
  # Only they can have multiple values within a group
  if(base::is.null(multi_value_col_names)){
    multi_value_col_names <-
      find_multi_value_col_names(df = df, group_col_names = NULL)
  }

  # Identify the columns to be collapsed,
  # i.e. those with multiple values within a group
  cols_to_collapse <- df |>
    dplyr::select(dplyr::all_of(c(group_col_names, multi_value_col_names))) |>
    find_multi_value_col_names(df = _, group_col_names = group_col_names) |>
    base::setdiff(sum_col_names)

  # Columns with the same value in the whole df do not have to be aggregated
  # group by group (much faster).
  # They are added again after the aggregation taking only the first row,
  # which bind_cols() recycles to all groups
  # (the value is the same in all rows, so the first row is representative)
  cols_constant <-
    base::setdiff(col_names,
                  c(group_col_names, multi_value_col_names, sum_col_names))

  # The remaining columns have a unique value per group,
  # so the first value can be taken
  cols_to_keep <-
    base::setdiff(col_names,
                  c(group_col_names, cols_to_collapse, cols_constant, sum_col_names))

  # Collapse, sum and keep columns in one single step
  # resulting in one row per group
  output <- df |>
    dplyr::summarise(
      .by = dplyr::all_of(group_col_names),
      # Paste the values of the columns with multiple values in the group
      dplyr::across(
        .cols = dplyr::all_of(cols_to_collapse),
        .fns = base::toString),
      dplyr::across(
        .cols = dplyr::all_of(sum_col_names),
        .fns = ~ base::sum(.x, na.rm = TRUE)),
      dplyr::across(
        # [1] instead of dplyr::first() to also work with list columns
        .cols = dplyr::all_of(cols_to_keep),
        .fns = ~ .x[1])) |>
    # Add the columns that have constant values
    # using the first row [1, ]  because df has different number of rows 
    # and because the values are constant, it does not matter
    dplyr::bind_cols(df[1, cols_constant]) |>
    # Restore the original order of the columns
    dplyr::select(dplyr::all_of(col_names))

  return(output)

}
