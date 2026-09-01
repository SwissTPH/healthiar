#' Find columns with multiple values

# DESCRIPTION ##################################################################
#' @description
#' This function find data frame or tibble column names with different values in their rows (i.e. not a unique value)

# ARGUMENTS ####################################################################
#' @param df \code{Data frame or tibble} containing the data
#' @param group_col_names \code{String vector} that refers to the column names in \code{df} that serve as grouping columns.

#' @returns
#' This function returns a \code{string vector} with the names of the columns with multiple values

#' @author Alberto Castro & Axel Luyten

#' @keywords internal




find_multi_value_col_names <- function(df,
                                       group_col_names = NULL){

  # Only the columns that are not grouping columns have to be scanned
  col_names <- base::setdiff(base::names(df), group_col_names)

  # Id of the group of each row
  # (1 for all rows if there are no grouping columns)
  group_id <- df |>
    dplyr::mutate(
      .by = dplyr::all_of(group_col_names),
      group_id = dplyr::cur_group_id()) |>
    dplyr::pull(group_id) 

  n_groups <- dplyr::n_distinct(group_id)

  # A column has multiple values if it builds more combinations with the group id
  # than there are groups.
  # This is much faster than scanning the values of each group separately
  has_multiple_values <-
    purrr::map_lgl(
      .x = col_names,
      .f = ~ dplyr::n_distinct(group_id, df[[.x]]) > n_groups)

  multi_value_col_names <- col_names[has_multiple_values]

  return(multi_value_col_names)

}
