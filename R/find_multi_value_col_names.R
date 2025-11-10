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

  multi_value_col_names <- df |>
    dplyr::summarise(
      .by = dplyr::all_of(c(group_col_names)),
      dplyr::across(
        .cols = dplyr::everything(),
        .fns = ~ base::length(base::unique(.x)) > 1)) |>
    # Select only columns that have potentially multiple values
    dplyr::select(-dplyr::all_of(group_col_names)) |>
    # Select columns where there is at least a TRUE (different value)
    dplyr::select(dplyr::where(~ any(.x)))|>
    base::names()

  return(multi_value_col_names)


}
