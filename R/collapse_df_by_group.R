#' Collapse rows by grouping columns

# DESCRIPTION ##################################################################
#' @description
#' This function paste rows with different values and keep only unique rows following grouping columns

# ARGUMENTS ####################################################################
#' @param df \code{Data frame or tibble} containing the data
#' @param group_col_names \code{String vector} containing the column names in \code{df} that serve as grouping columns.
#' @param multi_value_col_names \code{String vector} containing the columns names in \code{df} that do not have a unique value (but different values).
#' @param ci_col_names \code{String vector} containing the column names in \code{df} that refer to confidence interval bounds (suffix _ci).
#' @param only_unique_rows \code{Boolean} that determines if the final data frame or tibble must only keep unique rows
# VALUE ########################################################################
#' @returns
#' This function returns a \code{data frame} or \code{tibble} with lower number rows after collapsing

#' @author Alberto Castro & Axel Luyten

#' @keywords internal




collapse_df_by_group <- function(df,
                                 group_col_names,
                                 multi_value_col_names = NULL,
                                 ci_col_names = NULL,
                                 only_unique_rows = TRUE){

  if(base::is.null(multi_value_col_names)){
    multi_value_col_names <-
      healthiar:::find_multi_value_col_names(df = df, group_col_names = NULL)
  }

  if(base::is.null(ci_col_names)){
    ci_col_names <-
      base::grep("_ci", base::names(df), value = TRUE)
  }

  # Identify the columns to be collapsed
  cols_to_collapse <- df |>
    dplyr::select(dplyr::all_of(c(group_col_names, multi_value_col_names))) |>
    # Keep only central estimates
    # because no variability is expected across bounds
    # This enables faster evaluation
    dplyr::filter(
      dplyr::if_all(.cols = dplyr::all_of(ci_col_names),
                    .fns = ~ base::grepl("central", .x))) |>
    healthiar:::find_multi_value_col_names(df = _, group_col_names = group_col_names)


  # Collapse columns
  # i.e. paste the values so that they do not hinder the summarize below
  if(base::length(cols_to_collapse) > 0){
    df_pasted <-
      df |>
      dplyr::mutate(
        .by = dplyr::all_of(group_col_names),
        dplyr::across(
          .cols = dplyr::all_of(cols_to_collapse),
          .fns = ~ base::toString(.x),
          .names = "{.col}"))
  } else { df_pasted <- df}

  # # ALTERNATIVE CODE: "total" or remove value of cols_to_collapse, very small speed gain
  # # Collapse columns
  # # i.e. paste the values so that they do not hinder the summarize below
  # if(base::length(cols_to_collapse) > 0){
  #
  #   df_pasted <- df
  #
  #   df_pasted[, cols_to_collapse] <- "total"
  #
  # } else { df_pasted <- df}

  # # Collapse columns
  # # i.e. paste the values so that they do not hinder the summarize below
  # if(base::length(cols_to_collapse) > 0){
  #   df_pasted <- df |>
  #     dplyr::select(-dplyr::all_of(cols_to_collapse))
  # } else { df_pasted <- df}


  if(only_unique_rows){
    # Create df_collapsed with unique values (to be used below)
    output <- dplyr::distinct(df_pasted)
  } else {
    output <- df_pasted
  }




  return(output)

}
