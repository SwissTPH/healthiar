#' Find joining columns
#'
#' @description Find columns in two data frames that have same name and identical values (excluding exceptions).- The resulting joining columns are used to dplyr::x_join data frames and add the vector to by=.
#' @param df_1 First \code{Data frame}
#' @param df_2 Second \code{Data frame}
#' @param except \code{Vector} of strings showing columns that have to be excluded although they fulfill the inclusion criteria (same name and value)

#'
#' @returns
#' This function returns a \code{vector} of strings
#' @examples
#' TBD
#' @author Alberto Castro & Axel Luyten
#' @note Experimental function
#' @keywords internal

find_joining_columns <-
  function(df_1,
           df_2,
           except = NULL){

    joining_columns <-
      # First identify the columns that are common for df_1 and df_2
      dplyr::intersect(base::names(df_1),
                base::names(df_2))|>
      # Second, the identical columns of the common ones
      # They are the columns to be used when joining data frames
      purrr::keep(~ base::identical(df_1[[.x]],
                              df_2[[.x]]))|>
        # Finally exclude scenario specific columns
        dplyr::setdiff(except)


  }
