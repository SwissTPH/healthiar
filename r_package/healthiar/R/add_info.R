#' Add meta-information to the data frame containing the input data

#' @description
#' This function adds meta-information of the input data within the data frame containing the input data.

#' @param df \code{Data frame} containing the input data
#' @param info \code{String} or \code{Data frame} with one row or \code{Vector} of length 1 showing additional information or id for the pollutant.

#' @returns
#' This function returns a \code{data frame} with binding the input data with the info columns (info_ is added to the column names)

#' @author Alberto Castro & Axel Luyten

#' @note Experimental function

#' @keywords internal




add_info <- function(df, info){

  if(is.null(info)){
    output <-
      dplyr::mutate(df, info = NULL)

  } else if(is.vector(info)) {
    output <-
      dplyr::mutate(df, info = info)

  } else if(is.data.frame(info)){

    output <-
      stats::setNames(info, base::paste0("info_", 1: base::length(base::names(info))))

    output <- dplyr::bind_cols(df, output)

  }

  return(output)

}
