#' Add meta-information to the data frame containing the input data

# DESCRIPTION ##################################################################
#' @description
#' This function adds meta-information of the input data within the data frame containing the input data.

# ARGUMENTS ####################################################################
#' @param df \code{Data frame} containing the input data
#' @param info \code{String} or \code{Data frame} with one row or \code{Vector} of length 1 showing additional information or id for the pollutant.

# VALUE ########################################################################
#' @returns
#' This function returns a \code{data frame} with binding the input data with the info columns (info_ is added to the column names)

#' @author Alberto Castro & Axel Luyten

#' @keywords internal




add_info <- function(df, info){

  if(is.null(info)){
    output <-
      dplyr::mutate(df, info = NULL)

  } else if(is.vector(info)) {
    output <-
      dplyr::mutate(df, info = info)

  } else if(is.data.frame(info)){

    # The columns keep the names entered by the user, only adding the prefix
    # "info_". The prefix keeps them apart from the internal columns of the
    # input table (e.g. sex or age_group), next to which they are put below.
    # Moreover, it is the marker by which the rest of the package recognizes
    # the info columns as id columns identifying subgroups (see get_output()).
    # The prefix is always added, also to names that already start with "info",
    # so that the names stay unique (e.g. the columns "pollutant" and
    # "info_pollutant" do not end up with the same name)
    output <-
      stats::setNames(info, paste0("info_", names(info)))

    output <- dplyr::bind_cols(df, output)

  }

  return(output)

}
