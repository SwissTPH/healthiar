#' Get inflation factor

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the inflation factor based on inflation rate.

# ARGUMENTS ####################################################################
#' @inheritParams monetize

# EXAMPLES #####################################################################
#' @examples
#' # TODO
#'

#' @author Alberto Castro & Axel Luyten

#' @export



get_inflation_factor <-
  function(discount_year,
           inflation_rate = NULL){

    if(!base::is.null(inflation_rate)){
      # if discount_rate is NULL

      inflation_factor <- (1 + inflation_rate) ^ discount_year

    } else {

      inflation_factor <- 1
    }

    return(inflation_factor)
  }
