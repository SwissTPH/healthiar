#' Get inflation factor

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the inflation factor based on inflation rate.

# ARGUMENTS ####################################################################
#' @inheritParams monetize

# DETAILS ######################################################################

#' @details
#' \strong{Equation inflation factor (without discounting)}
#' @details
#' \deqn{inflation\_factor = (1 + inflation\_rate)^{discount\_year}}

# EXAMPLES #####################################################################
#' @examples
#' # TODO
#'

#' @author Alberto Castro & Axel Luyten

#' @export



get_inflation_factor <-
  function(n_years,
           inflation_rate = NULL){

    if(!base::is.null(inflation_rate)){
      # if discount_rate is NULL

      inflation_factor <- (1 + inflation_rate) ^ n_years

    } else {

      inflation_factor <- 1
    }

    return(inflation_factor)
  }
