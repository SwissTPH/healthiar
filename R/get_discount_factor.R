#' Get discount factor

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the discount factor based on discount rate. If the argument \code{inflation} is NULL (default), it is assumed that the discount rate is already corrected for inflation). Otherwise, enter a value in \code{inflation}.

# ARGUMENTS ####################################################################
#' @inheritParams monetize

# EXAMPLES #####################################################################
#' @examples
#' # TODO
#'

#' @author Alberto Castro & Axel Luyten

#' @export



get_discount_factor <-
  function(discount_rate,
           discount_year,
           discount_shape = "exponential",
           inflation = NULL){


    # If no discount_rate is provided,
    # then assume discount_factor = 1
    # This does not change the results



    if(base::is.null(discount_rate)){

      discount_factor <- 1

    } else { # if discount_rate is available
      if(is.null(inflation)){

        discount_factor <-
          base::ifelse(
            discount_shape == "exponential",
            1/((1 + discount_rate) ^ discount_year),
            base::ifelse(discount_shape == "hyperbolic_harvey_1986",
                         1/((1 + discount_year) ^ discount_rate),
                         base::ifelse(discount_shape == "hyperbolic_mazur_1987",
                                      1/(1 + discount_rate * discount_year),
                                      NA)))

      } else { # if inflation is not NULL
        # Adjust by inflation

        discount_factor <-
          base::ifelse(
            discount_shape == "exponential",
            1/(((1+discount_rate)*(1+inflation)) ^ discount_year),
            base::ifelse(discount_shape == "hyperbolic_harvey_1986",
                         1/(((1 + discount_year) ^ discount_rate) * ((1 + inflation) ^ discount_year)),
                         base::ifelse(discount_shape == "hyperbolic_mazur_1987",
                                      1/((1 + discount_rate * discount_year) * ((1 + inflation) ^ discount_year)),
                                      NA)))

      }

     }



    return(discount_factor)
  }
