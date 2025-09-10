#' Get discount factor

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the discount factor based on discount rate (already corrected for inflation)

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
           discount_shape = "exponential"){


    # If no discount_rate is provided,
    # then assume discount_factor = 1
    # This does not change the results

    if(any(is.null(discount_rate),
           is.null(discount_shape))){

      discount_factor <- 1
    } else{
      # If there is a discount_rate,
      # apply the function get_discount_factor()


      discount_factor <-
        ifelse(
          discount_shape == "exponential",
          (1 + discount_rate) ^ discount_year,
          ifelse(discount_shape == "hyperbolic_harvey_1986",
                 (1 + discount_year) ^ discount_rate,
                 ifelse(discount_shape == "hyperbolic_mazur_1987",
                        1 + discount_rate * discount_year,
                        NA)))

    }



    return(discount_factor)
  }
