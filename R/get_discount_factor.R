#' Get discount factor

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the discount factor based on discount rate. If the argument \code{inflation_rate} is NULL (default), it is assumed that the discount rate is already corrected for inflation). Otherwise (if a value for \code{inflation_rate} is entered), the resulted discount factor is adjusted for inflation.

# ARGUMENTS ####################################################################
#' @inheritParams monetize

# EXAMPLES #####################################################################
#' @examples
#' # TODO

# DETAILS ######################################################################

#' @details
#' \strong{Equations discount factors (without inflation)}
#' @details
#' \emph{Exponential discounting (no inflation)}
#' \deqn{discount\_factor = \frac{1}{(1 + discount\_rate) ^{discount\_year}}}
#' @details
#' \emph{Hyperbolic discounting Harvey (no inflation)}
#' \deqn{discount\_factor = \frac{1}{(1 + discount\_year)^{discount\_rate}}}
#' \emph{Hyperbolic discounting Mazure (no inflation)}
#' \deqn{discount\_factor = \frac{1}{(1 + (discount\_rate \times discount\_year)}}

#' @details
#' \strong{Equations discount factors with inflation}
#' @details
#' \emph{Exponential discounting (with inflation)}
#' \deqn{discount\_and\_inflation\_factor = \frac{1}{((1 + discount\_rate) \times (1 + inflation\_rate)) ^{discount\_year}}}
#' @details
#' \emph{Hyperbolic discounting Harvey (with inflation)}
#' \deqn{discount\_and\_inflation\_factor = \frac{1}{(1 + discount\_year)^{discount\_rate} \times (1 + inflation\_rate)^{discount\_year}}}
#' \emph{Hyperbolic discounting Mazure (with inflation)}
#' \deqn{discount\_and\_inflation\_factor = \frac{1}{(1 + (discount\_rate \times discount\_year) \times (1 + inflation\_rate)^{discount\_year}}}

#' @author Alberto Castro & Axel Luyten

#' @export



get_discount_factor <-
  function(discount_rate,
           projected_year,
           discount_shape = "exponential",
           inflation_rate = NULL){


    # If no discount_rate is provided,
    # then assume discount_factor = 1
    # This does not change the results

    if(base::is.null(discount_rate)){
      # if discount_rate is NULL

      discount_factor <- 1

      # If only discount_rate provided ####
    } else if(!base::is.null(discount_rate) &&
              base::is.null(inflation_rate)) {
      # if inflation_rate is NULL

        discount_factor <-
          base::ifelse(
            # Exponential ####
            discount_shape == "exponential",

            1/((1 + discount_rate) ^ discount_year),
            # Hyperbolic Harvey ####
            base::ifelse(discount_shape == "hyperbolic_harvey_1986",
                         1/((1 + discount_year) ^ discount_rate),
                         # Hyperbolic Mazur ####
                         base::ifelse(discount_shape == "hyperbolic_mazur_1987",
                                      1/(1 + discount_rate * projected_year),
                                      NA)))

    # If both discount and inflation rate provided ####
      } else if(!base::is.null(discount_rate) &&
                !base::is.null(inflation_rate)) {
        # if both discount_rate and inflation_rate are available
        # Adjust by inflation

        discount_factor <-
          base::ifelse(
            # Exponential ####
            discount_shape == "exponential",

            1/(((1+discount_rate)*(1+inflation_rate)) ^ discount_year),
            # Hyperbolic Harvey ####
            base::ifelse(discount_shape == "hyperbolic_harvey_1986",
                         1/(((1 + discount_year) ^ discount_rate) * ((1 + inflation_rate) ^ discount_year)),
                         # Hyperbolic Mazur ####
                         base::ifelse(discount_shape == "hyperbolic_mazur_1987",
                                      1/((1 + discount_rate * projected_year) * ((1 + inflation_rate) ^ projected_year)),
                                      NA)))
      }

    return(discount_factor)
  }
