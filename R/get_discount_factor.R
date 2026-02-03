#' Get discount factor

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the discount factor based on discount rate. If the argument \code{inflation_rate} is NULL (default), it is assumed that the discount rate is already corrected for inflation). Otherwise (if a value for \code{inflation_rate} is entered), the resulted discount factor is adjusted for inflation.

# ARGUMENTS ####################################################################
#' @inheritParams monetize
#'
# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' This function is called inside \code{monetize()}.
#'
#' #' One of the following three discount shapes can be selected:
#' \itemize{
#'  \item Exponential \insertCite{Frederick2002_jel}{healthiar}
#'  \item Hyperbolic as \insertCite{Harvey1986_ms;textual}{healthiar}
#'  \item Hyperbolic as \insertCite{Mazur1987_book;textual}{healthiar}}
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#monetization}{Monetization}}
#'
#'
# VALUE ########################################################################
#' @returns This function returns the \code{numeric} discount factor.
#'
# EXAMPLES #####################################################################
#' @examples
#' get_discount_factor(
#'   discount_rate = 0.07,
#'   n_years = 5
#'  )
#'
#'
#' @references
#'
#' \insertAllCited{}
#'
#'
#' @author Alberto Castro & Axel Luyten
#'
#' @export



get_discount_factor <-
  function(discount_rate,
           n_years,
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

            1/((1 + discount_rate) ^ n_years),
            # Hyperbolic Harvey ####
            base::ifelse(discount_shape == "hyperbolic_harvey_1986",
                         1/((1 + n_years) ^ discount_rate),
                         # Hyperbolic Mazur ####
                         base::ifelse(discount_shape == "hyperbolic_mazur_1987",
                                      1/(1 + discount_rate * n_years),
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

            1/(((1+discount_rate)*(1+inflation_rate)) ^ n_years),
            # Hyperbolic Harvey ####
            base::ifelse(discount_shape == "hyperbolic_harvey_1986",
                         1/(((1 + n_years) ^ discount_rate) * ((1 + inflation_rate) ^ n_years)),
                         # Hyperbolic Mazur ####
                         base::ifelse(discount_shape == "hyperbolic_mazur_1987",
                                      1/((1 + discount_rate * n_years) * ((1 + inflation_rate) ^ n_years)),
                                      NA)))
      }

    return(discount_factor)
  }
