#' Get discount factor

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the discount factor based on discount rate.

# ARGUMENTS ####################################################################
#' @inheritParams monetize
#' @param n_years
#' \code{Numeric value} or \code{numeric vector} specifying the number of years
#' elapsed for which the discount factor is to be calculated. One factor is
#' returned per entered value. The year 0, i.e. the present, gets a factor of 1
#' (no discounting). Note that this differs from the argument of the same name
#' in \code{monetize()}, which is the time horizon: \code{monetize()} calls this
#' function with each single year from 0 to that horizon.
#'
# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' This function is called inside \code{monetize()}.
#'
#' One of the following three discount shapes can be selected:
#' \itemize{
#'  \item Exponential \insertCite{Frederick2002_jel,HMTreasury2026_report}{healthiar}
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
#' @returns This function returns the \code{numeric} discount factor(s),
#' one per value entered in \code{n_years}.
#'
# EXAMPLES #####################################################################
#' @examples
#' # Goal: discount factor after a given number of years
#' get_discount_factor(
#'   discount_rate = 0.07,
#'   n_years = 5
#'  )
#'
#' # Goal: discount factor for each year of a time horizon
#' get_discount_factor(
#'   discount_rate = 0.07,
#'   n_years = 0:5
#'  )
#'
#'
#' @seealso
#' \itemize{
#'   \item Alternative: \code{\link{monetize}}
#' }
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
           discount_shape = "exponential"){


    # If no discount_rate is provided,
    # then assume discount_factor = 1
    # This does not change the results

    if(is.null(discount_rate)){
      # if discount_rate is NULL

      discount_factor <- 1

      # If only discount_rate provided ####
    } else if(!is.null(discount_rate)) {

        # case_when() and not nested ifelse(): ifelse() returns a result of the
        # length of its condition, so one single discount_shape truncated the
        # whole vector of years to its first element without any warning.
        # The shape can be one single value (direct call) or one per year
        # (column added in monetize()), so it is recycled first: case_when()
        # expects the conditions to have the same length as the results.
        # Same approach as for erf_shape in get_risk()
        discount_shape <- rep_len(discount_shape, length(n_years))

        discount_factor <-
          dplyr::case_when(
            # Exponential ####
            discount_shape == "exponential" ~
              1/((1 + discount_rate) ^ n_years),

            # Hyperbolic Harvey ####
            discount_shape == "hyperbolic_harvey_1986" ~
              1/((1 + n_years) ^ discount_rate),

            # Hyperbolic Mazur ####
            discount_shape == "hyperbolic_mazur_1987" ~
              1/(1 + discount_rate * n_years),

            # An unknown shape yields NA instead of a silently wrong number
            .default = NA_real_)
    }

    return(discount_factor)
  }
