#' Get the relative risk of an exposure level

#' @description
#' This function re-scales the relative risk from the increment value in the epidemiological study (e.g. for PM2.5 10 or 5 ug/m3) to the actual population exposure

#' @inheritParams attribute_master
#' @param rr \code{Numeric vector} containing the relative risk. The data frame must contain the central estimate as well as the lower and upper bound of the exposure-response function.
#' @param exp Population exposure to the stressor (e.g. annual population-weighted mean).
#' @param cutoff \code{Numeric value} showing the cut-off exposure level in ug/m3 (i.e. the exposure level below which no health effects occur).
#' @param erf_eq Equation of the user-defined exposure-response function that puts the relative risk (y) in relation with exposure (x). If the function is provided as \code{string}, it can only contains one variable: x (exposure). E.g. "3+x+x^2". If the function is provided as a \code{function}, the object should have a function class. If only the values of the x-axis (exposure) and y axis (relative risk) of the dots in the exposure-response function are available, a cubic spline natural interpolation can be assumed to get the function using, e.g., \code{stats::splinefun(x, y, method="natural")}

#' @returns
#' This function returns three \code{values} corresponding to the central estimate as well as the lower and upper bound of the exposure-response function.

#' @examples
#' get_risk(rr=1.05, exp=10, cutoff=5, erf_shape="linear" )

#' @author Alberto Castro & Axel Luyten

#' @note Experimental function

#' @export



get_risk <-
  function(
    erf_shape = NULL,
    rr = NULL,
    rr_increment = NULL,
    erf_eq = NULL,
    cutoff = 0,
    exp
  ) {

    # Check if exposure is upper than cutoff
    # Otherwise the value of the exposure must be the cutoff (minimum possible)
    exp <-
      base::ifelse(exp > cutoff,
                   exp,
                   # if exp < cutoff, then exp should be cutoff
                   cutoff)

    # Obtain rr_at_exp, i.e. the relative risk the level of exposure
    # instead of for the increment

    # If erf_eq is passed as argument
    if (! base::is.null(erf_eq)) {
      # And if erf_eq is a function
      # i.e. a single raw in the vectorial structure
      # if (base::is.function(erf_eq)) {
      #
      #   rr_at_exp <- erf_eq(exp - cutoff)
      # erf_eq that are functions are encapsulated in lists to be included in tibbles
      # That is why we need is.list() and sapply() and mapply()
      # } else
        if (base::is.list(erf_eq) && base::all(sapply(erf_eq, is.function))) {

        rr_at_exp <- mapply(function(f, cval) f(cval), erf_eq, exp - cutoff)
      # If the function is a string (vector)
      } else if (base::is.character(erf_eq)) {
        # The function must in this case created to be used below
        erf_fun <- base::eval(base::parse(text = base::paste0("function(c) { ", erf_eq, " }")))

        rr_at_exp <- erf_fun(exp - cutoff)
      }

    # If erf_eq is not entered by the user
    } else if (base::is.null(erf_eq)){

      # Calculate the rr_at_exp based on erf_shape
      rr_at_exp <-
        dplyr::case_when(
          erf_shape == "linear" ~
            1 + ((rr - 1) * (exp - cutoff) / rr_increment),
          erf_shape == "log_linear" ~
            base::exp(base::log(rr) * (exp - cutoff) / rr_increment),
          erf_shape == "linear_log" ~
            1 + ((rr - 1) * (base::log(exp) - base::log(cutoff)) / base::log(rr_increment)),
          erf_shape == "log_log" ~
            base::exp(base::log(rr) * (base::log(exp) - base::log(cutoff)) / base::log(rr_increment))
      )
    }

    return(rr_at_exp)

  }
