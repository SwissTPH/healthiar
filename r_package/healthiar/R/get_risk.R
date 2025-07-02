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



    # The function assumes that the user of the package does not define the function entirely,
    # but using arguments such as exp, cutoff, rr_increment and erf_shape
    # Therefore, the default value of the argument erf_eq should be NULL
    # If the user enter a TRUE, erf_eq is read. Otherwise the arguments
    # exp, cutoff, rr_increment and erf_shape.

    # Let's write the exposure-response function (erf)
    # based on c (concentration) as single data

    # A first (and most usual) option is to define the erf using
    # the shape of the function (erf_shape) and
    # the relative risk from the literature


    # Build the exposure-response function once outside
    if (!is.null(erf_eq)) {

      if (is.function(erf_eq)) {

        rr_c <- erf_eq(exp - cutoff)

      } else if (is.list(erf_eq) && all(sapply(erf_eq, is.function))) {

        rr_c <- mapply(function(f, cval) f(cval), erf_eq, exp - cutoff)

      } else if (is.character(erf_eq)) {

        erf_fun <- eval(parse(text = paste0("function(c) { ", erf_eq, " }")))

        rr_c <- erf_fun(exp - cutoff)
      }
    } else if (is.null(erf_eq)){

      # predefined shapes
      rr_c <-
        dplyr::case_when(
          erf_shape == "linear" ~ 1 + ((rr - 1) * (exp - cutoff) / rr_increment),
          erf_shape == "log_linear" ~ exp(log(rr) * (exp - cutoff) / rr_increment),
          erf_shape == "linear_log" ~ 1 + ((rr - 1) * (log(exp) - log(cutoff)) / log(rr_increment)),
          erf_shape == "log_log" ~ exp(log(rr) * (log(exp) - log(cutoff)) / log(rr_increment))
      )
    }

    return(rr_c)

  }
