#' Get the relative risk of an exposure level

# DESCRIPTION ##################################################################
#' @description
#' This function re-scales the relative risk from the increment value in the epidemiological study (e.g. for PM2.5 10 or 5 ug/m3) to the actual population exposure

# ARGUMENTS ####################################################################
#' @inheritParams attribute_master
#' @param rr \code{Numeric value} or \code{numeric vector} specifying the \strong{relative risk} estimate(s) and (optionally) the corresponding lower and upper 95\% confidence interval bounds. Not required if the \code{erf_eq} argument is already specified.
#' @param exp \code{Numeric value} or \code{numeric vector} specifying the \strong{exposure level(s)} to the environmental stressor (e.g. annual population-weighted mean) and (optionally) the corresponding lower and upper bound of the 95\% confidence interval.
#' @param cutoff \code{Numeric value} specifying the \strong{exposure cut-off value} (i.e. the exposure level below which no health effects occur) and (optionally) the corresponding lower and upper 95\% confidence interval bounds.
#' @param erf_eq \code{String} or \code{function} specifying the \strong{exposure-response function} and (optionally) the corresponding lower and upper 95\% confidence interval functions. See Details and Examples sections below.

# DETAILS ######################################################################
#' @details
#' \strong{Function arguments}
#' @details
#' \code{erf_eq}
#' @details
#' If the function is provided as \code{string}, it can only contain the variable c (exposure), e.g. "3+c+c^2". If the function is provided as a \code{function}, the object must be of the class function. If only the values of the x-axis (exposure) and y axis (relative risk) of the dots in the exposure-response function are available, a cubic spline natural interpolation can be assumed to get the function using, e.g., \code{stats::splinefun(x, y, method="natural")}
#' @details
#' \strong{Equations for scaling of relative risk}
#' @details
#' \emph{linear ERF}
#' \deqn{rr\_at\_exp =  1 + \frac{(rr - 1)}{rr\_increment} \cdot (exp - cutoff)}
#' @details
#' \emph{log-linear ERF}
#' @details
#' \deqn{rr\_at\_exp = e^{\frac{\log(\mathrm{rr})}{\mathrm{rr\_increment}} \cdot (\mathrm{exp} - \mathrm{cutoff})}}
#' @details
#' \emph{log-log ERF}
#' @details
#' \deqn{rr\_at\_exp = (\frac{exp + 1}{cutoff + 1})^{\frac{\log(\mathrm{rr})}{\log(\mathrm{rr\_increment + cutoff + 1}) - \log(cutoff + 1)}}}
#' @details
#' \emph{linear-log ERF}
#' @details
#' \deqn{rr\_at\_exp = 1 + \frac{\log(\mathrm{rr - 1})}{\log(\mathrm{rr\_increment + cutoff + 1}) - \log(cutoff + 1)} \cdot \frac{\log(exp + 1)}{\log(cutoff + 1)}}
#' @details
#' \strong{Sources}
#' @details
#' For the log-linear, log-log and linear-log exposure-response function equations see Pozzer et al. 2022 (https://doi.org/10.1029/2022GH000711).

# VALUE ########################################################################
#' @returns
#' This function returns the \code{numeric} risk value(s) at the specified exposure level(s), referred to as \emph{rr_at_exp} in the relative risk equations above.

# EXAMPLES #####################################################################
#' @examples
#' # Goal: scale relative risk to observed exposure level
#' get_risk(
#'   rr = 1.05,
#'   rr_increment = 10,
#'   erf_shape = "linear",
#'   exp = 10,
#'   cutoff = 5
#' )
#'
#' @examples
#' # Goal: determine the absolute risk for high annoyance at specific noise exposure levels
#' get_risk(
#'   erf_eq = "78.9270-3.1162*c+0.0342*c^2",
#'   exp = c(57.5, 62.5, 67.5, 72.5, 77.5)
#' )
#'
#' @examples
#' # Goal: attribute COPD cases to air pollution exposure by applying a user-defined exposure response function,
#' # e.g. MR-BRT curves from Global Burden of Disease study.
#' get_risk(
#'   erf_eq = splinefun(
#'     x = c(0, 5, 10, 15, 20, 25, 30, 50, 70, 90, 110),
#'     y = c(1.00, 1.04, 1.08, 1.12, 1.16, 1.20, 1.23, 1.35, 1.45, 1.53, 1.60),
#'     method = "natural"),
#'   exp = c(8, 9, 10)
#' )

#' @author Alberto Castro & Axel Luyten

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

      # If get_risk is used independently of attribute_health()
      # and only one function is entered by the user
      if(base::is.function(erf_eq)){
        rr_at_exp <- erf_eq(exp - cutoff)
        # when get_risk() is used inside attribute_health(),
        # erf_eq that are functions are encapsulated in lists to be included in tibbles
        # That is why we need is.list() and map()
        } else if (base::is.list(erf_eq) && base::all(purrr::map_lgl(erf_eq, base::is.function))) {

           rr_at_exp <- base::mapply(function(f, cval) f(cval), erf_eq, exp - cutoff)
           # A map() approach does not work here. Therefore, mapply
           # rr_at_exp <- erf_eq |>
           #   purrr::map_dbl(~ .x(exp - cutoff))


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

          # LINEAR ####
          erf_shape == "linear" ~
            1 + ( (rr - 1) * (exp - cutoff) / rr_increment ),

          # LOG-LINEAR ####
          erf_shape == "log_linear" ~
            base::exp( base::log(rr) * (exp - cutoff) / rr_increment ),

          # LOG-LOG ####
          erf_shape == "log_log" ~

            ## The curve below was proposed by ChatGPT
            ## It is not defined for exp = 0 or exp <= cutoff
            ## --> commented out
            # base::exp( base::log(rr) * ( base::log(exp - cutoff) ) / base::log(rr_increment) )

            ## This curve below follows the definition by Pozzer 2022 (http://doi.org/10.1029/2022GH000711)
            ## It is defined at all exposures and RR equals RR₁₀ when Ci=C0+10 exactly.
            ## --> implemented
            ## rr_at_exp = ((exp + 1) / (cutoff + 1)) ^ beta, where beta = log(rr) / ( log(rr_increment + cutoff + 1) - log(cutoff + 1) )
            ( ( exp + 1 ) / ( cutoff + 1 ) )^( base::log(rr) / ( base::log(rr_increment + cutoff + 1) - base::log(cutoff + 1) ) ),

          # LINEAR-LOG ####
          erf_shape == "linear_log" ~
            ## The curve below was initially proposed by ChatGPT
            ## It is not defined for exp = 0 or exp <= cutoff
            ## --> commented out
            # 1 + ( (rr - 1) * ( base::log(exp - cutoff) ) / base::log(rr_increment) )
            ## This curve below has been proposed by ChatGPT: it's an adaption of the initially proposed curve with the structure of Pozzer 2022's log-log ERF
            ## I've found no study using a lin-log ERF curve, so until now this ERF can't be validated.

        1 + ( ( rr - 1 ) / ( base::log(rr_increment + cutoff + 1) - base::log(cutoff + 1) ) ) * base::log( (exp + 1) / (cutoff + 1) )

        )
    }

    return(rr_at_exp)

  }
