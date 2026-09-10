#' Get inflation factor

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the inflation factor based on inflation rate.

# ARGUMENTS ####################################################################
#' @inheritParams monetize
#' @param is_deflation \code{Boolean value} (TRUE vs. FALSE) referring to the type of inflation factor.
#' FALSE (default) means inflate present values to future nominal values,
#' while TRUE means deflate future nominal values to present real values

# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' This function is called inside \code{monetize()}.
#'
#' It calculates the inflation factor based on the inflation rate
#' and the number of years into the future as described
#' in \insertCite{Brealey2023_book;textual}{healthiar}.
#'
#' If \code{inflation_rate} contains one single value,
#' inflation is assumed to be constant over time and
#' the inflation factor increases exponentially with the number of years.
#' If \code{inflation_rate} contains a vector of year-specific rates,
#' the inflation factor is the product of the year-specific factors,
#' which better reflects that inflation varies over time.
#' In that case, \code{inflation_rate} must contain
#' at least as many values as years to be considered (\code{n_years}),
#' the first value referring to the first year after the present.
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#monetization}{Monetization}}
#'
# VALUE ########################################################################
#' @returns This function returns the \code{numeric} inflation factor.


# EXAMPLES #####################################################################
#' @examples
#' # Constant inflation rate
#' get_inflation_factor(
#'   inflation_rate = 0.02,
#'   n_years = 5
#' )
#'
#' # Year-specific inflation rates
#' get_inflation_factor(
#'   inflation_rate = c(0.02, 0.03, 0.05, 0.04, 0.02),
#'   n_years = 5
#' )
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


get_inflation_factor <-
  function(n_years,
           inflation_rate = NULL,
           is_deflation = FALSE){

    # Variables for ifs #####################

    ## Create readable variables for if statements below

    ## No adjustment for inflation
    has_no_inflation_rate <- base::is.null(inflation_rate)

    ## Inflation constant over time (one single rate) vs.
    ## varying over time (one rate per year)
    is_constant <- base::length(inflation_rate) == 1
    is_year_specific <- base::length(inflation_rate) > 1


    # Data validation ######################
    ## error_if_too_few_rates #####
    # One rate per year is needed
    # to obtain the product of the year-specific factors below
    if(is_year_specific && base::length(inflation_rate) < base::max(n_years)){
      base::stop(
        base::paste0("inflation_rate must contain either one single value ",
                     "(constant inflation) or at least as many values as ",
                     "years to be considered (n_years)."),
        call. = FALSE)
    }


    # Get inflation factor ######################

    if(has_no_inflation_rate){

      inflation_factor <- 1

    } else if(is_constant){ # Constant inflation rate
      # The same rate applies to all years,
      # so the factor grows exponentially with the number of years

      inflation_factor <- (1 + inflation_rate) ^ n_years

    } else if(is_year_specific){ # Year-specific inflation rates
      # Each year has its own rate,
      # so the factor is the product of the year-specific factors

      # map_dbl to vectorialize the function
      # and consequently to accept vectors in n_years
      inflation_factor <-
        purrr::map_dbl(
          n_years,
          \(i) base::prod(1 + inflation_rate[base::seq_len(i)]))
    }

    if(is_deflation){ # Deflation
      # Deflation is the inverse of inflation

      inflation_factor <- 1 / inflation_factor
    }

    return(inflation_factor)
  }
