#' Get inflation factor

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the inflation factor based on inflation rate.

# ARGUMENTS ####################################################################
#' @inheritParams monetize
#' @param deflation \code{Boolean value} (TRUE vs. FALSE) referring to the type of inflation factor.
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
#' get_inflation_factor(
#'   inflation_rate = 0.02,
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
           deflation = FALSE){

    if(!base::is.null(inflation_rate) & deflation == FALSE){ # Default
      # if discount_rate is NULL

      inflation_factor <- (1 + inflation_rate) ^ n_years

    } else if (!base::is.null(inflation_rate) & deflation == TRUE){ # Deflation
      # if discount_rate is NULL

      inflation_factor <- 1/((1 + inflation_rate) ^ n_years)

    } else {

      inflation_factor <- 1
    }

    return(inflation_factor)
  }
