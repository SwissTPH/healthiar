#' Attribute discounted health impact

#' @description It calculates discounted health impacts (without valuation).

#' @inheritParams include_monetization

#' @return
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function i.e. central, lower and upper bound confidence interval.
#' Moreover, the data frame includes columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }
#' @import dplyr
#' @import purrr
#' @examples
#' TBD
#' @author Alberto Castro
#' @export

include_discount <-
  function(approach_discount = "direct",
           output = NULL,
           impact = NULL,
           corrected_discount_rate = NULL,
           discount_years = 1,
           discount_shape = NULL,
           discount_overtime = "all_years") {

    output <-
      healthiar::include_monetization(
        approach_discount = approach_discount,
        output = output,
        impact = impact,
        corrected_discount_rate = corrected_discount_rate,
        discount_years = discount_years,
        discount_shape = discount_shape,
        discount_overtime = discount_overtime,
        valuation = 1) |>
      dplyr::select(-contains("cost"))


    return(output)


  }
