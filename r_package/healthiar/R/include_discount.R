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
           output_healthiar = NULL,
           impact = NULL,
           discount_rate = NULL,
           discount_years = 1,
           discount_shape = NULL,
           discount_overtime = "all_years") {

    output_discounting <-
      healthiar::include_monetization(
        approach_discount = approach_discount,
        output_healthiar = output_healthiar,
        impact = impact,
        discount_rate = discount_rate,
        discount_years = discount_years,
        discount_shape = discount_shape,
        discount_overtime = discount_overtime,
        valuation = 1)


    output_discounting[["monetization_main"]] <-
      output_discounting[["monetization_main"]] |>
      dplyr::select(-contains("cost"))


    return(output_discounting)


  }
