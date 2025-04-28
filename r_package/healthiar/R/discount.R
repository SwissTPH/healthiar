#' Discount health impacts

#' @description This function calculates discounted health impacts (without valuation).

#' @inheritParams monetize

#' @returns
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function i.e. central, lower and upper bound confidence interval.
#' Moreover, the data frame includes columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }
#' @examples
#' TBD
#' @author Alberto Castro
#' @export

discount <-
  function(output_healthiar = NULL,
           impact = NULL,
           discount_rate = NULL,
           discount_years = 1,
           discount_shape = NULL,
           inflation = NULL) {

    output_discounting <-
      healthiar::monetize(
        output_healthiar = output_healthiar,
        impact = impact,
        discount_rate = discount_rate,
        discount_years = discount_years,
        discount_shape = discount_shape,
        valuation = 1,
        inflation = inflation)


    output_discounting[["monetization_main"]] <-
      output_discounting[["monetization_main"]] |>
      dplyr::select(-dplyr::contains("cost"))


    return(output_discounting)


  }
