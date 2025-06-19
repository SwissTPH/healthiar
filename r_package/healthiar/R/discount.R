#' Discount health impacts

#' @description
#' This function calculates discounted health impacts (without valuation).

#' @inheritParams monetize

#' @returns
#' TBD

#' @author Alberto Castro & Axel Luyten

#' @examples
#' # Goal: discount current attributable health impacts
#'
#' results <- discount(
#'   impact = 2E4,
#'   discount_shape = "exponential",
#'   discount_rate = 0.03,
#'   discount_years = 20
#' )
#'
#' results$monetization_main$monetized_impact

#' @export



discount <-
  function(output_attribute = NULL,
           impact = NULL,
           discount_rate = NULL,
           discount_years = 1,
           discount_shape = NULL,
           inflation = NULL) {

    output_discounting <-
      healthiar::monetize(
        output_attribute = output_attribute,
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
