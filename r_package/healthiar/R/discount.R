#' Discount health impacts

#' @description
#' This function calculates discounted health impacts (without valuation).

#' @inheritParams monetize

#' @returns
#' TBD

#' @examples
#' TBD

#' @author Alberto Castro & Axel Luyten

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
