#' Discount health impacts

# DESCRIPTION ##################################################################
#' @description
#' This function calculates discounted health impacts (without valuation).

# ARGUMENTS ####################################################################
#' @inheritParams monetize

# VALUE ########################################################################
#' @returns
#' This function returns discounted health impacts.

# EXAMPLES #####################################################################
#' @examples
#' # Goal: discount current attributable health impacts
#' results <- discount(
#'   impact = 2E4,
#'   discount_shape = "exponential",
#'   discount_rate = 0.03,
#'   n_years = 20
#' )
#' results$monetization_main$monetized_impact

#' @author Alberto Castro & Axel Luyten

#' @export



discount <-
  function(output_attribute = NULL,
           impact = NULL,
           discount_rate = NULL,
           n_years = 1,
           discount_shape = NULL,
           inflation_rate = NULL) {

    output_discounting <-
      monetize(
        output_attribute = output_attribute,
        impact = impact,
        discount_rate = discount_rate,
        n_years = n_years,
        discount_shape = discount_shape,
        valuation = 1,
        inflation_rate = inflation_rate)


    output_discounting[["monetization_main"]] <-
      output_discounting[["monetization_main"]] |>
      dplyr::select(-dplyr::contains("cost"))


    return(output_discounting)


  }
