#' Discount health impacts

# DESCRIPTION ##################################################################
#' @description
#' This function calculates discounted health impacts (without valuation).

# ARGUMENTS ####################################################################
#' @inheritParams monetize
#'
#'
#'
#' DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' This function applies a discount \insertCite{Frederick2002_jel,Harvey1986_ms,Mazur1987_book}{healthiar},
#' optionally with inflation \insertCite{Brealey2023_book}{healthiar},
#' to attributable health impacts into the future.
#'
#' From an epidemiological perspective, the attributable health impacts
#' cannot be discounted (or inflated), only economic costs/benefits can.
#' However, in some economic analyses the attributable health impacts are discounted (and/or inflated)
#' as a previous step to valuating them. For this specific purpose, this function is offered.
#'
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#monetization}{Monetization}}
#'
#'
# VALUE ########################################################################
#' @inherit monetize return

# EXAMPLES #####################################################################
#' @examples
#' # Goal: discount attributable health impacts
#' results <- discount(
#'   impact = 20000,
#'   discount_shape = "exponential",
#'   discount_rate = 0.03,
#'   n_years = 20
#' )
#' results$monetization_main$monetized_impact
#'
#' #' @references
#'
#' \insertAllCited{}
#'
#'
#' @author Alberto Castro & Axel Luyten
#'
#' @export



discount <-
  function(output_attribute = NULL,
           impact = NULL,
           discount_rate = NULL,
           n_years = NULL,
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
