#' include_cba

#' @description Perform cost-benefit analysis assuming that the monitized health impact is a benefit
#'
#' @inheritParams include_monetization
#' @param corrected_discount_rate_benefit,corrected_discount_rate_cost \code{Numeric value} referring to the the discount rate used in the benefit and the cost side (respectively). Their values determine the approach of cost-benefit analysis: direct approach (if the same discount_rate is used for cost and benefit) and indirect approach (different discount rates)
#' @param benefit \code{Numeric value} referring to the positive health impact as result of a reduction of harmful exposure
#' @param cost \code{Numeric value} referring to the investment cost to achive the reduction of exposure
#'
#' @return Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
include_cba <-
  function(approach_discount = "direct",
           output = NULL,
           positive_impact = NULL,
           valuation,
           cost,
           corrected_discount_rate_benefit = NULL,
           corrected_discount_rate_cost = NULL,
           discount_shape = NULL,
           discount_years_benefit = 1,
           discount_years_cost = 1,
           discount_overtime = "all_years") {

    relevant_columns <-
        c("monetized_impact", "monetized_impact_rounded")

    suffix <-
      c("_benefit", "_cost")

    relevant_columns_with_suffix <-
      paste0(
        relevant_columns,
        rep(suffix, each = length(relevant_columns))
      )



    cba_detailed_benefit <-
      healthiar::include_monetization(
        approach_discount = approach_discount,
        output = output,
        impact = positive_impact,
        corrected_discount_rate = corrected_discount_rate_benefit,
        discount_years = discount_years_benefit,
        discount_shape = discount_shape,
        discount_overtime = discount_overtime,
        valuation = valuation)

    cba_detailed_cost <-
      healthiar::include_monetization(
        approach_discount = approach_discount,
        impact = 1,
        valuation = cost,
        corrected_discount_rate = corrected_discount_rate_cost,
        discount_years = discount_years_cost,
        discount_shape = discount_shape,
        discount_overtime = discount_overtime)


    cba_detailed <-
      list(
        benefit = cba_detailed_benefit,
        cost = cba_detailed_cost)


    cba_main <-
      dplyr::left_join(
        cba_detailed_benefit,
        cba_detailed_cost,
        by = all_of(c("discount_shape", "discount_overtime")),
        suffix = suffix)|>
      dplyr::select(all_of(relevant_columns_with_suffix))|>
      dplyr::rename(benefit = monetized_impact_benefit,
                    cost = monetized_impact_cost,
                    benefit_rounded = monetized_impact_rounded_benefit,
                    cost_rounded = monetized_impact_rounded_cost) |>
      dplyr::mutate(benefit_minus_cost = benefit - cost,
                    benefit_minus_cost_rounded = round(benefit_minus_cost))




    if(!is.null(positive_impact) & is.null(output)){

      output_cba <-
        list(cba_main = cba_main,
             cba_detailed = cba_detailed)


    }



}
