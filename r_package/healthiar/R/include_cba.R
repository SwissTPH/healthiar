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

    # Define vectors that are relevant below

    relevant_columns <-
        c("monetized_impact", "monetized_impact_rounded")

    suffix <-
      c("_benefit", "_cost")

    relevant_columns_with_suffix <-
      paste0(
        relevant_columns,
        rep(suffix, each = length(relevant_columns))
      )

    # Run include_monetization for benefit and cost separately
    cba_detailed_benefit <-
      healthiar::include_monetization(
        approach_discount = approach_discount,
        output = output,
        impact = positive_impact,
        corrected_discount_rate = corrected_discount_rate_benefit,
        discount_years = discount_years_benefit,
        discount_shape = discount_shape,
        discount_overtime = discount_overtime,
        valuation = valuation)[["monetization_main"]]

    # For cost, assume 1 impact with full valuation to make use of include_monetization
    cba_detailed_cost <-
      healthiar::include_monetization(
        approach_discount = approach_discount,
        impact = 1,
        valuation = cost,
        corrected_discount_rate = corrected_discount_rate_cost,
        discount_years = discount_years_cost,
        discount_shape = discount_shape,
        discount_overtime = discount_overtime)[["monetization_main"]]

    # Build the detailed output list
    cba_detailed <-
      list(
        benefit = cba_detailed_benefit,
        cost = cba_detailed_cost)

    # Get main output
    cba_main <-
      # Join benefit and cost into one df
      dplyr::left_join(
        cba_detailed_benefit,
        cba_detailed_cost,
        by = all_of(c("discount_shape", "discount_overtime")),
        suffix = suffix)|>
      # Keep only relevant columns (results)
      # dplyr::select(all_of(relevant_columns_with_suffix))|> # This line resulted in a warning: Using `all_of()` outside of a selecting function was deprecated in tidyselect 1.2.0.
      dplyr::select(relevant_columns_with_suffix)|>
      # Rename columns to make them shorter
      # Moreover, cost is not actually a monetized impact
      dplyr::rename(benefit = monetized_impact_benefit,
                    cost = monetized_impact_cost,
                    benefit_rounded = monetized_impact_rounded_benefit,
                    cost_rounded = monetized_impact_rounded_cost) |>
      # Calculate the difference between benefit and cost
      dplyr::mutate(benefit_minus_cost = benefit - cost,
                    benefit_minus_cost_rounded = round(benefit_minus_cost))



    # Build the output list with main and detailed

    output_cba <-
      list(cba_main = cba_main,
           cba_detailed = cba_detailed)



    return(output_cba)



}
