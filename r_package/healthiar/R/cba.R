#' Cost-benefit analysis

#' @description
#' This function performs a cost-benefit analysis

#' @inheritParams monetize
#' @param discount_rate_benefit,discount_rate_cost \code{Numeric value} referring to the the discount rate used in the benefit and the cost side (respectively). Their values determine the approach of cost-benefit analysis: direct approach (if the same discount_rate is used for cost and benefit) and indirect approach (different discount rates)
#' @param benefit \code{Numeric value} referring to the positive health impact as result of a reduction of harmful exposure
#' @param cost \code{Numeric value} referring to the investment cost to achieve the reduction of exposure

#' @returns Description of the return value.

#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)

#' @export



cba <-
  function(output_attribute = NULL,
           positive_impact = NULL,
           valuation,
           cost,
           discount_rate_benefit = NULL,
           discount_rate_cost = NULL,
           discount_shape = "exponential",
           discount_years_benefit = 1,
           discount_years_cost = 1) {

    # Define vectors that are relevant below

    columns_monetization <-
        c("monetized_impact", "monetized_impact_rounded")

    suffix_monetization <-
      c("_benefit", "_cost")

    columns_monetization_with_suffix <-
      paste0(
        columns_monetization,
        rep(suffix_monetization, each = length(columns_monetization))
      )

    # Run include_monetization for benefit and cost separately
    # Important to obtain main and detailed to avoid losing information

    cba_detailed_benefit <-
      healthiar::monetize(
        output_attribute = output_attribute,
        impact = positive_impact,
        discount_rate = discount_rate_benefit,
        discount_years = discount_years_benefit,
        discount_shape = discount_shape,
        valuation = valuation)[["monetization_detailed"]]

    cba_main_benefit <-
      healthiar::monetize(
        output_attribute = output_attribute,
        impact = positive_impact,
        discount_rate = discount_rate_benefit,
        discount_years = discount_years_benefit,
        discount_shape = discount_shape,
        valuation = valuation)[["monetization_main"]]



    # For cost, assume 1 impact with full valuation to make use of include_monetization
    cba_detailed_cost <-
      healthiar::monetize(
        impact = 1,
        valuation = cost,
        discount_rate = discount_rate_cost,
        discount_years = discount_years_cost,
        discount_shape = discount_shape)[["monetization_main"]]

    # For costs main and detailed are the same because they only have one row
    cba_main_cost <- cba_detailed_cost

    # Build the detailed output list
    cba_detailed <-
      list(
        benefit = cba_detailed_benefit,
        cost = cba_detailed_cost)

    # Get main output
    cba_main <-
      # Join benefit and cost into one df
      dplyr::left_join(
        cba_main_benefit,
        cba_main_cost,
        by = c("discount_shape"),
        suffix = suffix_monetization)


    # Store names of columns with ci and geo_id
    # These columns define the different cases (rows)
    # This intermediate step is needed to ensure that no errors are produced
    # if no columns with ci or geo are available
    # (i.e, without using the function attribute in a previous step)
    columns_ci_geo <-
      names(cba_main)[grepl("_ci|geo_id", names(cba_main))]

    relevant_columns <-
      c(columns_ci_geo,
        columns_monetization_with_suffix,
        "discount_shape")


    cba_main <-
      cba_main |>
      # Keep only relevant columns
      dplyr::select(all_of(relevant_columns))|>
      # Moreover, cost is not actually a monetized impact
      dplyr::rename(benefit = monetized_impact_benefit,
                    cost = monetized_impact_cost,
                    benefit_rounded = monetized_impact_rounded_benefit,
                    cost_rounded = monetized_impact_rounded_cost) |>
      # Calculate the difference between benefit and cost (net_benefit)
      # as well as cbr (cost-benefit ratio) and roi (return of investment)
      dplyr::mutate(net_benefit = benefit - cost,
                    net_benefit_rounded = round(net_benefit),
                    cbr = benefit / cost,
                    roi = (benefit - cost) / cost * 100)




    # Build the output list with main and detailed

    output_cba <-
      list(cba_main = cba_main,
           cba_detailed = cba_detailed)



    if(is.null(positive_impact) & !is.null(output_attribute)){
      output <-
        c(output_attribute,
          output_cba)

    }else if(!is.null(positive_impact) & is.null(output_attribute)){
     output <- output_cba
    }



    return(output)



}
