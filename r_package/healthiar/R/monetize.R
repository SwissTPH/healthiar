#' Monetization of health impacts

#' @description Monetize health impacts
#'
#' @param output_healthiar \code{List} produced by \code{healthiar::attribute()} or \code{healthiar::compare()} as results.
#' @param impact \code{Numberic value} referring to the health impacts to be monetized (without attribute function). If a \code{Numberic vector} is entered multiple assessments (by year) will be carried out. Be aware that the value for year 0 (current) must be entered, while discount_years does not include the year 0. Thus, length of impact = discount_years + 1.
#' @param valuation \code{Numberic value} referring to unit value of a health impact
#' @param discount_rate \code{Numeric value} showing the discount rate for future years. If it is a nominal discount rate, no inflation is to be entered. If it is a real discount rate, the result can be adjusted by entering inflation in this function.
#' @param discount_shape \code{String} referring to the assumed equation for the discount factor. Per default: "exponential". Otherwise: "hyperbolic_harvey_1986" or "hyperbolic_mazur_1987".
#' @param discount_years \code{Numeric value} referring to the period of time to be considered in the discounting.
#' @param discount_overtime \code{String} that refers to the year or years where the discounting has to be applied. Options: "all-years" (i.e. all years of the period of discounting; default option) or "last_year" (only last year of discounting). Only applicable if approach_discount = "direct".
#' @param inflation \code{Numeric value} between 0 and 1 referring to the annual inflation (increase of prices). Ony to be entered if nominal (not real) discount rate is entered in the function. Default value = NULL (assuming no nominal discount rate)
#'
#' @returns Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
monetize <-
  function(output_healthiar = NULL,
           impact = NULL,
           valuation,
           discount_rate = NULL,
           discount_shape = NULL,
           discount_years = 0,
           discount_overtime = "all_years",
           inflation = NULL) {

    # Store variables to increase readability of conditions
    using_impact_from_healthiar <-
      !is.null(output_healthiar) & is.null(impact)
    using_impact_from_user <-
      !using_impact_from_healthiar

    is_lifetable <-
      is.null(output_healthiar[["health_detailed"]][["input_args"]]$bhd_central)
    is_not_lifetable <-
      !is_lifetable


  # Using the output of attribute ####
  if(using_impact_from_healthiar){

    # Using life table method for the health assessment #######
    # If bhd is null, it means that a life table (age-specific mortality) was entered.
    if(is_lifetable){

      health_outcome <-
        output_healthiar[["health_detailed"]][["input_args"]]$health_outcome

      # Store the original data (they refer to health)
      output_health <- output_healthiar

      # Output will be adapted according to monetized impacts
      impact_detailed <-
        output_health[["health_detailed"]][["impact_raw"]] |>

        ## Calculate total, discounted life years (single value) per sex & ci
        dplyr::mutate(
          impact_with_discount_nest = purrr::pmap(
            list(.x = lifeyears_nest),
            function(.x){

              ## Calculate total, discounted life years (single value) per sex & ci ####
              lifeyear_nest_with_and_without_discount <-
                .x |>
                # Convert year to numeric
                dplyr::rowwise() |>
                dplyr::mutate(year = as.numeric(year),
                              # Ignore user defined discount_years
                              # Here the difference between year of analysis and
                              # last year of mortality data is to be used
                              discount_years = year - {{year_of_analysis}},
                              discount_rate = {{discount_rate}},
                              discount_shape = {{discount_shape}})

              lifeyear_nest_with_and_without_discount <-
                healthiar:::add_monetized_impact(df = lifeyear_nest_with_and_without_discount,
                                                 discount_rate = discount_rate,
                                                 discount_years = length(lifeyear_nest_with_and_without_discount$discount_years)-1,
                                                 discount_shape = discount_shape,
                                                 discount_overtime = discount_overtime,
                                                 inflation = inflation,
                                                 valuation = valuation)[["monetization_main"]]

              return(lifeyear_nest_with_and_without_discount)

              }))

      ## If yll or yld
      if({{health_outcome}} %in% c("yll")){ # And "yld" if ever implemented

        impact_detailed <-
          impact_detailed |>
          dplyr::mutate(
            impact_with_discount_summed = purrr::pmap(
              list(.x = impact_with_discount_nest),
              function(.x, .y){
                impact_with_summed_discount <-
                  .x |>
                  #Deactivated filter because probably not needed anymore
                  #Year is always lower than the last_year and
                  # the non-relevant years (>last_year) excluded from the calculation in get_deaths_yll_yld()
                  #TODO To be confirmed
                  # Filter for the relevant years
                  # dplyr::filter(year < .y+1) |>
                  ## Sum among years to obtain the total impact (single value)
                  dplyr::summarise(
                    impact = sum(impact, na.rm = TRUE),
                    monetized_impact_before_inflation_and_discount = sum(monetized_impact_before_inflation_and_discount, na.rm = TRUE),
                    monetized_impact_after_inflation_and_discount = sum(monetized_impact_after_inflation_and_discount, na.rm = TRUE),
                    monetized_impact = sum(monetized_impact, na.rm = TRUE),
                    .groups = "drop")

                return(impact_with_summed_discount)
              }

            )
          )
      }

      impact_detailed <-
        impact_detailed |>
        # Remove column impact to avoid duplication
        dplyr::select(-impact) |>
        ## Unnest the obtained impacts to integrate them the main tibble
        ## Impact saved in column impact
        tidyr::unnest(impact_with_discount_summed) |>
        # Round results
        dplyr::mutate(
          # Round impacts and monetized impacts
          impact_rounded = round(impact),
          monetized_impact_rounded = round(monetized_impact),
          monetized_impact_before_discount_rounded = round(monetized_impact_before_inflation_and_discount),
          monetized_impact_after_discount_rounded = round(monetized_impact_after_inflation_and_discount))





      # Calculate impact per 100K inhab.

      if("population" %in% colnames(impact_detailed)){
        impact_detailed <-
          impact_detailed |>
          dplyr::mutate(
            impact_per_100k_inhab = (impact / population) *1E5
          )
      }


      # Get the main and detailed output by aggregating and/or filtering cases (rows)
      output_monetization <-
        healthiar:::get_output(impact_raw = impact_detailed) |>
        # Rename the list elements (not anymore health but health including monetization)
        setNames(c("monetization_main", "monetization_detailed"))

      # Keep only the main detailed data frame (raw) for monetization
      output_monetization[["monetization_detailed"]] <-
        output_monetization[["monetization_detailed"]][["impact_raw"]]

      # Add the list elements health_main and health_detailed
      output_monetization <-
        c(output_health,
          output_monetization)


    # Without using life table #######
    # If bhd_central is not NULL, then we are not using life table method
    }else if (is_not_lifetable){


      # Duplicate output to work with monetization
      output_monetization <-
        output_healthiar

      # Apply the function in main and detailed results
      output_monetization[["monetization_main"]] <-
        healthiar:::add_monetized_impact(df = output_healthiar[["health_main"]],
                                         valuation = valuation,
                                         discount_rate = discount_rate,
                                         discount_years = {{discount_years}},
                                         discount_shape = discount_shape,
                                         discount_overtime = discount_overtime,
                                         inflation = inflation)[["monetization_main"]]

      #Detailed results showing the by-year results of monetization
      output_monetization[["monetization_detailed"]][["by_year"]] <-
        healthiar:::add_monetized_impact(df = output_healthiar[["health_main"]],
                                         valuation = valuation,
                                         discount_rate = discount_rate,
                                         discount_years = {{discount_years}},
                                         discount_shape = discount_shape,
                                         discount_overtime = discount_overtime,
                                         inflation = inflation)[["monetization_detailed"]]

      #Detailed results showing all the details of the health results
      output_monetization[["monetization_detailed"]][["health_raw"]]<-
        healthiar:::add_monetized_impact(df = output_healthiar[["health_detailed"]][["impact_raw"]],
                                         valuation = valuation,
                                         discount_rate = discount_rate,
                                         discount_years = {{discount_years}},
                                         discount_shape = discount_shape,
                                         discount_overtime = discount_overtime,
                                         inflation = inflation)[["monetization_main"]]
    }


    # For both with and without life table
    # Identify the relevant columns for monetization that are in the output
    relevant_columns <-
      c("info", "geo_id_disaggregated", "geo_id_aggregated",
        paste0("impact", c("", "_before_inflation_and_discount", "_after_inflation_and_discount")),
        "discount_rate", "discount_shape", "discount_overtime", "approach_discount",
        "valuation",
        paste0("monetized_impact", c("", "_before_inflation_and_discount", "_after_inflation_and_discount")),
        paste0("monetized_impact", c("", "_before_inflation_and_discount", "_after_inflation_and_discount"), "_rounded"))


    # Keep only relevant columns for monetization
    output_monetization[["monetization_main"]] <-
      output_monetization[["monetization_main"]] |>
      dplyr::select(
        # The columns containing "_ci" are the uncertainties that define the rows
        contains("_ci"),
        # Use any_of() instead of all_of() because depending on the calculation pathway
        # there might not be any of the relevant_columns
        any_of(relevant_columns))



    # Using user input ####
    # If the user only provide a number of the impact (not based on output of attribute)
    # No life table approach when user is entering the health impacts
    # because we cannot access the life table calculation to discount by year
    }else if(using_impact_from_user){

      output_monetization <-
        healthiar:::add_monetized_impact(
          df = data.frame(impact = impact),
          valuation = valuation,
          discount_rate = discount_rate,
          discount_years = discount_years,
          discount_shape = discount_shape,
          discount_overtime = discount_overtime,
          inflation = inflation)

  }


  return(output_monetization)

}
