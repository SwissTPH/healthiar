#' add_monetized_impact

#' @description Function to calculate and add the monetization of the health impacts
#'
#' @param df \code{Data frame} including the column "impact" (health impact)
#' @param impact \code{Numberic value} referring to the health impacts to be monetized (without attribute function).
#' @inheritParams include_monetization
#'
#' @returns Description of the return value.
#' @examples
#' # To be added
#' @keywords internal

add_monetized_impact  <- function(df,
                                  valuation,
                                  discount_rate,
                                  discount_years,
                                  discount_shape,
                                  discount_overtime,
                                  inflation) {

  # If the discounting has to be applied in all years of the period
  if(discount_overtime == "all_years"){
    #Build a vector starting with 1
    discount_years_vector <- 0 : discount_years
    discount_period_length <- discount_years
  }else{
    # Otherwise (discount_overtime == "all_years",
    # i.e. if the discounting has to be applied only the last year)
    # only consider the last year of the period
    discount_years_vector <- discount_years
    discount_period_length <- 1
  }

  # Convert NULL into 1 so that it can be integrated in the calculations.
  # If no inflation is wished, no problem, the 0 will not change the results
  inflation <-
    ifelse(is.null(inflation),
           0,
           inflation)

  discount_rate <-
    ifelse(is.null(discount_rate),
           0,
           discount_rate)

  # Obtain the nominal discount rate if needed for the case of nominal_discount_rate
  # If the user refers to a real discount rate (without inflation), then
  # inflation is 0 and the discount rate does not change
  discount_rate_with_inflation <- ((1+discount_rate)*(1+inflation))-1




  df_with_input <-
    df |>
    # Add columns for input data in the table
    # Use {{}} to clarify the it refers to the argument and not to the column
    # with the same name
    dplyr::mutate(valuation = {{valuation}},
                  discount_rate = {{discount_rate}},
                  discount_years = {{discount_years}},
                  discount_shape = {{discount_shape}},
                  discount_overtime = {{discount_overtime}},
                  inflation = inflation,
                  discount_rate_with_inflation = discount_rate_with_inflation)

  # If impact is inserted as vector to refer to different monetized impacts by year
  # (case of real costs, not applicable for nominal costs)
  # discount_years -1 because the year 0 has been added to the df

  if(length(df$impact) == discount_years + 1){
    df_by_year <-
      df_with_input|>
      dplyr::mutate(discount_year = discount_years_vector,
                    .before = everything())

  } else {
    df_by_year <-
      # Split by discount year
      dplyr::cross_join(x = dplyr::tibble(discount_year = discount_years_vector),
                        y = df_with_input)
  }



  df_by_year <-
    df_by_year |>
    # Add monetized impact and inflation factor
    dplyr::mutate(
      monetized_impact_before_inflation_and_discount = impact * valuation) |>
    # rowwise() because discount_years is a vector
    # otherwise vectors from columns and vectors from discount_years cannot be digested
    # better step by step
    dplyr::rowwise() |>
    # Calculate discount factor
    # If any arguments "discount_rate" and "discount_shape" are NULL,
    # no discount (i.e. discount_factor=1)
    dplyr::mutate(

      inflation_factor =
        (1+inflation)^discount_year,

      discount_factor_wo_inflation =
        healthiar::get_discount_factor(
          discount_rate = discount_rate,
          discount_year = discount_year,
          discount_shape = discount_shape),

      discount_factor =
        healthiar::get_discount_factor(
          discount_rate = discount_rate_with_inflation,
          discount_year = discount_year,
          discount_shape = discount_shape))


    df_by_year <-
      df_by_year |>
      dplyr::mutate(
        monetized_impact_after_inflation = monetized_impact_before_inflation_and_discount * inflation_factor,
        monetized_impact_after_inflation_and_discount =
          monetized_impact_after_inflation / discount_factor,
        monetized_impact = monetized_impact_after_inflation_and_discount,
        .after = impact)

  df_aggregated <-
    df_by_year |>
    # Keep only the last year
    dplyr::filter(discount_year == {{discount_years}}) |>
    # Remove the variable discount year because it is not anymore relevant
    # (not by-year results)
    dplyr::select(-discount_year)


  monetization_main <-
    df_aggregated |>
    # Round monetized impacts
    dplyr::mutate(
      monetized_impact_before_inflation_and_discount_rounded = round(monetized_impact_before_inflation_and_discount),
      monetized_impact_after__inflation_and_discount_rounded = round(monetized_impact_after_inflation_and_discount),
      monetized_impact_rounded = round(monetized_impact),
      .after = monetized_impact)

  monetization <-
    list(
      monetization_main = monetization_main,
      monetization_detailed = df_by_year
    )

  return(monetization)

}

