#' add_monetized_impact

#' @description Function to calculate and add the monetization of the health impacts
#'
#' @param df \code{Data frame} including the column "impact" (health impact)
#' @param impact \code{Numberic value} referring to the health impacts to be monetized (without attribute function).
#' @inheritParams include_monetization
#'
#' @return Description of the return value.
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export
add_monetized_impact  <- function(df,
                                  valuation,
                                  corrected_discount_rate,
                                  discount_years,
                                  discount_shape,
                                  discount_overtime) {

  # If the discounting has to be applied in all years of the period
  if(discount_overtime == "all_years"){
    #Build a vector starting with 1
    discount_years_vector <- 1 : discount_years
    discount_period_length <- discount_years
  }else{
    # Otherwise (discount_overtime == "all_years",
    # i.e. if the discounting has to be applied only the last year)
    # only consider the last year of the period
    discount_years_vector <- discount_years
    discount_period_length <- 1
  }


  df_with_input <-
    df |>
    # Add columns for input data in the table
    # Use {{}} to clarify the it refers to the argument and not to the column
    # with the same name
    dplyr::mutate(corrected_discount_rate = {{corrected_discount_rate}},
                  discount_years = {{discount_years}},
                  discount_shape = {{discount_shape}},
                  discount_overtime = {{discount_overtime}})

  df_with_discount_factor <-
    dplyr::cross_join(x = df_with_input,
                      y = dplyr::tibble(discount_year = discount_years_vector)) |>
    # rowwise() because discount_years is a vector
    # otherwise vectors from columns and vectors from discount_years cannot be digested
    # better step by step
    dplyr::rowwise() |>
    # Calculate discount factor
    # If any arguments "corrected_discount_rate" and "discount_shape" are NULL,
    # no discount (i.e. discount_factor=1)
    dplyr::mutate(
      discount_factor =
        healthiar::get_discount_factor(
          corrected_discount_rate = corrected_discount_rate,
          discount_year = discount_year,
          discount_shape = discount_shape))

  sum_of_discount_factors <-
    df_with_discount_factor |>
    # Sum across years of time period
    # The grouping id here is the impact
    dplyr::group_by(impact) |>
    dplyr::summarise(discount_factor_overtime = sum(discount_factor),
                     .groups = "keep")

  df_with_monetization <-
    # Join the sum of discount factors with the original data
    dplyr::left_join(
      df_with_input,
      sum_of_discount_factors,
      by = "impact" ) |>
    # Add columns
    dplyr::mutate(
      # Calculate impact after discounting
      impact_before_discount = impact,
      impact_after_discount = impact / discount_period_length * discount_factor_overtime,
      impact = impact_after_discount,
      # Add column for valuation
      valuation = valuation,
      # Calculate monetized impact
      # Sum across the different discount factors
      # (one for each year of the period)
      # The default value 1 for time period enables that the calculation below
      # is not affected if no discount is demanded by the user
      monetized_impact_before_discount = impact_before_discount * valuation,
      monetized_impact_after_discount = impact_after_discount * valuation,
      monetized_impact = monetized_impact_after_discount,
      .after = impact) |>

    # Round monetized impacts
    dplyr::mutate(
      monetized_impact_before_discount_rounded = round(monetized_impact_before_discount),
      monetized_impact_after_discount_rounded = round(monetized_impact_after_discount),
      monetized_impact_rounded = round(monetized_impact),
      .after = monetized_impact)

  return(df_with_monetization)

}

