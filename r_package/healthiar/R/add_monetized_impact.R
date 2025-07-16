#' add_monetized_impact

#' @description
#' This function calculates and adds the monetized health impacts

#' @inheritParams monetize
#' @param df \code{Data frame} including the column "impact" (health impact)
#' @param impact \code{Numberic value} referring to the health impacts to be monetized (without attribute function).

#' @returns Description of the return value.

#' @examples
#' # To be added

#' @keywords internal

add_monetized_impact  <-
  function(df,
           valuation,
           discount_rate,
           discount_years,
           discount_shape,
           inflation,
           info = NULL) {
    # If df has only one column (impact)
    # it means that this is the direct input from user
    # no previous health assessment with healthiar
    using_impact_from_user <- base::ncol(df) == 1
    using_impact_vector <- base::length(df$impact)>1
    using_impact_vector_from_user <- using_impact_from_user & using_impact_vector
    using_impact_value_from_user <- using_impact_from_user & !using_impact_vector
    using_impact_from_healthiar <- !using_impact_from_user
    using_lifetable <- "year" %in% base::names(df)
    using_impact_from_healthiar_with_lifetable <- using_impact_from_healthiar & using_lifetable
    using_impact_from_healthiar_without_lifetable <- using_impact_from_healthiar & !using_lifetable

    # Definition of calculation pathways
    taking_last_discounted_year <- using_impact_from_healthiar_without_lifetable | using_impact_value_from_user
    summing_across_discounted_years <- using_impact_vector_from_user | using_impact_from_healthiar_with_lifetable

    # Define discount years
    discount_years_vector <- 0 : discount_years
    discount_period_length <- discount_years

  # Convert NULL into 1 so that it can be integrated in the calculations.
  # If no inflation is wished, no problem, the 0 will not change the results
  inflation <-
    base::ifelse(base::is.null(inflation),
                 0,
                 inflation)

  discount_rate <-
    base::ifelse(base::is.null(discount_rate),
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
                  inflation = inflation,
                  discount_rate_with_inflation = discount_rate_with_inflation) |>
    # Add info
    healthiar:::add_info(
      info = info
    )

  # If impact is inserted as vector to refer to different monetized impacts by year
  # (case of real costs, not applicable for nominal costs)

  if(summing_across_discounted_years){

    df_by_year <-  df_with_input
    df_by_year$discount_year <- discount_years_vector

  } else if(taking_last_discounted_year){
    df_by_year <-
      # Split by discount year
      dplyr::cross_join(x = tibble::tibble(discount_year = discount_years_vector),
                        y = df_with_input)
  }



  df_by_year <-
    df_by_year |>
    # Add monetized impact and inflation factor
    dplyr::mutate(
      monetized_impact_before_inflation_and_discount = impact * valuation,

      # Calculate discount factor
      # If any arguments "discount_rate" and "discount_shape" are NULL,
      # no discount (i.e. discount_factor=1)
      inflation_factor =  (1+inflation)^discount_year,

      # Add discount factor without and with inflation
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

    grouping_variables <-
      df_by_year |>
      dplyr::select(starts_with("geo_id"),
                    dplyr::ends_with("_ci"),
                    dplyr::any_of(c("discount_shape"))) |>
      base::names()


    if(taking_last_discounted_year){
      df_relevant <-
        df_by_year|>
      # Keep only the last year
      dplyr::filter(discount_year == max(discount_year)) |>
        # Remove the variable discount year because it is not anymore relevant
        # (not by-year results)
        dplyr::select(-discount_year)

    }else if(summing_across_discounted_years){
      df_relevant <-
        df_by_year |>
        dplyr::group_by(dplyr::across(dplyr::any_of(grouping_variables)))|>
        dplyr::summarize(
          dplyr::across(dplyr::starts_with("monetized"), sum)
        )
    }

  monetization_main <-
    df_relevant |>
    # Round monetized impacts
    dplyr::mutate(
      monetized_impact_before_inflation_and_discount_rounded = base::round(monetized_impact_before_inflation_and_discount),
      monetized_impact_after__inflation_and_discount_rounded = base::round(monetized_impact_after_inflation_and_discount),
      monetized_impact_rounded = base::round(monetized_impact),
      .after = monetized_impact)

  monetization <-
    base::list(
      monetization_main = monetization_main,
      monetization_detailed = df_by_year
    )

  return(monetization)

}

