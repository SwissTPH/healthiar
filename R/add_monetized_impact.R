#' add_monetized_impact

# DESCRIPTION ##################################################################
#' @description
#' This function calculates and adds the monetized health impacts

# ARGUMENTS ####################################################################
#' @inheritParams monetize
#' @param df \code{Data frame} including the column "impact" (health impact)
#' @param impact \code{Numberic value} referring to the health impacts to be monetized (without attribute function).

# VALUE ########################################################################
#' @returns Description of the return value.

# EXAMPLES #####################################################################
#' @examples
#' # TBD

#' @author Alberto Castro & Axel Luyten

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

  df_with_input <-
    df |>
    # Add columns for input data in the table
    # Use {{}} to clarify the it refers to the argument and not to the column
    # with the same name
    dplyr::mutate(valuation = {{valuation}},
                  discount_rate = {{discount_rate}},
                  discount_years = {{discount_years}},
                  discount_shape = {{discount_shape}},
                  inflation = inflation) |>
    # Add info
    healthiar:::add_info(
      info = info
    )

  # If impact is inserted as vector to refer to different monetized impacts by year
  # (case of real costs, not applicable for nominal costs)

  if(summing_across_discounted_years){

    df_by_year <-  df_with_input
    df_by_year$discount_year <-
      base::rep(discount_years_vector, len = base::nrow(df_with_input))

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
      conversion_factor =
        healthiar::get_discount_factor(
          discount_rate = discount_rate,
          discount_year = discount_year,
          discount_shape = discount_shape,
          inflation = inflation),
      monetized_impact = impact * valuation * conversion_factor,
      .after = impact)



    if(taking_last_discounted_year){
      df_relevant <-
        df_by_year|>
      # Keep only the last year
      dplyr::filter(discount_year == max(discount_year)) |>
        # Remove the variable discount year because it is not anymore relevant
        # (not by-year results)
        dplyr::select(-discount_year)

    }else if(summing_across_discounted_years){

      grouping_variables <-
        df_by_year |>
        dplyr::select(-dplyr::any_of(c("year", "discount_year")),
                      -dplyr::contains("discount_factor"),
                      -dplyr::contains("impact")) |>
        base::names()

      df_relevant <-
        df_by_year |>
        dplyr::summarize(
          .by = dplyr::any_of(grouping_variables),
          dplyr::across(dplyr::contains("impact"), sum)
        )
    }

  monetization_main <-
    df_relevant |>
    # Round monetized impacts
    dplyr::mutate(
      monetized_impact_rounded = base::round(monetized_impact),
      .after = monetized_impact)

  monetization <-
    base::list(
      monetization_main = monetization_main,
      monetization_detailed = df_by_year
    )

  return(monetization)

}

