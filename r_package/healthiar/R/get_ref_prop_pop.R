#' Calculates reference proportion of population

# DESCRIPTION ##################################################################
#' @description
#' This function calculates reference proportion of population. To be used in \code{socialize()} and \code{standardize()} in case that \code{ref_prop_pop} is not provided.

# ARGUMENTS ####################################################################
#' @param df \code{Data frame} or \code{tibble} with the data by \code{geo_id_micro} and \code{age_group} including a column for \code{population}

# VALUE ########################################################################
#' @returns
#' A tibble with two columns: one for \code{age_group} and a second one for \code{ref_prop_pop}.

# EXAMPLES #####################################################################
#' @examples
#' # TODO

#' @author Alberto Castro & Axel Luyten

#' @keywords internal



get_ref_prop_pop <-
  function(df){

    total_population_table <-
      df |>
      dplyr::summarize(total_population = sum(population, na.rm = TRUE))

    ref_prop_pop_table <-
      dplyr::left_join(input_data,
                       population_by_geo_table,
                       by = "age_group") |>
      dplyr::mutate(ref_prop_pop = population / total_population) |>
      base::unique(age_group) |>
      dplyr::select(age_group, ref_prop_pop)


    return(ref_prop_pop_table)

  }
