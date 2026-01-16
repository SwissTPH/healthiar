#' Calculates reference proportion of population

# DESCRIPTION ##################################################################
#' @description
#' This function calculates reference proportion of population. To be used in \code{socialize()} and \code{standardize()} in case that \code{ref_prop_pop} is not provided.

# ARGUMENTS ####################################################################
#' @param df \code{Data frame} or \code{tibble} with the data by \code{geo_id_micro} and \code{age_group} including a column for \code{population}

# VALUE ########################################################################
#' @returns
#' A \code{tibble} with the columns
#' \itemize{
#'  \item \code{age_group} containing \code{numeric} age values
#'  \item \code{ref_prop_pop} containing \code{numeric} values
#'  }

# EXAMPLES #####################################################################

#' @author Alberto Castro & Axel Luyten

#' @keywords internal



get_ref_prop_pop <-
  function(df){


    ref_prop_pop_table <-
      df |>
      dplyr::summarise(
        ref_population = sum(population, na.rm = TRUE),
        .by = age_group) |>
      dplyr::mutate(
        ref_prop_pop = ref_population / sum(ref_population)) |>
      dplyr::select(age_group, ref_prop_pop)


    return(ref_prop_pop_table)

  }
