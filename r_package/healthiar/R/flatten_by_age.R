#' Obtain age-standardized health impacts

#' @description
#' This function obtains age-standardized health impacts based on multiple age-group specific assessments

#' @param listed_output_attribute \code{List} containing as sub-lists the results of \code{healthiar::attribute_health()} for each age group. Each list element should refer to one specific age group.#'
#' @param age_groups \code{String vector} with the age groups included in the age standardization. Each vector element refers to each of the list elements of \code{output_attribute}.

#' @returns Returns the impact (absolute and relative) theoretically attributable to the difference in the social indicator (e.g. degree of deprivation) between the quantiles.

#' @author
#' Alberto Castro & Axel Luyten

#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)




flatten_by_age <- function(listed_output_attribute,
                                age_groups){


  impact_by_age_group <- listed_output_attribute
  base::names(impact_by_age_group) <- age_groups

  # Add age groups as columns and bind rows
  impact_by_age_group <-
    purrr::imap_dfr(
      impact_by_age_group,
      \(x, name){
        x[["health_main"]] |>
          dplyr::mutate(age_group = name, .before = dplyr::everything())
      }
    )


  return(impact_by_age_group)



}
