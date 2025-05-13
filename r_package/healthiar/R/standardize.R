#' Obtain age-standardized health impacts

#' @description
#' This function obtains age-standardized health impacts based on multiple age-group specific assessments

#' @param output_healthiar \code{List} containing the list produced by \code{healthiar::attribute_health()} or \code{healthiar::compare()} as results. Each list element should refer to each specific age group.
#' @param impact \code{Numeric vector} containing the health impacts to be used for social analysis and matched with the argument \code{geo_id_disaggregated}.
#' @param age_groups \code{String vector} with the age groups included in the age standardization. Each vector element refers to each of the list elements of \code{output_healthiar}.

#' @returns Returns the impact (absolute and relative) theoretically attributable to the difference in the social indicator (e.g. degree of deprivation) between the quantiles.

#' @author
#' Alberto Castro & Axel Luyten

#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)

#' @export



standardize <- function(output_healthiar = NULL,
                        impact = NULL,
                        age_groups){

  impact_by_age_group <- output_healthiar
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

  # Calculate total population across age groups
  total_population_age_groups <- base::sum(impact_by_age_group$population)

  # Calculate age-standardize health impacts
  impact_std_by_age_group <-
    impact_by_age_group |>
    dplyr::mutate(
      pop_weight = impact_by_age_group$population / total_population_age_groups,
      impact_per_100k_inhab_std = impact_per_100k_inhab * pop_weight
      # ,
      # impact_std_total = sum(impact_std),
      # population_total = sum(population)
      )

  # Identify invariant columns
  invariant_cols <- impact_std_by_age_group |>
    dplyr::summarize(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.x) == 1)) |>
    base::unlist() |>
    base::which() |>
    base::names()

  # Remove the rows per age group category keeping only the sum
  impact_std_sum <-
    impact_std_by_age_group |>
    dplyr::group_by(dplyr::across(dplyr::all_of(invariant_cols))) |>
    dplyr::summarize(impact_per_100k_inhab = sum(impact_per_100k_inhab_std),
                     population = sum(population),
                     .groups = "drop")

  output<-
    list(health_main = impact_std_sum,
         health_detailed = impact_std_by_age_group)

  return(output)



}
