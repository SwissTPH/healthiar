#' Obtain age-standardized health impacts

#' @description
#' This function obtains age-standardized health impacts based on multiple age-group specific assessments

#' @param listed_outputs_attribute \code{List} containing as sub-lists the results of \code{healthiar::attribute_health()} for each age group. Each list element should refer to one specific age group.#'
#' @param age_groups \code{String vector} with the age groups included in the age standardization. Each vector element refers to each of the list elements of \code{output_healthiar}.

#' @returns Returns the impact (absolute and relative) theoretically attributable to the difference in the social indicator (e.g. degree of deprivation) between the quantiles.

#' @author
#' Alberto Castro & Axel Luyten

#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)

#' @export



standardize <- function(output_healthiar,
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

  # Identify geo_id cols
  geo_id_cols <-
    names(impact_by_age_group)[grepl("geo_id_", names(impact_by_age_group))]

  # Identify columns with uncertainty
  uncertainty_cols <-
    names(impact_by_age_group)[grepl("_ci", names(impact_by_age_group))]

  # Identify invariant columns
  invariant_cols <- impact_by_age_group |>
    dplyr::summarize(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.x) == 1)) |>
    base::unlist() |>
    base::which() |>
    base::names()

  # Add geo_ids to the group_cols and uncertainty_cols because
  # below impacts are summed across age_groups but not geo_ids
  group_cols <-
    c(geo_id_cols,
      uncertainty_cols,
      invariant_cols)|>
    unique()


  # Calculate total population across age groups
  total_population <-
    impact_by_age_group |>
    dplyr::group_by(dplyr::across(dplyr::any_of(group_cols))) |>
    dplyr::summarize(
      total_population = base::sum(population),
      .groups = "drop")

  # Calculate age-standardize health impacts
  impact_std_by_age_group <-
    #Add total population
    dplyr::left_join(impact_by_age_group,
                     total_population,
                     by = group_cols)|>
    dplyr::mutate(
      pop_weight = population / total_population,
      impact_per_100k_inhab_std = impact_per_100k_inhab * pop_weight)

  # Remove the rows per age group category keeping only the sum
  impact_std_sum <-
    impact_std_by_age_group |>
    dplyr::group_by(dplyr::across(
      dplyr::any_of(group_cols))) |>
    dplyr::summarize(impact_per_100k_inhab = sum(impact_per_100k_inhab_std),
                     population = sum(population),
                     .groups = "drop")

  output<-
    list(health_main = impact_std_sum,
         health_detailed = impact_std_by_age_group)

  return(output)



}
