#' Obtain age-standardized health impacts

# DESCRIPTION ##################################################################
#' @description
#' This function obtains age-standardized health impacts based on multiple age-group specific assessments

# ARGUMENTS ####################################################################
#' @inheritParams socialize

# VALUE ########################################################################
#' @returns Returns the impact (absolute and relative) theoretically attributable to the difference in the social indicator (e.g. degree of deprivation) between the quantiles.

# EXAMPLES #####################################################################
#' @examples
#' # Goal: age-standardize two age group-specific impacts
#' output_attribute <- attribute_health(
#'   rr_central = 1.063,
#'   rr_increment = 10,
#'   erf_shape = "log_linear",
#'   cutoff_central =  0,
#'   age_group = c("below_40", "above_40"),
#'   exp_central = c(8.1, 10.9),
#'   bhd_central = c(1000, 4000),
#'   population = c(100000, 500000)
#' )
#' results <- standardize(
#'   output_attribute = output_attribute,
#'   age_group = c("below_40", "above_40"),
#'   ref_prop_pop = c(0.5, 0.5)
#' )
#' results$health_detailed$impact_per_100k_inhab # age group-specific impact rate
#' results$health_main$impact_per_100k_inhab # age-standardized impact rate

#' @author Alberto Castro & Axel Luyten

#' @export



standardize <- function(output_attribute,
                        age_group,
                        ref_prop_pop = NULL){

  impact_by_age_group <- output_attribute$health_detailed$results_by_age_group


  if(is.null(ref_prop_pop)){

    ref_prop_pop <-
      healthiar:::get_ref_prop_pop(df = input_data)$ref_prop_pop

  }


  # Identify geo_id cols
  geo_id_cols <-
    base::names(impact_by_age_group)[base::grepl("geo_id_", base::names(impact_by_age_group))]

  # Identify columns with uncertainty
  uncertainty_cols <-
    base::names(impact_by_age_group)[base::grepl("_ci", base::names(impact_by_age_group))]

  # Identify invariant columns
  invariant_cols <- impact_by_age_group |>
    dplyr::summarize(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.x) == 1)) |>
    base::unlist() |>
    base::which() |>
    base::names()

  # Add geo_ids to the group_cols and uncertainty_cols because
  # below impacts are summed across age_group but not geo_ids
  group_cols <-
    c(geo_id_cols,
      uncertainty_cols,
      invariant_cols)|>
    base::unique()

  # Calculate age-standardize health impacts
  impact_std_by_age_group <-
    ## Add reference proportion of population
    dplyr::left_join(
      impact_by_age_group,
      tibble::tibble(age_group = age_group,
                     ref_prop_pop = ref_prop_pop),
      by = "age_group")|>
    #Add total population
    dplyr::mutate(
      .by = dplyr::any_of(geo_id_cols),
      total_population = base::sum(population),
      total_impact = base::sum(impact)) |>
    # Calculate population weight and standardized impact
    dplyr::mutate(
      # Calculate
      pop_weight = population / total_population,
      impact_weight = impact/total_impact,
      impact_per_100k_inhab_std = impact_per_100k_inhab * ref_prop_pop,
      exp_std = exp * pop_weight,
      pop_fraction_std = pop_fraction * impact_weight)

  # Remove the rows per age group category keeping only the sum
  impact_std_sum <-
    impact_std_by_age_group |>
    dplyr::summarize(
      .by = dplyr::any_of(group_cols),
      bhd = base::sum(bhd),
      impact = base::sum(impact),
      impact_per_100k_inhab = sum(impact_per_100k_inhab_std),
      exp = base::mean(exp_std),
      pop_fraction = base::sum(pop_fraction),
      population = base::sum(population))

  output<-
    base::list(health_main = impact_std_sum,
               health_detailed = impact_std_by_age_group)

  return(output)



}
