#' Obtain age-standardized health impacts

# DESCRIPTION ##################################################################
#' @description
#' This function obtains age-standardized health impacts based on multiple age-group specific assessments
#'
# ARGUMENTS ####################################################################
#' @inheritParams socialize
#'
#'
# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' This function applies the direct method of standardization,
#' where the age-specific rates observed in a study population are
#' applied to a standard (reference) population distribution.
#'
#' For age standardization in health impact assessments,
#' the World Health Organization \insertCite{Ahmad2001_report}{healthiar} and
#' the Global Burden of Disease study \insertCite{GBD2020_tldemo}{healthiar}
#' provide the relevant information on this topic.
#'
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#standardization}{Standardization}}
#'
#'
#' @details
#' This function works after running \code{attribute_health()} or \code{attribute_lifetable()} functions.
#' If you want to use it in combination with compare(),
#' please standardize first the results of attribute functions and then compare.

# VALUE ########################################################################
#' @returns
#' This function returns a \code{list} containing:
#'
#' 1) \code{health_main} (\code{tibble}) containing the main results.
#' The direct method of standardization applies the age group-specific rates
#' observed in the study population to a reference population distribution.
#' It therefore standardizes \strong{rates} and not counts:
#' \itemize{
#'  \item \code{impact_per_100k_inhab} and \code{bhd_per_100k_inhab}
#'  (\code{numeric} columns) are age-standardized, i.e. the age group-specific
#'  rates weighted with \code{ref_prop_pop};
#'  \item \code{pop_fraction} (\code{numeric} column) is the ratio of these two
#'  age-standardized rates, i.e. the age-standardized attributable fraction;
#'  \item \code{impact}, \code{bhd} and \code{population} (\code{numeric}
#'  columns) are the crude totals across the age groups, and \code{exp}
#'  (\code{numeric} column) the population-weighted mean exposure.
#'  They are \strong{not} standardized: a standardized count would require the
#'  absolute size of the reference population, while \code{ref_prop_pop}
#'  provides only its age distribution.
#'  }
#' Note that all results are identical to the crude ones if
#' \code{ref_prop_pop} is not entered, because in that case the age
#' distribution of the study population itself is taken as reference.
#'
#' 2) \code{health_detailed} (\code{tibble}) containing the results per age
#' group, including the interim columns of the standardization. The columns
#' ending in \code{_std} are the contribution of each age group and add up to
#' the corresponding column of \code{health_main}, i.e.
#' \code{base::sum(impact_per_100k_inhab_std)}, \code{base::sum(exp_std)} and
#' \code{base::sum(pop_fraction_std)}.

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
#' results$health_detailed$results_raw$impact_per_100k_inhab # age group-specific impact rate
#' results$health_main$impact_per_100k_inhab # age-standardized impact rate
#'
#'
#' @seealso
#' \itemize{
#'   \item Upstream:
#'     \code{\link{attribute_health}}, \code{\link{attribute_lifetable}}
#' }
#'
#'
#' @references
#'
#' \insertAllCited{}
#'
#'
#' @author Alberto Castro & Axel Luyten
#'
#' @export

standardize <- function(output_attribute,
                        age_group,
                        ref_prop_pop = NULL){

  impact_by_age_group <- output_attribute$health_detailed$results_by_age_group

  if(base::is.null(ref_prop_pop)){

    ## Compile input data
    ## without social component
    input_data <-
      output_attribute$health_detailed$results_by_age_group |>
      dplyr::select(
        dplyr::any_of(c("geo_id_micro", "age_group", "population")))

    ref_prop_pop <-
      get_ref_prop_pop(df = input_data)$ref_prop_pop

  }


  # Identify geo_id cols
  geo_id_cols <-
    base::names(impact_by_age_group)[base::grepl("geo_id_", base::names(impact_by_age_group))]

  # Identify columns with uncertainty
  uncertainty_cols <-
    base::names(impact_by_age_group)[base::grepl("_ci", base::names(impact_by_age_group))]

  # Identify the info columns. They can identify subgroups kept apart with the
  # argument main_results_by of attribute_health() (e.g. one exposure-outcome pair
  # each), whose impacts must never be summed. They are therefore added to the
  # groups below, just like the geo units
  info_cols <-
    base::names(impact_by_age_group)[base::grepl("^info", base::names(impact_by_age_group))]

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
      invariant_cols,
      info_cols)|>
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
      # info_cols because otherwise the population of all subgroups would be
      # summed, i.e. counted as many times as subgroups there are
      .by = dplyr::any_of(c(geo_id_cols, info_cols)),
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
      impact_per_100k_inhab = base::sum(impact_per_100k_inhab_std),
      exp = base::mean(exp_std),
      pop_fraction = base::sum(pop_fraction),
      population = base::sum(population))

  output <-
    base::list(health_main = impact_std_sum,
               health_detailed = c(output_attribute$health_detailed,
                                   list(impact_std_by_age_group = impact_std_by_age_group)))

  return(output)



}
