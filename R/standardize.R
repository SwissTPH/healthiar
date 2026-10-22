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
#'
#' \strong{Combination with compare()}
#'
#' Age-standardized results cannot currently be compared with \code{compare()}.
#' \code{compare()} reads the results by age group of each assessment, i.e.
#' \code{health_detailed$results_raw}, which \code{standardize()} passes on
#' unchanged: the standardization is in \code{health_main} and in
#' \code{health_detailed$impact_std_by_age_group}, so applying
#' \code{compare()} to the output of \code{standardize()} gives exactly the
#' same result as applying it to the assessments themselves.
#' The other way round does not work either, because a comparison has one
#' exposure and one population per scenario and therefore none of the columns
#' that \code{standardize()} needs.
#' To compare two scenarios in terms of age-standardized rates, apply
#' \code{standardize()} to each of them and compare the resulting
#' \code{health_main$impact_per_100k_inhab} directly.

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
#' \code{sum(impact_per_100k_inhab_std)}, \code{sum(exp_std)} and
#' \code{sum(pop_fraction_std)}.

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

  if(is.null(ref_prop_pop)){

    ## Compile input data
    ## without social component
    input_data <-
      output_attribute$health_detailed$results_by_age_group |>
      dplyr::select(
        dplyr::any_of(c("geo_id_micro", "age_group", "population")))

    # The whole table of get_ref_prop_pop() is kept (and not only the
    # ref_prop_pop column) so that each proportion stays attached to its own
    # age group in the join below. Taking the bare vector assumed that the user
    # lists the age groups in the age_group argument in the same order as they
    # appear in results_by_age_group, and silently attached the reference
    # proportions to the wrong age groups otherwise
    ref_prop_pop_table <- get_ref_prop_pop(df = input_data)

  } else {

    # If the user enters the reference proportions, they refer to the age
    # groups in the order in which they were entered in the age_group argument
    ref_prop_pop_table <-
      tibble::tibble(age_group = age_group,
                     ref_prop_pop = ref_prop_pop)

  }


  # Identify geo_id cols
  geo_id_cols <-
    names(impact_by_age_group)[grepl("geo_id_", names(impact_by_age_group))]

  # Identify columns with uncertainty
  uncertainty_cols <-
    names(impact_by_age_group)[grepl("_ci", names(impact_by_age_group))]

  # Identify the info columns. They can identify subgroups kept apart with the
  # argument main_results_by of attribute_health() (e.g. one exposure-outcome pair
  # each), whose impacts must never be summed. They are therefore added to the
  # groups below, just like the geo units
  info_cols <-
    names(impact_by_age_group)[grepl("^info", names(impact_by_age_group))]

  # Identify invariant columns
  invariant_cols <- impact_by_age_group |>
    dplyr::summarize(dplyr::across(dplyr::everything(), ~ dplyr::n_distinct(.x) == 1)) |>
    unlist() |>
    which() |>
    names()

  # Add geo_ids to the group_cols and uncertainty_cols because
  # below impacts are summed across age_group but not geo_ids
  group_cols <-
    c(geo_id_cols,
      uncertainty_cols,
      invariant_cols,
      info_cols)|>
    unique()

  # Calculate age-standardize health impacts
  impact_std_by_age_group <-
    ## Add reference proportion of population
    dplyr::left_join(
      impact_by_age_group,
      ref_prop_pop_table,
      by = "age_group")|>
    #Add total population
    dplyr::mutate(
      # info_cols because otherwise the population of all subgroups would be
      # summed, i.e. counted as many times as subgroups there are.
      # uncertainty_cols for the same reason: every age group appears once per
      # uncertainty combination (central, lower, upper), so without them the
      # population and the baseline health data are counted once per _ci row
      .by = dplyr::any_of(c(geo_id_cols, uncertainty_cols, info_cols)),
      total_population = sum(population),
      total_impact = sum(impact)) |>
    # Calculate population weight and standardized impact
    dplyr::mutate(
      # Calculate
      pop_weight = population / total_population,
      # Share of the total impact that the age group contributes.
      # Not used for pop_fraction_std below anymore (see there), but kept
      # because it is a result on its own
      impact_weight = impact / total_impact,
      impact_per_100k_inhab_std = impact_per_100k_inhab * ref_prop_pop,
      exp_std = exp * pop_weight,
      # The baseline health data are also expressed as a rate and standardized,
      # so that the attributable fraction below can be obtained as the ratio of
      # two age-standardized rates. if_else() as in get_impact(): without the
      # population of the age group no rate can be calculated
      bhd_per_100k_inhab =
        dplyr::if_else(population > 0,
                       (bhd / population) * 1E5,
                       NA_real_),
      bhd_per_100k_inhab_std = bhd_per_100k_inhab * ref_prop_pop) |>
    # Contribution of the age group to the age-standardized attributable
    # fraction, i.e. its standardized attributable rate divided by the
    # standardized baseline rate of all age groups together. Defined so that
    # the sum across age groups is the age-standardized attributable fraction,
    # exactly like exp_std and impact_per_100k_inhab_std.
    # Previously this column was pop_fraction * impact_weight, which summed up
    # to neither the crude nor the standardized fraction.
    # In its own mutate() because the denominator is a sum across age groups
    dplyr::mutate(
      .by = dplyr::any_of(c(geo_id_cols, uncertainty_cols, info_cols)),
      pop_fraction_std =
        impact_per_100k_inhab_std / sum(bhd_per_100k_inhab_std))

  # Remove the rows per age group category keeping only the sum
  impact_std_sum <-
    impact_std_by_age_group |>
    dplyr::summarize(
      .by = dplyr::any_of(group_cols),
      bhd = sum(bhd),
      impact = sum(impact),
      impact_per_100k_inhab = sum(impact_per_100k_inhab_std),
      bhd_per_100k_inhab = sum(bhd_per_100k_inhab_std),
      # sum() and not mean(): pop_weight already adds up to 1 across the age
      # groups, so the sum of exp * pop_weight is the population-weighted mean
      # exposure. mean() divided it once more by the number of age groups
      exp = sum(exp_std),
      # The age-standardized attributable fraction, i.e. the sum of the
      # contributions of the age groups (identical to the ratio of the two
      # age-standardized rates above). Adding up the age group-specific
      # fractions instead (as before) gave a number that is not a fraction and
      # that can exceed 1
      pop_fraction = sum(pop_fraction_std),
      population = sum(population))

  output <-
    list(health_main = impact_std_sum,
               health_detailed = c(output_attribute$health_detailed,
                                   list(impact_std_by_age_group = impact_std_by_age_group)))

  return(output)



}
