#' Attributable disability-adjusted life years

#' @description
#' This function calculates the disability-adjusted life years (DALY) attributable to the exposure to an environmental stressor by adding the two DALY components YLL and YLD.

#' @param output_attribute_yll,output_attribute_yld \code{variable} containing YLL or YLD results of a \code{attribute_...()} function call, respectively.

#' @inherit attribute_master return

#' @author Alberto Castro & Axel Luyten

#' @examples
#' # Goal: obtain DALY (disability-adjusted life years) from two existing
#' # attribute_... outputs
#'
#' ## Create YLL (years of life lost) assessment
#' results_yll <- attribute_lifetable(
#'   health_outcome = "yll",
#'   approach_exposure = "single_year",
#'   exp_central = 8.85,
#'   prop_pop_exp = 1,
#'   cutoff_central = 5,
#'   rr_central =  1.118,
#'   rr_increment = 10,
#'   erf_shape = "log_linear",
#'   first_age_pop = 0,
#'   last_age_pop = 99,
#'   deaths_male = exdat_pop_1$number_of_deaths_male,
#'   deaths_female = exdat_pop_1$number_of_deaths_female,
#'   population_midyear_male = exdat_pop_male$population_2019,
#'   population_midyear_female = exdat_pop_female$population_2019,
#'   year_of_analysis = 2019,
#'   min_age = 20
#' )
#'
#' ## Create YLD (years lived with disability) assessment
#' results_yld  <- attribute_health(
#'   exp_central = 8.85,
#'   prop_pop_exp = 1,
#'   cutoff_central = 5,
#'   bhd_central = 1000,
#'   rr_central = 1.1,
#'   rr_increment = 10,
#'   erf_shape = "log_linear",
#'   duration_central = 100,
#'   dw_central = 0.5,
#'   info = "pm2.5_yld"
#' )
#'
#' results <- daly(
#'   output_attribute_yll = results_yll,
#'   output_attribute_yld = results_yld
#' )
#'
#' # Attributable impact in DALY
#' results$health_main |>
#'   dplyr::select(impact, impact_yll, impact_yld)

#' @export

daly <-
  function(
    output_attribute_yll,
    output_attribute_yld){

    # Capture all arguments and values
    input_args <-
      healthiar:::get_input_args(environment = base::environment(),
                                 call = match.call())

    # Store results_raw of yll and yld
    # Shorter and handy to code
    results_raw_yll <- output_attribute_yll[["health_detailed"]][["impact_agg_sex"]] |>
      dplyr::filter(sex=="total")
    results_raw_yld <- output_attribute_yld[["health_detailed"]][["impact_agg_sex"]]

    # Capture all column names
    # They should be the same for yll and yld but just in case
    column_names_results_raw <-
      unique(c(names(results_raw_yll), names(results_raw_yld)))

    # Identify the columns names using keywords
    common_columns <-
      column_names_results_raw[grepl("exp|exposure|cutoff|geo|approach_risk|sex",
                                     #TODO: age_group is to be added here as soon as it is an output in lifetable approach
                                    column_names_results_raw)]
    # Remove exceptions (columns with any of the keywords that should not be selected)
    common_columns <- common_columns[!grepl("approach_exposure|rr_at_exp", common_columns)]
    common_columns_for_join <- c(common_columns, "erf_ci")


    common_columns_identical <-
      healthiar:::check_if_args_identical(
        args_a = input_args$value$output_attribute_yld,
        args_b = input_args$value$output_attribute_yld,
        names_to_check = common_columns)


    if(!all(common_columns_identical))
    {stop("The arguments ",
          paste(names(common_columns_identical)[common_columns_identical]
                , collapse = ", "),
          " must be identical in both scenarios")}



    # Remove those containing the word impact
    column_names_results_raw_without_impact <-
      column_names_results_raw[!grepl("impact|lifeyears|lifetable", column_names_results_raw)]


    # Obtain the new results_raw for DALY
    results_raw <-
      # Join results_raw tables from yll and yld
      # but giving a suffix _yll and _yld to free the name "impact" to YLD
      # We need to use "impact" as final result to be consistent with the other
      # healthiar functions
      dplyr::full_join(
        results_raw_yll,
        results_raw_yld,
        by = common_columns_for_join,
        suffix = c("_yll", "_yld")) |>
      dplyr::mutate(
        # Add metric
        outcome_metric = "daly",
        # Add impact as sum of yll and yld (including rounded impact)
        impact = impact_yll + impact_yld,
        impact_rounded = round(impact))

    # Add impact per 100k inhabitants if population is available
    if("population" %in% names(results_raw)){
      results_raw <-
        results_raw |>
        dplyr::mutate(
          impact_per_100k = (impact / population) * 1E5)
    }

    # Use args and impact to produce impact
    # input_table is not available (two branches: yll and yld) but not needed
    output <-
      healthiar:::get_output(
        input_args = input_args,
        results_raw = results_raw)

    return(output)

  }
