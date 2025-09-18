#' Attributable disability-adjusted life years

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the disability-adjusted life years (DALY) attributable to the exposure to an environmental stressor by adding the two DALY components YLL and YLD.

# ARGUMENTS ####################################################################
#' @param output_attribute_yll,output_attribute_yld \code{variable} containing YLL or YLD results of a \code{attribute_...()} function call, respectively.

# VALUE ########################################################################
#' @inherit attribute_master return

# EXAMPLES #####################################################################
#' @examples
#' # Goal: obtain DALY (disability-adjusted life years) from two existing \code{attribute_...} outputs
#' # Step 1: Create YLL (years of life lost) assessment
#' results_yll <- attribute_lifetable(
#'   health_outcome = "yll",
#'   approach_exposure = "single_year",
#'   approach_newborns = "without_newborns",
#'   exp_central = 8.85,
#'   prop_pop_exp = 1,
#'   cutoff_central = 5,
#'   rr_central =  1.118,
#'   rr_increment = 10,
#'   erf_shape = "log_linear",
#'   age_group = exdat_lifetable$age_group,
#'   sex = exdat_lifetable$sex,
#'   bhd_central = exdat_lifetable$deaths,
#'   population = exdat_lifetable$midyear_population,
#'   year_of_analysis = 2019,
#'   min_age = 20
#' )
#' # Step 2: Create YLD (years lived with disability) assessment
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
#' # Step 3: obtain DALY
#' results <- daly(
#'   output_attribute_yll = results_yll,
#'   output_attribute_yld = results_yld
#' )
#' # Attributable impact in DALY
#' results$health_main |>
#'   dplyr::select(impact, impact_yll, impact_yld)

#' @author Alberto Castro & Axel Luyten

#' @export

daly <-
  function(
    output_attribute_yll,
    output_attribute_yld){

    # Capture all arguments and values
    input_args <-
      get_input_args(environment = base::environment(),
                                 call = match.call())

    # Store results_raw of yll and yld
    # Shorter and handy to code
    results_raw_yll <- output_attribute_yll[["health_detailed"]][["results_by_geo_id_micro"]]
    results_raw_yld <- output_attribute_yld[["health_detailed"]][["results_by_geo_id_micro"]]

    # Capture all column names
    # They should be the same for yll and yld but just in case
    column_names_results_raw <-
      unique(c(names(results_raw_yll), names(results_raw_yld)))

    results_raw_yll[, c("sex", "age_group")] <- "total"
    results_raw_yld[, c("sex", "age_group")] <- "total"


    # Identify the columns names using keywords
    common_cols <-
      column_names_results_raw[grepl("exp|exposure|cutoff|geo|approach_risk|sex|age_group|bhd_ci",
                                     column_names_results_raw)]
    # Remove exceptions (columns with any of the keywords that should not be selected)
    common_cols <- common_cols[!grepl("approach_exposure|rr_at_exp", common_cols)]
    cols_for_join <- c(common_cols, "erf_ci")


    identical_cols <-
      check_if_args_identical(
        args_a = input_args$value$output_attribute_yld,
        args_b = input_args$value$output_attribute_yld,
        names_to_check = common_cols)

    # Remove those containing the word impact
    column_names_results_raw_without_impact <-
      column_names_results_raw[!grepl("impact|lifeyears|lifetable", column_names_results_raw)]


    if(!all(identical_cols))
    {stop("The arguments ",
          base::toString(base::names(identical_cols)[identical_cols]),
          " must be identical in both scenarios")}


    # Obtain the new results_raw for DALY
    results_raw <-
      # Join results_raw tables from yll and yld
      # but giving a suffix _yll and _yld to free the name "impact" to YLD
      # We need to use "impact" as final result to be consistent with the other
      # healthiar functions
      dplyr::full_join(
        results_raw_yll,
        results_raw_yld,
        by = cols_for_join,
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
      get_output(
        input_args = input_args,
        results_raw = results_raw)

    return(output)

  }
