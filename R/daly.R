#' Attributable disability-adjusted life years
#'
# DESCRIPTION ##################################################################
#' @description
#' This function quantifies the attributable disability-adjusted life years (DALY)
#' by summing the two DALY components:
#' years of life lost (YLL) and years lived with disability (YLD).
#'
# ARGUMENTS ####################################################################
#' @param output_attribute_yll,output_attribute_yld \code{variable} containing YLL or YLD results of a \code{attribute_...()} function call, respectively.
#'
# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' This function sums the years of life lost (YLL) and years lived with disability (YLD)
#' to obtain the disability-adjusted lost years (DALY) \insertCite{GBD2020_tl}{healthiar}.
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#dalys}{dalys}}
#'
# VALUE ########################################################################
#' @returns
#' This function returns a \code{list} containing:
#'
#' 1) \code{health_main} (\code{tibble}) containing the main results;
#' \itemize{
#'  \item \code{impact} (\code{numeric} column) attributable health burden/impact in DALY
#'  \item \code{impact_yld} (\code{numeric} column) attributable health burden/impact in YLD
#'  \item \code{impact_yll} (\code{numeric} column) attributable health burden/impact in YLL
#'  \item \code{dw} (\code{numeric} column) disability weight used for YLD calculation
#'  \item And many more
#'  }
#'
#' 2) \code{health_detailed} (\code{list}) containing detailed (and interim) results.
#' \itemize{
#'  \item \code{results_raw} (\code{tibble}) containing results for each combination of input uncertainty
#'  \item \code{results_by_geo_id_micro} (\code{tibble}) containing results for each geographic unit under analysis (specified in \code{geo_id_micro} argument)
#'  \item \code{input_args} (\code{list}) containing all the argument inputs used in the background
#'  }

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
#'
#'
#' @seealso
#' \itemize{
#'   \item Upstream: \code{\link{attribute_health}}, \code{\link{attribute_lifetable}}
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

daly <-
  function(
    output_attribute_yll,
    output_attribute_yld){

    # Capture all arguments and values
    input_args <-
      get_input_args(
        environment = base::environment(),
        call = base::match.call())

    # Store results_raw of yll and yld
    # Shorter and handy to code
    results_raw_yll <- output_attribute_yll[["health_detailed"]][["results_by_geo_id_micro"]]
    results_raw_yld <- output_attribute_yld[["health_detailed"]][["results_by_geo_id_micro"]]

    # Capture all column names
    # They should be the same for yll and yld but just in case
    column_names_results_raw <-
      base::unique(c(names(results_raw_yll), names(results_raw_yld)))

    results_raw_yll[, c("sex", "age_group")] <- "total"
    results_raw_yld[, c("sex", "age_group")] <- "total"


    # Identify the columns names using keywords
    common_cols <-
      column_names_results_raw[base::grepl("exp|exposure|cutoff|geo|approach_risk|sex|age_group|bhd_ci",
                                     column_names_results_raw)]
    # Remove exceptions (columns with any of the keywords that should not be selected)
    common_cols <- common_cols[!base::grepl("approach_exposure|rr_at_exp", common_cols)]

    # The info columns can identify subgroups or exposure-outcome pairs, so the
    # years of life lost have to be joined to the years lived with disability
    # within the same subgroup. Otherwise every pair would be joined to every
    # other pair. intersect() because column_names_results_raw is the union of
    # both outputs, while a join column must be present in both of them
    info_cols_for_join <-
      base::intersect(
        base::grep("^info", base::names(results_raw_yll), value = TRUE),
        base::grep("^info", base::names(results_raw_yld), value = TRUE))

    cols_for_join <- c(common_cols, info_cols_for_join, "erf_ci")


    identical_cols <-
      check_if_args_identical(
        args_a = results_raw_yll,
        args_b = results_raw_yld,
        names_to_check = common_cols)

    # Remove those containing the word impact
    column_names_results_raw_without_impact <-
      column_names_results_raw[!base::grepl("impact|lifeyears|lifetable", column_names_results_raw)]


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
        impact_rounded = base::round(impact))

    # population is not one of the joining columns, so it carries the suffixes
    # _yll and _yld whenever both assessments provide it. It is the same
    # population in both (the years of life lost and the years lived with
    # disability of the same people), so it is restored here under its own name.
    # Without this, neither the rate below nor the one in get_output() is
    # calculated and the results tables have no impact rate at all
    if (base::all(c("population_yll", "population_yld") %in% base::names(results_raw))) {
      results_raw <- results_raw |>
        dplyr::mutate(population = dplyr::coalesce(population_yll, population_yld))
    }

    # Add impact per 100k inhabitants if population is available.
    # The name must be impact_per_100k_inhab as everywhere else in the package
    # (get_impact(), monetize(), standardize()): get_output() recognises the
    # rates by that suffix to keep them out of the sums and to re-calculate
    # them after aggregating. Named impact_per_100k, the column was added up
    # like an impact, i.e. the rates of different geo units were summed.
    # The assignment also overwrites the column of the same name that the join
    # carries over from the YLL branch, which holds the YLL and not the DALY rate
    if("population" %in% base::names(results_raw)){
      results_raw <-
        results_raw |>
        dplyr::mutate(
          impact_per_100k_inhab = (impact / population) * 1E5)
    }

    # Use args and impact to produce impact
    # input_table is not available (two branches: yll and yld) but not needed
    output <-
      get_output(
        input_args = input_args,
        results_raw = results_raw)

    return(output)

  }
