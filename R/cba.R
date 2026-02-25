#' Cost-benefit analysis

# DESCRIPTION ##################################################################
#' @description
#' This function performs a cost-benefit analysis.
#' It is assumed that the benefit is caused by the positive health impacts of a policy intervention,
#' which generates some costs.

# ARGUMENTS ####################################################################
#' @inheritParams monetize
#' @param discount_rate_benefit,discount_rate_cost \code{Numeric value} referring to the the discount rate used in the benefit and the cost side (respectively). Their values determine the approach of cost-benefit analysis: direct approach (if the same discount_rate is used for cost and benefit) and indirect approach (different discount rates).
#' @param n_years_benefit,n_years_cost \code{Numeric value} referring to number of years in the future to be considered in the benefit and cost side (respectively). Years for discounting and/or inflation. Be aware that the year 0 (without discounting/inflation, i.e. the present) is not be counted here. If a vector is entered in the argument impact, n_years does not need to be entered (length of impact = n_years + 1)
#' @param impact_benefit \code{Numeric value} referring to the positive health impact as result of a reduction of harmful exposure.
#' @param cost \code{Numeric value} referring to the investment cost to achieve the reduction of exposure.

# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' This function provides as output three types of cost-benefit indicators
#' \insertCite{Boardman2018_book}{healthiar}:
#'
#' \itemize{
#'  \item net benefit
#'  \item cost-benefit ratio
#'  \item return of investment}
#'
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#cost-benefit-analysis}{cost-benefit analysis}
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#monetization}{monetization}}
#'
# VALUE ########################################################################
#' @returns
#' This function returns a \code{list} containing:
#'
#' 1) \code{cba_main} (\code{tibble}) containing the main CBA results;
#' \itemize{
#'  \item \code{net_benefit} (\code{numeric} column) containing the difference between benefit and cost (i.e. benefit - cost)
#'  \item \code{benefit} (\code{numeric} column) containing discounted benefit (i.e. monetized attributable health impact)
#'  \item \code{cost} (\code{numeric} column) containing discounted cost
#'  \item And many more
#' }
#'
#' 2) \code{cba_detailed} (\code{list}) containing detailed (and interim) results.
#' \itemize{
#'  \item \code{benefit} (\code{list})
#'  \item \code{cost} (\code{tibble})
#' }
#'
#' If the argument \code{output_attribute} was specified, then the two results elements are added to the existing output.

# EXAMPLES #####################################################################
#' @examples
#' # Goal: performs a cost-benefit analysis using an existing output
#' # of a attribute_... function
#'
#' output_attribute <- attribute_health(
#'   erf_shape = "log_linear",
#'   rr_central = 1.369,
#'   rr_increment = 10,
#'   exp_central = 8.85,
#'   cutoff_central = 5,
#'   bhd_central = 30747
#' )
#'
#' results <- cba(
#'   output_attribute = output_attribute,
#'   valuation = 50000,
#'   cost = 100000000,
#'   discount_shape = "exponential",
#'   discount_rate_benefit = 0.03,
#'   discount_rate_cost = 0.03,
#'   n_years_benefit = 5,
#'   n_years_cost = 5
#' )
#'
#' results$cba_main |>
#'   dplyr::select(benefit, cost, net_benefit)
#'
#'
#' @seealso
#' \itemize{
#'   \item Upstream: \code{\link{attribute_health}}
#'   \item Alternative: \code{\link{monetize}}
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



cba <-
  function(output_attribute = NULL,
           impact_benefit = NULL,
           valuation,
           cost,
           discount_rate_benefit = NULL,
           discount_rate_cost = NULL,
           inflation_rate = NULL,
           discount_shape = "exponential",
           n_years_benefit = 1,
           n_years_cost = 0,
           discount_is_nominal = FALSE) {

    # Define vectors that are relevant below

    columns_monetization <-
        c("monetized_impact", "monetized_impact_rounded")

    suffix_monetization <-
      c("_benefit", "_cost")

    columns_monetization_with_suffix <-
      base::paste0(
        columns_monetization,
        base::rep(suffix_monetization, each = base::length(columns_monetization))
      )

    # Run include_monetization for benefit and cost separately
    # Important to obtain main and detailed to avoid losing information

    cba_benefit <- monetize(
      output_attribute = output_attribute,
      impact = impact_benefit,
      discount_rate = discount_rate_benefit,
      discount_shape = discount_shape,
      inflation_rate = inflation_rate,
      n_years = n_years_benefit,
      valuation = valuation,
      discount_is_nominal = discount_is_nominal)


    cba_main_benefit <- cba_benefit[["monetization_main"]]
    cba_detailed_benefit <- cba_benefit[["monetization_detailed"]]





    # For cost, assume 1 impact with full valuation
    cba_cost <-
      monetize(
        impact = 1,
        valuation = cost,
        discount_rate = discount_rate_cost,
        discount_shape = discount_shape,
        inflation_rate = inflation_rate,
        n_years = n_years_cost,
        discount_is_nominal = discount_is_nominal)

    # For costs main and detailed are the same because they only have one row
    cba_main_cost <- cba_cost[["monetization_main"]]
    cba_detailed_cost <- cba_cost[["monetization_detailed"]]

    # Build the detailed output list
    cba_detailed <-
      base::list(
        benefit = cba_detailed_benefit,
        cost = cba_detailed_cost)

    # Get main output
    cba_main <-
      # Join benefit and cost into one df
      dplyr::left_join(
        cba_main_benefit,
        cba_main_cost,
        by = c("discount_shape"),
        suffix = suffix_monetization)


    # Store names of columns with ci and geo_id
    # These columns define the different cases (rows)
    # This intermediate step is needed to ensure that no errors are produced
    # if no columns with ci or geo are available
    # (i.e, without using the function attribute in a previous step)
    columns_ci_geo <-
      base::names(cba_main)[base::grepl("_ci|geo_id", base::names(cba_main))]

    relevant_columns <-
      c(columns_ci_geo,
        columns_monetization_with_suffix,
        "discount_shape")

    cba_main <-
      cba_main |>
      # Keep only relevant columns
      dplyr::select(dplyr::all_of(relevant_columns))|>
      # Moreover, cost is not actually a monetized impact
      dplyr::rename("benefit" = "monetized_impact_benefit",
                    "cost" = "monetized_impact_cost",
                    "benefit_rounded" = "monetized_impact_rounded_benefit",
                    "cost_rounded" = "monetized_impact_rounded_cost") |>
      # Calculate the difference between benefit and cost (net_benefit)
      # as well as cbr (cost-benefit ratio) and roi (return of investment)
      dplyr::mutate(net_benefit = benefit - cost,
                    net_benefit_rounded = base::round(net_benefit),
                    cbr = benefit / cost,
                    roi = (benefit - cost) / cost * 100)




    # Build the output list with main and detailed

    output_cba <-
      base::list(cba_main = cba_main,
           cba_detailed = cba_detailed)



    if(base::is.null(impact_benefit) & !base::is.null(output_attribute)){
      output <-
        c(output_attribute,
          output_cba)

    }else if(!base::is.null(impact_benefit) & base::is.null(output_attribute)){
     output <- output_cba
    }



    return(output)



}
