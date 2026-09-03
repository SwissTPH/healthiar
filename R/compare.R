#' Compare the attributable health impacts between two scenarios

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the health impacts between two scenarios
#' (e.g. before and after a intervention in a health impact assessments) using either the delta or pif approach.

# ARGUMENTS ####################################################################
#' @param output_attribute_scen_1 Scenario 1 as in the output of attribute()
#' @param output_attribute_scen_2 Scenario 2 as in the output of attribute()
#' @param approach_comparison \code{String} showing the method of comparison. Options: "delta" or "pif".

# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#' This function compares the attributable health impacts in scenario 1 with scenario 2.
#' It can use two approaches:
#' \itemize{
#'  \item{Delta: Subtraction of health impacts in the two scenarios (two PAF)
#'    \insertCite{WHO2014_book}{healthiar}}
#'  \item{Potential impact fraction (PIF): Single PIF for both scenarios
#'    \insertCite{WHO2003_report}{healthiar}}}
#'
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#comparison-of-two-health-scenarios}{comparison of two health scenarios}}
#'
#'
#' \strong{Specifications of the comparison approach}
#'
#' Please, note that the PIF comparison approach assumes same baseline health data for scenario 1 and 2
#' (e.g. comparison of two scenarios at the same time point).
#' With the delta comparison approach, the difference between two scenarios is obtained by subtraction.
#' The delta approach is suited for all comparison cases,
#' allowing a comparison of a situation now with a situation in the future.
#'
#'
#' IMPORTANT: If your aim is to quantify health impacts from a policy intervention,
#' be aware that you should use the same year of analysis
#' and therefore same health baseline data
#' in both scenarios. The only variable that should change is the exposure
#' (as a result of the intervention).
#'
#'
#' \strong{Comparing DALY}
#'
#' If you want to use \code{compare()} DALY with \code{daly()},
#' do not enter the output of \code{daly()} in \code{compare()}.
#' Instead, follow these steps:
#'
#' 1) use \code{compare()} for YLL and YLD separately
#'
#' 2) use \code{daly()} inserting the output of both compare()
#'
#' Alternatively, you can use \code{attribute_health}
#' to quantify DALY entering DALY in the argument \code{bhd_central}
#' and then use \code{compare()}
#'
#'
#'
# VALUE ########################################################################
#' @returns
#' This function returns a \code{list} containing:
#' @returns
#' 1) \code{health_main} (\code{tibble}) containing the main results from the comparison;
#' \itemize{
#'  \item \code{impact} (\code{numeric} column) difference in attributable health burden/impact between scenario 1 and 2
#'  \item \code{impact_scen_1} (\code{numeric} column) attributable health impact of scenario 1
#'  \item \code{impact_scen_2} (\code{numeric} column) attributable health impact of scenario 2
#'  \item And many more
#'  }
#' @returns
#' 2) \code{health_detailed} (\code{list}) containing detailed (and interim) results from the comparison.
#' \itemize{
#'  \item \code{results_raw} (\code{tibble}) containing comparison results for each combination of input uncertainty for both scenario 1 and 2
#'  \item \code{results_by_geo_id_micro} (\code{tibble}) containing comparison results for each geographic unit under analysis (specified in \code{geo_id_micro} argument)
#'  \item \code{results_by_geo_id_macro} (\code{tibble}) containing comparison results for each aggregated geographic unit under analysis (specified in \code{geo_id_macro} argument))
#'  \item \code{input_table} (\code{list}) containing the inputs to each relevant argument for both scenario 1 and 2
#'  \item \code{input_args} (\code{list}) containing all the argument inputs for both scenario 1 and 2 used in the background
#'  \item \code{scen_1} (\code{tibble}) containing results for scenario 1
#'  \item \code{scen_2} (\code{tibble}) containing results for scenario 2
#'  }

# EXAMPLES #####################################################################
#' @examples
#' # Goal: comparison of two scenarios with delta approach
#' scenario_A <- attribute_health(
#'   exp_central = 8.85,   # EXPOSURE 1
#'   cutoff_central = 5,
#'   bhd_central = 25000,
#'   approach_risk = "relative_risk",
#'   erf_shape = "log_linear",
#'   rr_central = 1.118,
#'   rr_increment = 10
#' )
#' scenario_B <- attribute_health(
#'   exp_central = 6,     # EXPOSURE 2
#'   cutoff_central = 5,
#'   bhd_central = 25000,
#'   approach_risk = "relative_risk",
#'   erf_shape = "log_linear",
#'   rr_central = 1.118,
#'   rr_increment = 10
#' )
#' results <- compare(
#' approach_comparison = "delta",
#' output_attribute_scen_1 = scenario_A,
#' output_attribute_scen_2 = scenario_B
#' )
#' # Inspect the difference, stored in the \code{impact} column
#' results$health_main |>
#'   dplyr::select(impact, impact_scen_1, impact_scen_2) |>
#'   print()
#'
#' # Goal: comparison of two scenarios with potential impact fraction (pif) approach
#' output_attribute_scen_1 <- attribute_health(
#'   exp_central = 8.85,   # EXPOSURE 1
#'   cutoff_central = 5,
#'   bhd_central = 25000,
#'   approach_risk = "relative_risk",
#'   erf_shape = "log_linear",
#'   rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
#'   rr_increment = 10
#' )
#' output_attribute_scen_2 <- attribute_health(
#'   exp_central = 6,      # EXPOSURE 2
#'   cutoff_central = 5,
#'   bhd_central = 25000,
#'   approach_risk = "relative_risk",
#'   erf_shape = "log_linear",
#'   rr_central = 1.118, rr_lower = 1.060, rr_upper = 1.179,
#'   rr_increment = 10
#' )
#' results <- compare(
#'   output_attribute_scen_1 = output_attribute_scen_1,
#'   output_attribute_scen_2 = output_attribute_scen_2,
#'   approach_comparison = "pif"
#' )
#' # Inspect the difference, stored in the impact column
#' results$health_main$impact
#'
#'
#' @seealso
#' \itemize{
#'   \item Upstream: \code{\link{attribute_health}}, \code{\link{attribute_mod}},
#'     \code{\link{standardize}},
#'   \item Downstream: \code{\link{daly}}
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



compare <-
  function(
    output_attribute_scen_1,
    output_attribute_scen_2,
    approach_comparison = "delta"){

    # Extract input data (for subsequent get_impact call) ########################

    input_args_scen_1 <- output_attribute_scen_1[["health_detailed"]][["input_args"]]
    input_args_scen_2 <- output_attribute_scen_2[["health_detailed"]][["input_args"]]

    input_table_scen_1 <- output_attribute_scen_1[["health_detailed"]][["input_table"]]
    input_table_scen_2 <- output_attribute_scen_2[["health_detailed"]][["input_table"]]

    results_raw_scen_1 <- output_attribute_scen_1[["health_detailed"]][["results_raw"]]
    results_raw_scen_2 <- output_attribute_scen_2[["health_detailed"]][["results_raw"]]

    intermediate_calculations_scen_1 <- output_attribute_scen_1[["health_detailed"]][["intermediate_calculations"]]
    intermediate_calculations_scen_2 <- output_attribute_scen_2[["health_detailed"]][["intermediate_calculations"]]



    # Force the same environment in the functions of erf_eq.
    # Otherwise, not identified as identical and error joining below.
    if(!base::is.null(input_args_scen_1[["value"]][["erf_eq_central"]])){

      erf_eq_vars <- base::paste0("erf_eq", c("erf_eq", "_central", "_lower", "_upper"))

      input_args_scen_1[["value"]][erf_eq_vars] <-
        input_args_scen_2[["value"]][erf_eq_vars]

      input_table_scen_1[["erf_eq"]] <- input_table_scen_2[["erf_eq"]]

      }

    # Key variables #############################
    # Identify the arguments that have _scen_1 or _scen_2 in the name (scenario specific)
    # This is useful for joining data frames below
    scenario_specific_arguments <-
      c("exp_central", "exp_lower", "exp_upper",
        "bhd_central", "bhd_lower", "bhd_upper",
        "population",
        "prop_pop_exp",
        "pop_exp",
        "year_of_analysis",
        "info",
        "impact", "pop_fraction")

    # Only those for baseline health data (including for lifetable)
    scenario_arguments_for_bhd_and_lifetable <-
      c("bhd_central", "bhd_lower", "bhd_upper",
        "approach_exposure", "approach_newborns",
        "year_of_analysis")

    is_absolute_risk <-
      base::unique(input_table_scen_1$approach_risk) == "absolute_risk"

    is_lifetable <- base::unique(input_table_scen_1$is_lifetable)





    # Data validation ########################

    # Argument used (user entered data)
    passed_arguments_scen_1 <-
      base::names(purrr::keep(input_args_scen_1[["is_entered_by_user"]], ~ .x == TRUE))

    passed_arguments_scen_2 <-
      base::names(purrr::keep(input_args_scen_2[["is_entered_by_user"]], ~ .x == TRUE))


   # Check that the two scenarios used the same arguments (calculation pathways)

    if(!base::identical(passed_arguments_scen_1, passed_arguments_scen_2)){
      stop("The two scenarios must use the same arguments.",
           call. = FALSE)
    }


    # Arguments that should be identical in both scenarios
    common_arguments_scen_1 <-
      base::setdiff(passed_arguments_scen_1, scenario_specific_arguments)

    common_arguments_scen_2 <-
      base::setdiff(passed_arguments_scen_2, scenario_specific_arguments)



    if(base::identical(common_arguments_scen_1, common_arguments_scen_2)){
      common_arguments <- common_arguments_scen_1
    }else{
      stop("The two scenarios must use the same common arguments.",
           call. = FALSE)
    }

    common_arguments_identical <-
      check_if_args_identical(
        args_a = input_args_scen_1[["value"]],
        args_b = input_args_scen_2[["value"]],
        names_to_check = common_arguments)

    # Check that (relevant) input values from scenarios A & B are equal
    # Works also if no input was provided (might be the case for e.g. ..._lower arguments)
    # Check if the common arguments in both scenarios are identical

    if( ! base::all(common_arguments_identical) )
    {stop(
      base::paste0(
        base::paste(base::names(common_arguments_identical)[!common_arguments_identical],
                    collapse = ", "),
        " must be identical in both scenarios."),
      call. = FALSE)}

    # Check that bhd is the same in both scenarios for the PIF approach (only one place in the equation)



    if(approach_comparison == "pif"){

      error_if_var_is_not_identical <- function(var){
        if(var %in% c(base::names(input_table_scen_1),base::names(input_table_scen_2))  &&
           ! base::identical(input_table_scen_1[[var]], input_table_scen_2[[var]])){

          stop("For the PIF approach, ", var, " must be identical in both scenarios.",
               call. = FALSE)
        }
      }

      # Error if population and bhd are different in the scenarios
      # (only applicable for PIF)

      for(v in c("population", "bhd", "year_of_analysis")) {
        error_if_var_is_not_identical(var = v)
      }


      # PIF and absolute risk are not compatible
      if(is_absolute_risk){
        stop("For the PIF approach, the absolute risk approach cannot be used.",
             call. = FALSE)
      }
    }


    # Delta approach ########################

    if(approach_comparison == "delta"){


      # Identify the columns that are to be used to join results_raw_scen_1 and _scen_2
      joining_columns_output <-
        find_joining_columns(
          df_1 = results_raw_scen_1,
          df_2 = results_raw_scen_2,
          except = scenario_specific_arguments)

      # If different year of analysis
      # Add year as joining column
      # Otherwise too large table

      if( is_lifetable){
        if(! base::unique(results_raw_scen_1$year_of_analysis) ==
          base::unique(results_raw_scen_2$year_of_analysis)){

          joining_columns_output <- c(joining_columns_output, "year")
        }
      }



      # Merge the result tables by common columns
      results_raw <-
        dplyr::left_join(
         results_raw_scen_1,
         results_raw_scen_2,
          by = joining_columns_output,
          suffix = c("_scen_1", "_scen_2")) |>
        # Calculate the delta (difference) between scenario 1 and 2
        dplyr::mutate(impact = impact_scen_1 - impact_scen_2,
                      impact_rounded = base::round(impact, 0))

      input_table <-
        base::list(input_table_scen_1 = input_table_scen_1,
                   input_table_scen_2 = input_table_scen_2)

      intermediate_calculations <-
        base::list(intermediate_calculations_scen_1 = intermediate_calculations_scen_1,
                   intermediate_calculations_scen_2 = intermediate_calculations_scen_2)


      # PIF approach ########################


      # If the user choose "pif"  as comparison method
      # pif is additonally calculated
      # impact is overwritten with the new values that refer to pif instead of paf
      # Use if instead of else if becuase otherwise the package will read here inside
      # and produce an error because the variables are different
      }else if(approach_comparison == "pif"){

        # Get identical columns to join data frames (as above)
        joining_columns_input <-
          find_joining_columns(
            df_1 = input_table_scen_1,
            df_2 = input_table_scen_2,
            # except = scenario_specific_arguments)
            except = base::setdiff(
              scenario_specific_arguments,
              # Keep year_of_analysis in the table
              # so it can be accessed in the get_impact script
              c("year_of_analysis", "population"))
            )

        # Merge the input tables by common columns
        input_table_both_scen <-
          dplyr::left_join(
            input_table_scen_1,
            input_table_scen_2,
            by = joining_columns_input,
            suffix = c("_scen_1", "_scen_2"))

        results <-
          get_impact(
            input_table = input_table_both_scen,
            pop_fraction_type = "pif")

        # Collect results
        results_raw <- results[["results_raw"]]
        intermediate_calculations <- results[["intermediate_calculations"]]

        # Store the input table of each scenario separately (as in delta above)
        # and not the merged one, which was only needed for get_impact() above.
        # Keeping the same structure in both approaches enables the functions
        # downstream (e.g. summarize_uncertainty()) to identify a comparison
        # and to access the input data of each scenario in the same way
        input_table <-
          base::list(input_table_scen_1 = input_table_scen_1,
                     input_table_scen_2 = input_table_scen_2)

      }


    # Organize output ##############################

    # Classify the individual results of each scenario in delta and pif method
    # in a list

    output <-
      get_output(
        input_args = base::list(approach_comparison = approach_comparison,
                                input_args_scen_1 = input_args_scen_1,
                                input_args_scen_2 = input_args_scen_2),
        input_table = input_table,
        intermediate_calculations = intermediate_calculations,
        results_raw = results_raw)

    output[["health_detailed"]][["scen_1"]] <- results_raw_scen_1
    output[["health_detailed"]][["scen_2"]] <- results_raw_scen_2


    return(output)


  }
