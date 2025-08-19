#' Compare the attributable health impacts between two scenarios

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the health impacts between two scenarios (e.g. before and after a intervention in a health impact assessments) using either the delta or pif approach.

# ARGUMENTS ####################################################################
#' @param output_attribute_scen_1 Scenario 1 as in the output of attribute()
#' @param output_attribute_scen_2 Scenario 2 as in the output of attribute()
#' @param approach_comparison \code{String} showing the method of comparison. Options: "delta" or "pif".

# DETAILS ######################################################################
#' @details
#' Note that the PIF comparison approach assumes same baseline health data for scenario 1 and 2 (e.g. comparison of two scenarios at the same time).
#' @details
#' Equations population impact fraction (PIF)
#' @details The Population Impact Fraction (PIF) is defined as the proportional change in disease or mortality when exposure to a risk factor is changed (for instance due to an intervention). The most general equation describing this mathematically is an integral form:
#' \deqn{PIF = \frac{\int RR(x)PE(x)dx - \int RR(x)PE'(x)dx}{\int RR(x)PE(x)dx}}
#' @details Where:
#' @details x     = exposure level
#' @details PE(x) = population distribution of exposure
#' @details PE'(x) = alternative population distribution of exposure
#' @details RR(x) = relative risk at exposure level compared to the reference level
#' @details
#' If the population exposure is described as a categorical rather than continuous exposure, the integrals in equation (5) may be converted to sums, resulting in the following equations for the PIF:
#' \deqn{PIF = \frac{\sum RR_{i} \times PE_{i} - \sum RR_{i}PE'_{i}}{\sum RR_{i}PE_{i}}}
#' @details Where:
#' @details i     = is the exposure category (e.g. in bins of 1 \eqn{µg/m^3} PM2.5 or 5 dB noise exposure)
#' @details \eqn{PE_i} = fraction of population in exposure category i
#' @details \eqn{PE'_i} = fraction of population in category i for alternative (ideal) exposure scenario
#' @details \eqn{RR_i} = relative risk for exposure category level i compared to the reference level
#' @details
#' Finally, if the exposure is provided as the population weighted mean concentration (PWC), the equation for the PIF is reduced to:
#' \deqn{PIF = \frac{RR_{PWC} - RR_{alt PWC}}{RR_{PWC}}}
#' @details Where:
#' @details \eqn{RR_{PWC}} = relative risk associated with the population weighted mean exposure
#' @details \eqn{RR_{PWC}} = relative risk associated with the population weighted mean for the alternative exposure scenario
#' @details
#' Delta comparison approach
#' @details
#' With the delta comparison the difference between two scenarios is obtained by substraction. The delta approach is suited for all comparison cases, and specifically for comparison of a situation now with a situation in the future.

# VALUE ########################################################################
#' @inherit attribute_master return

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
#' # Goal: comparison of two scenarios with population impact fraction (pif) approach
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

#' @author Alberto Castro & Axel Luyten

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

    interim_results_scen_1 <- output_attribute_scen_1[["health_detailed"]][["interim_results"]]
    interim_results_scen_2 <- output_attribute_scen_2[["health_detailed"]][["interim_results"]]



    # Force the same environment in the functions of erf_eq.
    # Otherwise, not identified as identical and error joining below.
    if(!is.null(input_args_scen_1$value$erf_eq_central)){
      erf_eq_vars <- paste0("erf_eq_", c("central", "lower", "upper"))

      input_args_scen_1$value$erf_eq_central <- input_args_scen_2$value$erf_eq_central
      input_args_scen_1$value$erf_eq_lower <- input_args_scen_2$value$erf_eq_lower
      input_args_scen_1$value$erf_eq_upper <- input_args_scen_2$value$erf_eq_upper
      input_table_scen_2$erf_eq <- input_table_scen_1$erf_eq    }

    # Key variables #############################
    # Identify the arguments that have _scen_1 or _scen_2 in the name (scenario specific)
    # This is useful for joining data frames below
    scenario_specific_arguments <-
      c("exp_central", "exp_lower", "exp_upper",
        "bhd_central", "bhd_lower", "bhd_upper",
        "population",
        "prop_pop_exp",
        "pop_exp",
        "population_midyear_male", "population_midyear_female",
        "year_of_analysis",
        "info",
        "impact", "pop_fraction")

    # Only those for baseline health data (including for lifetable)
    scenario_arguments_for_bhd_and_lifetable <-
      c("bhd_central", "bhd_lower", "bhd_upper",
        "approach_exposure", "approach_newborns",
        "year_of_analysis")


    # Data validation ########################

    # Argument used (user entered data)
    passed_arguments_scen_1 <-
      base::names(purrr::keep(input_args_scen_1$is_entered_by_user, ~ .x == TRUE))

    passed_arguments_scen_2 <-
      base::names(purrr::keep(input_args_scen_2$is_entered_by_user, ~ .x == TRUE))


   # Check that the two scenarios used the same arguments (calculation pathways)

    if(!base::identical(passed_arguments_scen_1, passed_arguments_scen_2)){
      stop("The two scenarios must use the same arguments.",
           call. = FALSE)
    }


    # Arguments that should be identical in both scenarios
    common_arguments_scen_1 <-
      passed_arguments_scen_1[!passed_arguments_scen_1 %in% scenario_specific_arguments]

    common_arguments_scen_2 <-
      passed_arguments_scen_2[!passed_arguments_scen_2 %in% scenario_specific_arguments]



    if(base::identical(common_arguments_scen_1, common_arguments_scen_2)){
      common_arguments <- common_arguments_scen_1
    }else{
      stop("The two scenarios must use the same common arguments.",
           call. = FALSE)
    }

    common_arguments_identical <-
      healthiar:::check_if_args_identical(
        args_a = input_args_scen_1$value,
        args_b = input_args_scen_2$value,
        names_to_check = common_arguments)

    # Check that (relevant) input values from scenarios A & B are equal
    # Works also if no input was provided (might be the case for e.g. ..._lower arguments)
    # Check if the common arguments in both scenarios are identical

    if(!all(common_arguments_identical))
    {stop(
      base::paste0(
        base::paste(names(common_arguments_identical)[!common_arguments_identical],
                    collapse = ", "),
        " must be identical in both scenarios."),
      call. = FALSE)}

    # Check that bhd is the same in both scenarios for the PIF approach (only one place in the equation)

    if(approach_comparison == "pif" &&
       "bhd" %in% c(names(input_table_scen_1),names(input_table_scen_2))  &&
       !base::identical(input_table_scen_1$bhd, input_table_scen_2$bhd)){
      stop("For the PIF approach, bhd must be identical in both scenarios.",
           call. = FALSE)
    }

    if(approach_comparison == "pif" &&
       "population" %in% c(names(input_table_scen_1),names(input_table_scen_2))  &&
       !base::identical(input_table_scen_1$population, input_table_scen_2$population)){
      stop("For the PIF approach, population must be identical in both scenarios.",
           call. = FALSE)
    }
    # Check if absolute risk with pif (not possible)

    if(approach_comparison == "pif" &&
       base::unique(input_table_scen_1$approach_risk) == "absolute_risk"){
      stop("For the PIF approach, the absolute risk approach cannot be used.",
           call. = FALSE)
    }

    # Delta approach ########################

    if(approach_comparison == "delta"){


      # Identify the columns that are to be used to join results_raw_scen_1 and _scen_2
      joining_columns_output <-
        healthiar:::find_joining_columns(
          df_1 = results_raw_scen_1,
          df_2 = results_raw_scen_2,
          except = scenario_specific_arguments)

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

      interim_results <-
        base::list(interim_results_scen_1 = interim_results_scen_1,
                   interim_results_scen_2 = interim_results_scen_2)


      # PIF approach ########################


      # If the user choose "pif"  as comparison method
      # pif is additonally calculated
      # impact is overwritten with the new values that refer to pif instead of paf
      # Use if instead of else if becuase otherwise the package will read here inside
      # and produce an error because the variables are different
      }else if(approach_comparison == "pif"){

        # Get identical columns to join data frames (as above)
        joining_columns_input <-
          healthiar:::find_joining_columns(
            df_1 = input_table_scen_1,
            df_2 = input_table_scen_2,
            except =  c(scenario_specific_arguments,
                        ## Keep year_of_analysis in the table so it can be accessed in the get_impact script
                        "year_of_analysis"))

        # Merge the input tables by common columns
        input_table <-
          dplyr::left_join(
            input_table_scen_1,
            input_table_scen_2,
            by = joining_columns_input,
            suffix = c("_scen_1", "_scen_2"))


        ## Added if statement below to avoid error in the non-lifetable cases
        # input_args_scen_1 and input_args_scen_2 should have the same health_outcome (see checks above)
        # So let's use e.g. input_args_scen_1
        if(base::unique(input_table_scen_1$is_lifetable)) {
          # Calculate the health impacts for each case (uncertainty, category, geo area...)
          results <-
            input_table |>
            # Duplicate column with new names (without _scen_..)
            # to enable that get_impact() can read it
            dplyr::mutate(year_of_analysis = year_of_analysis_scen_1,
                          population = population_scen_1) |>
            dplyr::select(-population_scen_1, -population_scen_2)|>
            healthiar:::get_impact(input_table = _,
                                   pop_fraction_type = "pif")
        } else { # Non-lifetable cases

          results <-
            healthiar:::get_impact(
              input_table = input_table,
              pop_fraction_type = "pif")
        }

        results_raw <- results$results_raw
        interim_results <- results$interim_results

      }



    # Organize output
    # Classify the individual results of each scenario in delta and pif method
    # in a list

    output <-
      healthiar:::get_output(
        input_args = base::list(approach_comparison = approach_comparison,
                                input_args_scen_1 = input_args_scen_1,
                                input_args_scen_2 = input_args_scen_2),
        input_table = input_table,
        interim_results = interim_results,
        results_raw = results_raw)

    output[["health_detailed"]][["scen_1"]] <- results_raw_scen_1
    output[["health_detailed"]][["scen_2"]] <- results_raw_scen_1




    return(output)


  }
