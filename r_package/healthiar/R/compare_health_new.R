#' Compare attributable health cases based on single baseline health value and relative risk

#' @description Calculates the health impacts between two scenarios (e.g. before and after a intervention in a health impact assessments) using either the delta or pif approach.
#' @details
#' Note that several input parameters (such as baseline health data and relative risk must be the same to correctly compare the two scenarios.
#' @param output_attribute_scen_1 Scenario 1 as in the output of attribute()
#' @param output_attribute_scen_2 Scenario 2 as in the output of attribute()
#' @param approach_comparison \code{String} showing the method of comparison. Options: "delta" or "pif".
#' @return
#' TBD
#' @examples
#' TBD
#' @author Alberto Castro & Axel Luyten
#' @export

compare_health_new <-
  function(
    output_attribute_scen_1,
    output_attribute_scen_2,
    approach_comparison = "delta"){

    args_1 <- output_attribute_scen_1[["health_detailed"]][["args"]]
    args_2 <- output_attribute_scen_2[["health_detailed"]][["args"]]

    input_1 <- output_attribute_scen_1[["health_detailed"]][["input"]]
    input_2 <- output_attribute_scen_2[["health_detailed"]][["input"]]

    raw_1 <- output_attribute_scen_1[["health_detailed"]][["impact_raw"]]
    raw_2 <- output_attribute_scen_2[["health_detailed"]][["impact_raw"]]


    # Extract input data (for subsequent get_impact call) ########################


    # Key variables #############################
    # Identify the arguments that have _1 or _2 in the name (scenario specific)
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
    scenario_specific_arguments_for_bhd_and_lifetable <-
      c("bhd_central", "bhd_lower", "bhd_upper",
        "approach_exposure", "approach_newborns",
        "first_age_pop", "last_age_pop",
        "population_midyear_male", "population_midyear_female",
        "year_of_analysis")

    # Excluding the baseline health data and lifetable
    scenario_specific_arguments_wo_bhd_and_lifetable <-
      setdiff(scenario_specific_arguments,
              scenario_specific_arguments_for_bhd_and_lifetable)

    # Arguments that should be identical in both scenarios
    common_arguments_1 <-
      names(input_1)[!names(input_1) %in% scenario_specific_arguments]

    common_arguments_2 <-
      names(input_2)[!names(input_2) %in% scenario_specific_arguments]

    # Check that (relevant) input values from scenarios A & B are equal ##########
    ## Works also if no input was provided (might be the case for e.g. ..._lower arguments)

    #Check if the common arguments in both scenarios are identical
    if(identical(common_arguments_1, common_arguments_2)){
      common_arguments <- common_arguments_1
    }else{
      stop("The two scenarios have to use the same arguments")
    }

    # Create function to check that the common_arguments have the same values
    check_if_identical <- function(original, updated, names_to_check) {

      # Compare values
      checked_values <-
        purrr::map_lgl(names_to_check, ~ identical(original[[.x]], updated[[.x]])) |>
        setNames(names_to_check)  # Name the result

      return(checked_values)
    }

    identical_common_arguments <-
      check_if_identical(
        original = args_1,
        updated = args_2,
        names_to_check = common_arguments)



    if(!all(identical_common_arguments))
      {stop("The exposure-response function, cut-off and geo data must be identical in both scenarios")}

    # Delta approach ########################

    if(approach_comparison == "delta"){


      # Identify the columns that are to be used to join raw_1 and _2
      joining_columns_output <-
        healthiar:::find_joining_columns(
          df1 = raw_1,
          df2 = raw_2,
          except = scenario_specific_arguments)

      # Merge the result tables by common columns
      impact_raw <-
        dplyr::left_join(
         raw_1,
         raw_2,
          by = joining_columns_output,
          suffix = c("_1", "_2")) |>
        # Calculate the delta (difference) between scenario 1 and 2
        dplyr::mutate(impact = impact_1 - impact_2,
                      impact_rounded = round(impact, 0))

      input <- list(input_1 = input_1, input_2 = input_2)


      # PIF approach ########################


      # If the user choose "pif"  as comparison method
      # pif is additonally calculated
      # impact is overwritten with the new values that refer to pif instead of paf
      # Use if instead of else if becuase otherwise the package will read here inside
      # and produce an error because the variables are different
      }else if(approach_comparison == "pif"){


        # Identify the arguments scenario specific arguments excluding bhd
        # This will be used for the exceptions in the joining columns
        # Scenario-specific arguments cannot be used as joining columns
        # because we want to keep different columns for scenario_1 and _2
        # bhd and lifetable_with_pop_nest are excluded
        # because they have to be identical in scenario_1 and _2
        # for the pif approach by definition

        identical_scenario_specific_arguments_for_bhd_and_lifetable <-
          check_if_identical(
            original = args_1,
            updated = args_2,
            names_to_check = scenario_specific_arguments_for_bhd_and_lifetable)


        if(!all(identical_scenario_specific_arguments_for_bhd_and_lifetable))
        {stop("The baseline health data (including lifetable if applicable) must be identical in both scenarios")}



        # Delete bhd arguments because they are not used in the lifetable
        scenario_specific_arguments_lifetable <-
          setdiff(scenario_specific_arguments, c("bhd_central", "bhd_lower", "bhd_upper"))


        # Get identical columns to join data frames (as above)
        joining_columns_input <-
          healthiar:::find_joining_columns(
            df1 = input_1,
            df2 = input_2,
            except =  c(scenario_specific_arguments_lifetable,
                        ## Keep year_of_analysis in the table so it can be accessed in the get_impact script
                        "year_of_analysis"))

        # Merge the input tables by common columns
        input <-
          dplyr::left_join(
            input_1,
            input_2,
            by = joining_columns_input,
            suffix = c("_1", "_2"))


        ## Added if statement below to avoid error in the non-lifetable cases
        # args1 and args2 should have the same health_outcome (see checks above)
        # So let's use e.g. args1
        if(stringr::str_detect(args_1$health_outcome, "lifetable") ) {
          # Calculate the health impacts for each case (uncertainty, category, geo area...)
          impact_raw <-
            healthiar:::get_impact(
              input = input |> rename(year_of_analysis = year_of_analysis_1),
              pop_fraction_type = "pif")
        } else { # Non-lifetable cases
          impact_raw <-
            healthiar:::get_impact(
              input = input,
              pop_fraction_type = "pif")
        }

      }



    # Organize output
    # Classify the individual results of each scenario in delta and pif method
    # in a list

    output <-
      healthiar:::get_output(
        args = args,
        input = input,
        impact_raw = impact_raw)

    output[["health_detailed"]][["scenario_1"]] <- raw_1
    output[["health_detailed"]][["scenario_2"]] <- raw_1




    return(output)


  }
