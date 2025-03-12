#' Compare attributable health cases based on single baseline health value and relative risk

#' @description Calculates the health impacts between two scenarios (e.g. before and after a intervention in a health impact assessments) using either the delta or pif approach.
#' @details
#' Note that several input parameters (such as baseline health data and relative risk must be the same to correctly compare the two scenarios.
#' @param output_attribute_1 Scenario 1 as in the output of attribute()
#' @param output_attribute_2 Scenario 2 as in the output of attribute()
#' @param approach_comparison \code{String} showing the method of comparison. Options: "delta" or "pif".
#' @return
#' TBD
#' @examples
#' TBD
#' @author Alberto Castro & Axel Luyten
#' @export

compare_health_new <-
  function(
    output_attribute_scen_A,
    output_attribute_scen_B,
    approach_comparison){

  browser()

  scen_a <- output_attribute_scen_A
  scen_b <- output_attribute_scen_B

  # Check that (relevant) input values from scenarios A & B are equal ##########
  ## Works also if no input was provided (might be the case for e.g. ..._lower arguments)

  ## Check number of geo units
  # TBA
  # Test the code with attribute output of an iteration


  ## rr
  if (
    identical(
      scen_a[["health_detailed"]][["raw"]] |> filter(erf_ci == "central") |> slice() |> pull(rr),
      scen_b[["health_detailed"]][["raw"]] |> filter(erf_ci == "central") |> slice() |> pull(rr)
    )  &
    identical(
      scen_a[["health_detailed"]][["raw"]] |> filter(erf_ci == "lower") |> slice() |> pull(rr),
      scen_b[["health_detailed"]][["raw"]] |> filter(erf_ci == "lower") |> slice() |> pull(rr)
    ) &
    identical(
      scen_a[["health_detailed"]][["raw"]] |> filter(erf_ci == "upper") |> slice() |> pull(rr),
      scen_b[["health_detailed"]][["raw"]] |> filter(erf_ci == "upper") |> slice() |> pull(rr)
    )
  ) {
    print("OK - RR input data of the scenarios match")
  } else {
    stop("RR input data of the two scenarios have to be identical")
  }

  ## erf_eq
  ### absolute risk formula


  ### function rr & ar
  # tricky, because the column erf_eq contains a strange string like "c("function (x, deriv = 0L) ", "{", "    deriv <-  [...] "

  ##

  ## rr from function (from point-pairs)

  ## rr increment
  if (
    identical(
      scen_a[["health_detailed"]][["raw"]] |> slice() |> pull(rr_increment),
      scen_b[["health_detailed"]][["raw"]] |> slice() |> pull(rr_increment)
    )  &
    identical(
      scen_a[["health_detailed"]][["raw"]] |> slice() |> pull(rr_increment),
      scen_b[["health_detailed"]][["raw"]] |> slice() |> pull(rr_increment)
    ) &
    identical(
      scen_a[["health_detailed"]][["raw"]] |> slice() |> pull(rr_increment),
      scen_b[["health_detailed"]][["raw"]] |> slice() |> pull(rr_increment)
    )
  ) {
    print("OK - RR increment input data of the scenarios match")
  } else {
    stop("RR increment input data of the two scenarios have to be identical")
  }

  ## cutoff
  if (
    identical(
      scen_a[["health_detailed"]][["raw"]] |> filter(cutoff_ci == "central") |> slice() |> pull(cutoff),
      scen_b[["health_detailed"]][["raw"]] |> filter(cutoff_ci == "central") |> slice() |> pull(cutoff)
    )  &
    identical(
      scen_a[["health_detailed"]][["raw"]] |> filter(cutoff_ci == "lower") |> slice() |> pull(cutoff),
      scen_b[["health_detailed"]][["raw"]] |> filter(cutoff_ci == "lower") |> slice() |> pull(cutoff)
    ) &
    identical(
      scen_a[["health_detailed"]][["raw"]] |> filter(cutoff_ci == "upper") |> slice() |> pull(cutoff),
      scen_b[["health_detailed"]][["raw"]] |> filter(cutoff_ci == "upper") |> slice() |> pull(cutoff)
    )
  ) {
    print("OK - cutoff input data of the scenarios match")
  } else {
    stop("cutoff input data of the two scenarios have to be identical")
  }


  # Extract input data (for subsequent get_impact call) ########################

  # Identify the arguments that have _1 or _2 in the name (scenario specific)
  # This is useful for joining data frames below
  scenario_specific_arguments <-
    c("exp_central", "exp_lower", "exp_upper",
      "bhd_central", "bhd_lower", "bhd_upper",
      "population",
      "prop_pop_exp",
      "pop_exp",
      "approach_exposure",
      "approach_newborns",
      "first_age_pop", "last_age_pop",
      "population_midyear_male", "population_midyear_female",
      "year_of_analysis",
      "info")

  #Add impact and pop_fraction
  scenario_specific_arguments <-
    c(scenario_specific_arguments,
      "impact", "pop_fraction")

  # Delta approach ########################

  if(approach_comparison == "delta"){


    # Identify the columns that are to be used to join impact_raw_1 and _2
    joining_columns_output <-
      healthiar:::find_joining_columns(
        df1 = output_attribute_1[["health_detailed"]][["raw"]],
        df2 = output_attribute_2[["health_detailed"]][["raw"]],
        except = scenario_specific_arguments)

    # Merge the result tables by common columns
    impact_raw <-
      dplyr::left_join(
        output_attribute_1[["health_detailed"]][["raw"]],
        output_attribute_2[["health_detailed"]][["raw"]],
        by = joining_columns_output,
        suffix = c("_1", "_2")) |>
      # Calculate the delta (difference) between scenario 1 and 2
      dplyr::mutate(impact = impact_1 - impact_2,
                    impact_rounded = round(impact, 0))


    # If the user choose "pif"  as comparison method
    # pif is additonally calculated
    # impact is overwritten with the new values that refer to pif instead of paf
    # Use if instead of else if becuase otherwise the package will read here inside
    # and produce an error because the variables are different
    }else if(approach_comparison == "pif"){

      # Either both NULL or identical. Use the function identical() to enable NULL==NULL
      ## bhd
      if (
        identical(
          scen_a[["health_detailed"]][["raw"]] |> filter(bhd_ci == "central") |> slice() |> pull(bhd),
          scen_b[["health_detailed"]][["raw"]] |> filter(bhd_ci == "central") |> slice() |> pull(bhd)
        )  &
        identical(
          scen_a[["health_detailed"]][["raw"]] |> filter(bhd_ci == "lower") |> slice() |> pull(bhd),
          scen_b[["health_detailed"]][["raw"]] |> filter(bhd_ci == "lower") |> slice() |> pull(bhd)
        ) &
        identical(
          scen_a[["health_detailed"]][["raw"]] |> filter(bhd_ci == "upper") |> slice() |> pull(bhd),
          scen_b[["health_detailed"]][["raw"]] |> filter(bhd_ci == "upper") |> slice() |> pull(bhd)
        )
      ) {
        print("OK - baseline health input data of the scenarios match")
      } else {
        stop("Baseline health input data of the two scenarios have to be identical")
      }

      # Error if length of fraction_of_year_lived > 1
      if (!identical(first_age_pop_1, first_age_pop_2) &
          !identical(last_age_pop_1, last_age_pop_2) &
          !identical(population_midyear_male_1, population_midyear_male_2) &
          !identical(population_midyear_female_1, population_midyear_female_2) &
          !identical(year_of_analysis_1, year_of_analysis_2)){
        stop("Age interval, probability of dying and population in the scenario 1 and 2 have to be identical")
      }


      # Compile input data of scenario 1
      input_1 <-
        healthiar:::compile_input(
          health_outcome = health_outcome,
          approach_risk = approach_risk,
          exp_central = exp_central_1, exp_lower = exp_lower_1, exp_upper = exp_upper_1,
          prop_pop_exp = prop_pop_exp_1,
          cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
          bhd_central = bhd_central_1, bhd_lower = bhd_lower_1, bhd_upper = bhd_upper_1,
          rr_central = rr_central,
          rr_lower = rr_lower,
          rr_upper = rr_upper,
          rr_increment = rr_increment,
          erf_shape = erf_shape,
          erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
          min_age = min_age,
          max_age = max_age,
          info = info_1,
          geo_id_disaggregated = geo_id_disaggregated,
          geo_id_aggregated = geo_id_aggregated,
          population = population_1,
          # YLD
          dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
          duration_central = duration_central, duration_lower = duration_lower, duration_upper = duration_upper,
          # Lifetable data
          approach_exposure = approach_exposure_1,
          approach_newborns = approach_newborns_1,
          year_of_analysis =  year_of_analysis_1,
          first_age_pop =  first_age_pop_1,
          last_age_pop = last_age_pop_1,
          deaths_male = deaths_male_1,
          deaths_female = deaths_female_1,
          population_midyear_male = population_midyear_male_1,
          population_midyear_female =  population_midyear_female_1)

      # Compile input data of scenario 2
      input_2 <-
        healthiar:::compile_input(
          health_outcome = health_outcome,
          approach_risk = approach_risk,
          exp_central = exp_central_2, exp_lower = exp_lower_2, exp_upper = exp_upper_2,
          prop_pop_exp = prop_pop_exp_2,
          cutoff_central = cutoff_central, cutoff_lower = cutoff_lower, cutoff_upper = cutoff_upper,
          bhd_central = bhd_central_2, bhd_lower = bhd_lower_2, bhd_upper = bhd_upper_2,
          rr_central = rr_central,
          rr_lower = rr_lower,
          rr_upper = rr_upper,
          rr_increment = rr_increment,
          erf_shape = erf_shape,
          erf_eq_central = erf_eq_central, erf_eq_lower = erf_eq_lower, erf_eq_upper = erf_eq_upper,
          min_age = min_age,
          max_age = max_age,
          info = info_2,
          geo_id_disaggregated = geo_id_disaggregated,
          geo_id_aggregated = geo_id_aggregated,
          population = population_2,
          # YLD
          dw_central = dw_central, dw_lower = dw_lower, dw_upper = dw_upper,
          duration_central = duration_central, duration_lower = duration_lower, duration_upper = duration_upper,
          # Lifetable data
          approach_exposure = approach_exposure_2,
          approach_newborns = approach_newborns_2,
          year_of_analysis = year_of_analysis_2,
          first_age_pop =  first_age_pop_2,
          last_age_pop = last_age_pop_2,
          deaths_male = deaths_male_2,
          deaths_female = deaths_female_2,
          population_midyear_male = population_midyear_male_2,
          population_midyear_female =  population_midyear_female_2)

      # Identify the arguments scenario specific arguments excluding bhd
      # This will be used for the exceptions in the joining columns
      # Scenario-specific arguments cannot be used as joining columns
      # because we want to keep different columns for scenario_1 and _2
      # bhd and lifetable_with_pop_nest are excluded
      # because they have to be identical in scenario_1 and _2
      # for the pif approach by definition
      scenario_specific_arguments_excluding_bhd <-
        setdiff(scenario_specific_arguments, c("bhd_central", "bhd_lower", "bhd_upper"))


      # Get identical columns to join data frames (as above)
      joining_columns_input <-
        healthiar:::find_joining_columns(
          df1 = input_1,
          df2 = input_2,
          except =  c(scenario_specific_arguments_excluding_bhd,
                      ## Keep year_of_analysis in the table so it can be accessed in the get_impact script
                      "year_of_analysis"))

      # Merge the input tables by common columns
      input <-
        dplyr::left_join(
          input_1,
          input_2,
          by = joining_columns_input,
          suffix = c("_1", "_2"))

      # browser()

      ## Added if statement below to avoid error in the non-lifetable cases
      if( stringr::str_detect(health_outcome, "lifetable") ) {
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
    healthiar:::get_output(impact_raw = impact_raw)

  output[["health_detailed"]][["scenario_1"]] <- impact_raw_1
  output[["health_detailed"]][["scenario_2"]] <- impact_raw_1




  return(output)


}
