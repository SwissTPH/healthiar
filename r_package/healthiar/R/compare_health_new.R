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
  }


}
