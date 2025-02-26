#' Compare attributable health cases based on single baseline health value and relative risk

#' @description Calculates the health impacts between two scenarios (e.g. before and after a intervention in a health impact assessments) using either the delta or pif approach.
#' @details
#' Note that several input parameters (such as baseline health data and relative risk must be the same to correctly compare the two scenarios.
#' @param output_attribute_scen_A description
#' @param output_attribute_scen_B description
#' @return
#' TBD
#' @examples
#' TBD
#' @author Alberto Castro & Axel Luyten
#' @export

compare_health_new <- function(
    output_attribute_scen_A,
    output_attribute_scen_B){

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

}
