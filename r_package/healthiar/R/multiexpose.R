#' Aggregate health impacts from multiple exposures

#' @description
#' This function aggregates health impacts from multiple exposures to environmental stressors.
#' @param output_attribute_1,output_attribute_2  Output of attribute() for exposure 1 and 2 resp.Baseline health data and population cannot be different in exposure 1 and 2.
#' @param exposure_name_1,exposure_name_2 \code{String} referring to the name of the environmental exposures 1 and 2
#' @param approach \code{String} specifying the multiple exposures approach to be used in the assessment. Options: "additive" (default), "multiplicative" or "combined".
#' @author Alberto Castro & Axel Luyten
#' @note Experimental function
#' @export

multiexpose <-
  function(
    output_attribute_1,
    output_attribute_2,
    exposure_name_1,
    exposure_name_2,
    approach = "additive"){

    # Capture all input arguments and tables
    input_args <- as.list(environment())
    pop_fraction_type <- input_args$pop_fraction_type

    input_args_1 <- output_attribute_1[["health_detailed"]][["input_args"]]
    input_args_2 <- output_attribute_2[["health_detailed"]][["input_args"]]

    input_table_1 <- output_attribute_1[["health_detailed"]][["input_table"]]
    input_table_2 <- output_attribute_2[["health_detailed"]][["input_table"]]


    # Add the exposure names to the input_table
    input_table_1_for_binding <-
      input_table_1 |>
      dplyr::mutate(exposure_name = exposure_name_1)

    input_table_2_for_binding <-
      input_table_2 |>
      dplyr::mutate(exposure_name = exposure_name_2)

    #Bind the tables together
    input_table <-
      dplyr::bind_rows(
        input_table_1_for_binding,
        input_table_2_for_binding) |>
    # Add the approach
      dplyr::mutate(
        approach_multiexposure = approach)

      # Calculate the health impacts for each case (uncertainty, category, geo area...)
      impact_raw <-
        healthiar:::get_impact(input_table = input_table,
                               pop_fraction_type = "paf")

      # Get the main and detailed output by aggregating and/or filtering cases (rows)
      output <-
        healthiar:::get_output(input_args = input_args,
                               input_table = input_table,
                               impact_raw = impact_raw)




    return(output)

  }
