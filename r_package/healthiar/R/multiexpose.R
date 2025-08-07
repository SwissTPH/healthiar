#' Aggregate health impacts from multiple exposures

# DESCRIPTION ##################################################################
#' @description
#' This function aggregates health impacts from multiple exposures to environmental stressors.

# ARGUMENTS ####################################################################
#' @param output_attribute_1,output_attribute_2  Output of attribute() for exposure 1 and 2, respectively. Baseline health data and population must be identical in outputs 1 and 2.
#' @param exp_name_1,exp_name_2 \code{String} referring to the name of the environmental exposures 1 and 2
#' @param approach_multiexposure \code{String} specifying the multiple exposures approach to be used in the assessment. Options: "additive" (default), "multiplicative" or "combined".

# VALUE ########################################################################
#' @inherit attribute_master return

# EXAMPLES #####################################################################
#' @examples
#' # Goal: determine aggregated health impacts from multiple exposures
#' # Step 1: create assessment with exposure 1
#' output_attribute_1 <- attribute_health(
#'   erf_shape = "log_linear",
#'   rr_central = 1.369,
#'   rr_increment = 10,
#'   exp_central = 8.85,
#'   cutoff_central = 5,
#'   bhd_central = 30747
#' )
#' output_attribute_1$health_main$impact
#' # Step 2: create assessment with exposure 2
#' output_attribute_2 <- attribute_mod(
#'   output_attribute = output_attribute_1,
#'   exp_central = 10.9,
#'   rr_central = 1.031
#' )
#' output_attribute_2$health_main$impact
#' # Step 3: aggregate impacts of the two assessments
#' results <- multiexpose(
#'   output_attribute_1 = output_attribute_1,
#'   output_attribute_2 = output_attribute_2,
#'   exp_name_1 = "pm2.5",
#'   exp_name_2 = "no2",
#'   approach_multiexposure = "multiplicative"
#' )
#' results$health_main$impact

#' @author Alberto Castro & Axel Luyten

#' @export



multiexpose <-
  function(
    output_attribute_1,
    output_attribute_2,
    exp_name_1,
    exp_name_2,
    approach_multiexposure = "additive"){

    # Capture all arguments and values
    input_args <-
      healthiar:::get_input_args(environment = base::environment(),
                                 call = match.call())

    pop_fraction_type <- input_args$value$pop_fraction_type

    input_table_1 <- output_attribute_1[["health_detailed"]][["input_table"]]
    input_table_2 <- output_attribute_2[["health_detailed"]][["input_table"]]


    # Add the exposure names to the input_table
    input_table_1_for_binding <-
      input_table_1 |>
      dplyr::mutate(exp_name = exp_name_1)

    input_table_2_for_binding <-
      input_table_2 |>
      dplyr::mutate(exp_name = exp_name_2)

    #Bind the tables together
    input_table <-
      dplyr::bind_rows(
        input_table_1_for_binding,
        input_table_2_for_binding) |>
    # Add the approach
      dplyr::mutate(
        approach_multiexposure = approach_multiexposure)

      # Calculate the health impacts for each case (uncertainty, category, geo area...)
      results_raw <-
        healthiar:::get_impact(input_table = input_table,
                               pop_fraction_type = "paf")

      # Get the main and detailed output by aggregating and/or filtering cases (rows)
      output <-
        healthiar:::get_output(input_args = input_args,
                               input_table = input_table,
                               results_raw = results_raw)

      # Put the column exp_name as first column because it is now relevant
      output[["health_detailed"]][c("input_table", "results_raw")] <-
        purrr::map(output[["health_detailed"]][c("input_table", "results_raw")],
                   ~ dplyr::select(.x,
                                   exp_name, dplyr::everything()))




    return(output)

  }
