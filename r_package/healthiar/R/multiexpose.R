#' Aggregate health impacts from multiple exposures

#' @description
#' This function aggregates health impacts from multiple exposures to environmental stressors.

#' @param output_attribute_1,output_attribute_2  Output of attribute() for exposure 1 and 2 resp.Baseline health data and population cannot be different in exposure 1 and 2.
#' @param exposure_name_1,exposure_name_2 \code{String} referring to the name of the environmental exposures 1 and 2
#' @param approach \code{String} specifying the multiple exposures approach to be used in the assessment. Options: "additive" (default), "multiplicative" or "combined".

#' @author Alberto Castro & Axel Luyten

#' @note Experimental function

#' @examples
#' results_pm_copd <- attribute_health(
#'   erf_shape = "log_linear",
#'   rr_central = 1.369,
#'   rr_increment = 10,
#'   exp_central = 8.85,
#'   cutoff_central = 5,
#'   bhd_central = 30747
#' )
#' results_pm_copd$health_main$impact
#'
#' results_no2_copd <- attribute_mod(
#'   output_attribute_1 = results_pm_copd,
#'   exp_central = 10.9,
#'   rr_central = 1.031
#' )
#' results_no2_copd$health_main$impact
#'
#' results <- multiexpose(
#'   output_attribute_1 = results_pm_copd,
#'   output_attribute_2 = results_no2_copd,
#'   exposure_name_1 = "pm2.5",
#'   exposure_name_2 = "no2",
#'   approach = "multiplicative"
#' )
#' results$health_main$impact

#' @export



multiexpose <-
  function(
    output_attribute_1,
    output_attribute_2,
    exposure_name_1,
    exposure_name_2,
    approach = "additive"){

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
      results_raw <-
        healthiar:::get_impact(input_table = input_table,
                               pop_fraction_type = "paf")

      # Get the main and detailed output by aggregating and/or filtering cases (rows)
      output <-
        healthiar:::get_output(input_args = input_args,
                               input_table = input_table,
                               results_raw = results_raw)

      # Put the column exposure_name as first column because it is now relevant
      output[["health_detailed"]][c("input_table", "results_raw")] <-
        purrr::map(output[["health_detailed"]][c("input_table", "results_raw")],
                   ~ dplyr::select(.x,
                                   exposure_name, dplyr::everything()))




    return(output)

  }
