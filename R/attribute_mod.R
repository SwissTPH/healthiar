#' Create a scenario 2 by modifying an existing scenario 1 and determine attributable health impacts in it

# DESCRIPTION ##################################################################
#' @description
#' This function assesses the attributable health impacts in a new scenario 2 which is obtained by modifying an existing scenario 1. Supply an existing attribute output and specify how scenario 1 should be modified to create scenario 2.

# ARGUMENTS ####################################################################
#' @inheritParams attribute_master
#' @param output_attribute \code{List} containing the output of the function attribute() for scenario 1.

# VALUE ########################################################################
#' @inherit attribute_master return

# EXAMPLES #####################################################################
#' @examples
#' # Goal: adjust an existing healthiar scenario and determine the health
#' # impacts in the modified scenario
#'
#' ## First create a scenario to be modified
#' scenario_A <- attribute_health(
#'   exp_central = 8.85,   # EXPOSURE 1
#'   cutoff_central = 5,
#'   bhd_central = 25000,
#'   approach_risk = "relative_risk",
#'   erf_shape = "log_linear",
#'   rr_central = 1.118,
#'   rr_increment = 10
#' )
#'
#' scenario_A$health_main$impact # Attributable impact in scenario A
#'
#' ## Modify scenario (adjust exposure value)
#' scenario_B <- attribute_mod(
#'   output_attribute = scenario_A,
#'   exp_central = 6       # EXPOSURE 2
#' )
#'
#' scenario_B$health_main$impact # Attributable impact in scenario B

#' @author Alberto Castro & Axel Luyten

#' @export



attribute_mod <-
  function(
    output_attribute,
    erf_shape = NULL,
    rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
    rr_increment = NULL,
    erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
    exp_central = NULL, exp_lower = NULL, exp_upper = NULL,
    prop_pop_exp = NULL,
    pop_exp = NULL,
    cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
    bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
    geo_id_micro = NULL, geo_id_macro = NULL,
    age_group = NULL, sex = NULL,
    population = NULL,
    info = NULL,
    min_age = NULL, max_age = NULL,
    approach_exposure = NULL,
    approach_newborns = NULL,
    year_of_analysis = NULL
    ) {


    # Capture all arguments and values
    input_args_2_value <- base::as.list(base::environment())

    # Removing output_attribute from args
    input_args_2_value$output_attribute <- NULL

    #Remove all arguments that are NULL in input_args_2_value to avoid that they overwrite
    #those in input_args_1_value
    input_args_2_value <- purrr::discard(input_args_2_value, is.null)


    # Extract input_args_1_value
    input_args_1 <- output_attribute[["health_detailed"]][["input_args"]]

    # New argument names
    input_arg_2_names_with_new_values <- base::names(input_args_2_value)


    # Add input_args
    input_for_attribute_input_args <- output_attribute$health_detailed$input_args

    # Modify values
    input_for_attribute_input_args$value[input_arg_2_names_with_new_values] <-
      input_args_2_value

    # Create input_for_attribute
    # Compilation of the data that to be re-entered in attribute_master() below
    # First all values (arguments in the function)
    input_for_attribute <-
      input_for_attribute_input_args$value
    # Second the hole input_args as list.
    # This is to transported internally
    # because attribute_master does not create input_args,
    # only attribute_health() and attribute_lifetable() create input_args.
    input_for_attribute$input_args <- input_for_attribute_input_args

    # Add is_lifetable
    # which is not available in input_args
    # because it depends on the function call
    input_for_attribute[["is_lifetable"]] <-
      base::unique(output_attribute$health_detailed$input_table$is_lifetable)

    # Use the arguments attribute()
    output_attribute_2 <-
      base::do.call(healthiar:::attribute_master,
                    input_for_attribute)

    return(output_attribute_2)


  }
