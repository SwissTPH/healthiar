#' Check the input data of attribute_master()

#' @description
#' Check the input data in attribute_master() and provides specific warnings or errors if needed.
#' @param input \code{List} with the argument names and values entered in the function
#' @returns This function returns warning or error messages if needed.
#' @author Alberto Castro & Axel Luyten
#' @keywords internal

validate_input_attribute <-
  function(input_args){

    # Create a copy of input args to modify data set when needed
    input <- input_args

    # Add age range (needed below)
    if(!is.null(input$first_age_pop) && !is.null(input$last_age_pop)){
    input$age_range <- input$last_age_pop:input$first_age_pop
    }


    # Functions ###########

    get_length <- function(var){
      length <-
        ifelse(is.list(var),
               length(var[[1]]), # Take first element for example
               length(var))
      return (length)
    }

    same_length <- function(var_value_1, var_value_2){

      # Only if var_2 (e.g. prop_pop_exp) is not 1 (default value)
      if(!identical(var_value_2, 1)){
        get_length(var_value_1) == get_length(var_value_2)
      } else {TRUE}
    }

    error_if_different_length <- function(var_name_1, var_name_2){

      # Store var_value
      var_value_1 <- input[[var_name_1]]
      var_value_2 <- input[[var_name_2]]

      if(!is.null(var_value_1) && !is.null(var_value_2) && # Only if vars are available
         !same_length(var_value_1, var_value_2)){

          # Create error message
          stop(paste0(var_name_1,
                      " and ",
                      var_name_2,
                      " must have the same length."),
               call. = FALSE)
        }
      }

    error_if_lower_than_0 <- function(var_name){
      var_value <- input[[var_name]]

      if(!is.null(var_value) && # Only if available
         any(unlist(var_value) < 0)){ # any(unlist( To make it robust for lists
        # Create error message
          stop(paste0(var_name,
                      " cannot be lower than 0"),
               call. = FALSE)
        }
    }

    error_if_higher_than_1 <- function(var_name){
      var_value <- input[[var_name]]

      if(!is.null(var_value) && # Only if available
         any(unlist(var_value) > 1)){ # any(unlist( To make it robust for lists
        # Create error message
        stop(paste0(var_name,
                    " cannot be higher than 1"),
             call. = FALSE)
      }
    }

    error_if_sum_higher_than_1 <- function(var_name){
      var_value <- input[[var_name]]

      if(!is.null(var_value)){ # Only if available
         if((is.list(var_value) &&
            any(purrr::map_lgl(var_value, ~ sum(.x, na.rm = TRUE) > 1))) |

            (is.vector(var_value) &&
            sum(var_value, na.rm = TRUE) > 1)){

        # Create error message
        stop(paste0("The sum of values in ",
                    var_name,
                    " cannot be higher than 1"),
             call. = FALSE)
      }
      }
    }

    error_if_ar_and_length_1_or_0 <- function(var_name){
      # Store var_value
      var_value <- input[[var_name]]

      if(!is.null(var_value) &&  # Only if available
         !get_length(var_value) > 1){
        # Create error message
          stop(
            paste0("For absolute risk, the length of ",
                   var_name ,
                   " must be higher than 1."),
            call. = FALSE)
        }
      }

    warning_if_ar_and_existing <- function(var_name){

      # Store var_value
      var_value <- input[[var_name]]

      if(!is.null(var_value)){ # Only if available
        # Create warning message
        warning(
          paste0("For absolute risk, the value of ",
                 var_name,
                 " is not considered (cutoff defined by exposure-response function)"),
          call. = FALSE)
      }
    }

    # Relevant variables ###########

    # ci_suffix to avoid repetitions
    ci_suffix <- c("_central", "_lower", "_upper")

    # numeric_var_names to use it in error_if_lower_than_0()
    # which can be used only if the variable is numeric
    numeric_var_names <-
      input |>
      purrr::keep(is.numeric) |>
      base::names()

    lifetable_var_names_with_same_length <-
      c("age_range",
        "deaths_male", "deaths_female",
        "population_midyear_male", "population_midyear_female")

    # All pathways #####
    # --> Error if rr if lower than 0

    # Check one-by-one in loop
    #(purrr does not allow deactivating part of the error message)
    for (x in numeric_var_names) {
      error_if_lower_than_0(x)
    }

    # --> Error if any(prop_pop_exp) and dw are higher than 1
    for (x in c("prop_pop_exp", paste0("dw", ci_suffix))) {
      error_if_higher_than_1(x)
    }

    # --> error if length of life table variables is different

    for (i in seq_along(lifetable_var_names_with_same_length)) {
      for(j in seq_along(lifetable_var_names_with_same_length)){
        if(i<j){
          error_if_different_length(lifetable_var_names_with_same_length[i],
                                    lifetable_var_names_with_same_length[j])
        }
      }
    }

    # --> Error if sum(prop_pop_exp) > 1
    error_if_sum_higher_than_1("prop_pop_exp")




    # If relative risk #####

    # Exposure has to have the same length as prop_pop_exp
    # Only for relative risk
    if(input$approach_risk == "relative_risk"){
      # --> length(exp) and length(prop_pop_exp) must be the same
      for(exp_ci_suffix in paste0("exp", ci_suffix)){
        error_if_different_length(exp_ci_suffix, "prop_pop_exp")
      }
    }

    # if absolute_risk ###########
    if(input$approach_risk == "absolute_risk"){

      # --> Error if length(exp) is not higher than 1

      for(exp_ci_suffix in paste0("exp", ci_suffix)){
        error_if_ar_and_length_1_or_0(exp_ci_suffix)
      }

      # --> Warning if cutoff is entered (will not be considered)

      for(cutoff_ci_suffix in paste0("cutoff", ci_suffix)){
        warning_if_ar_and_existing(cutoff_ci_suffix)
      }
    }



  }
