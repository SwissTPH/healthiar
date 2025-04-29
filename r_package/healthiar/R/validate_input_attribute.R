#' Check the input data of attribute_master()

#' @description
#' Check the input data in attribute_master() and provides specific warnings or errors if needed.
#' @param input_args \code{List} with the argument names and values entered in the function
#' @returns This function returns warning or error messages if needed.
#' @author Alberto Castro & Axel Luyten
#' @keywords internal

validate_input_attribute <-
  function(input_args){

    # Functions ###########

    # Recreate the variables
    list2env(input_args, envir = environment())

    get_length <- function(var){
      length <-
        ifelse(is.list(var),
               length(var[[1]]), # Take first element for example
               length(var))
      return (length)
    }

    same_length <- function(var_1, var_2){
      # Only if var_2 (e.g. prop_pop_exp) is not 1 (default value)
      if(!identical(var_2, 1)){
        get_length(var_1) == get_length(var_2)
      } else {TRUE}

    }

    error_if_different_length <- function(var_1, var_2){
      if(!is.null(var_1) && !is.null(var_2)){ # Only if available
        if(!same_length(var_1, var_2)){
          # Store varname
          varname_1 <- deparse(substitute(var_1))
          varname_2 <- deparse(substitute(var_2))
          # Create error message
          stop(paste0(varname_1,
                      "and",
                      varname_2,
                      "must have the same length."),
               call. = FALSE)
        }
      }
    }

    error_if_lower_than_0 <- function(var_name){
      var_value <- input_args[[var_name]]

      if(!is.null(var_value)){ # Only if available
        if(any(unlist(var_value) < 0)){ # any(unlist( To make it robust for lists
          # Create error message
          stop(paste0(var_name,
                      " cannot be lower than 0"),
               call. = FALSE)
        }
      }
    }

    error_if_ar_and_length_1_or_0 <- function(var){
      if(!is.null(var)){ # Only if available
        if(!get_length(var) > 1){
          # Store varname
          varname <- deparse(substitute(var))
          # Create error message
          stop(
            paste0("For absolute risk, the length of ",
                   varname ,
                   " must be higher than 1."),
            call. = FALSE)
        }
      }
    }

    warning_if_ar_and_existing <- function(var){
      if(!is.null(var)){ # Only if available
        # Store varname
        varname <- deparse(substitute(var))
        # Create warning message
        warning(
          paste0("For absolute risk, the value of ",
                 varname,
                 " is not considered (cutoff defined by exposure-response function)"),
          call. = FALSE)
      }
    }

    # All pathways #####
    # --> rr must be higher than 0

    numeric_var_names <-
      input_args |>
      purrr::keep(is.numeric) |>
      base::names()

    # Check one-by-one in loop
    #(purrr does not allow deactivating part of the error message)
    for (x in numeric_var_names) {
      error_if_lower_than_0(x)
    }



    # If relative risk #####

    # Exposure has to have the same length as prop_pop_exp
    # Only for relative risk
    if(approach_risk == "relative_risk"){
      # --> length(exp) and length(prop_pop_exp) must be the same
      error_if_different_length(exp_central, prop_pop_exp)
      error_if_different_length(exp_lower, prop_pop_exp)
      error_if_different_length(exp_upper, prop_pop_exp)


    }

    # if absolute_risk ###########
    if(approach_risk == "absolute_risk"){
      # --> length(exp) must be higher than 1
      error_if_ar_and_length_1_or_0(exp_central)
      error_if_ar_and_length_1_or_0(exp_lower)
      error_if_ar_and_length_1_or_0(exp_upper)

      # --> cutoff is not considered
      warning_if_ar_and_existing(cutoff_central)
      warning_if_ar_and_existing(cutoff_lower)
      warning_if_ar_and_existing(cutoff_upper)


    }



  }
