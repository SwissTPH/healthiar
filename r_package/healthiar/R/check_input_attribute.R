#' Check the input data of attribute_master()

#' @description
#' Check the input data in attribute_master() and provides specific warnings or errors if needed.
#' @param input_args \code{List} with the argument names and values entered in the function
#' @returns This function returns warning or error messages if needed.
#' @author Alberto Castro & Axel Luyten
#' @keywords internal

check_input_attribute <-
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
        identical(get_length(var_1), get_length(var_2))
      } else {TRUE}

    }

    error_if_different_length <- function(var_1, var_2){
      if(!is.null(var_1) & !is.null(var_2)){ # Only if available
        if(!same_length(var_1, var_2)){
          stop(paste0(var_1, "and", var_2, "must have the same length."))
        }
      }
    }

    error_if_ar_and_length_1_or_0 <- function(var){
      if(!is.null(var)){ # Only if available
        if(!get_length(var) > 1){
          stop(
            paste0("For absolute risk, the length of ",
                   var ,
                   " must be higher than 1."))
        }
      }
    }


    # length(exp) = length(prop_pop_exp) ###########

    # Exposure has to have the same length as prop_pop_exp
    # Only for relative risk
    if(approach_risk == "relative_risk"){

      error_if_different_length(exp_central, prop_pop_exp)
      error_if_different_length(exp_lower, prop_pop_exp)
      error_if_different_length(exp_upper, prop_pop_exp)

    }

    # if absolute_risk --> length(exp)>1 ###########
    if(approach_risk == "absolute_risk"){

      error_if_ar_and_length_1_or_0(exp_central)
      error_if_ar_and_length_1_or_0(exp_lower)
      error_if_ar_and_length_1_or_0(exp_upper)


    }



  }
