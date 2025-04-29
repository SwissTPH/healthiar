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

    # length(exp) = length(prop_pop_exp) ###########

    # Exposure has to have the same length as prop_pop_exp
    # Only for relative risk
    if(approach_risk == "relative_risk"){

      if(!same_length(exp_central, prop_pop_exp)){
        stop("exp_central and prop_pop_exp must have the same length.")
      }

      if(!is.null(exp_lower)){ # Only if available
        if(!same_length(exp_lower, prop_pop_exp)){
          stop("exp_lower and prop_pop_exp must have the same length.")
        }
      }

      if(!is.null(exp_upper)){ # Only if available
        if(!same_length(exp_upper, prop_pop_exp)){
          stop("exp_upper and prop_pop_exp must have the same length.")
        }
      }


    }

    # if absolute_risk --> length(exp)>1 ###########
    if(approach_risk == "absolute_risk"){
      if(!get_length(exp_central) > 1){
        stop("For absolute risk, the length of exp_central must be higher than 1.")
      }

      if(!is.null(exp_lower)){ # Only if available
        if(!get_length(exp_lower) > 1){
        stop("For absolute risk, the length of exp_lower must be higher than 1.")
        }
      }

      if(!is.null(exp_upper)){ # Only if available
        if(!get_length(exp_upper) > 1){
          stop("For absolute risk, the length of exp_lower must be higher than 1.")
        }
      }

    }



  }
