#' get_input_args

# DESCRIPTION ##################################################################
#' @description
#' This function compiles the input data of the main function and calculates the population attributable fraction based on the input data (all in one data frame)

# VALUE ########################################################################
#' @returns
#' This function returns a \code{list} with all input data together by argument

# EXAMPLES #####################################################################
#' @examples
#' # TODO

#' @author Alberto Castro & Axel Luyten

#' @keywords internal



get_input_args <-
  function(environment, call){

    # Create empty list
    input_args <- list()

    # Get all values passed as arguments
    input_args$value <- base::as.list(environment)

    is_not_null <- purrr::map_lgl(input_args$value, function(x){!is.null(x)})

    # Get what the arguments that the user passed
    input_args_passed <- base::as.list(call)[-1] # drop function name

    # Tag arguments showing if they are explicitly passed by the user (TRUE/FALSE)
    is_passed <- base::names(input_args$value) %in% base::names(input_args_passed)

    input_args$is_entered_by_user <-
      base::as.list(stats::setNames(
        is_passed,
        base::names(input_args$value)))

    # Specify if the default value was provided
    # If the argument is not null but there is a value differnt to NULL
    is_default <- is_not_null & !is_passed

    input_args$is_default <-
      base::as.list(stats::setNames(
        is_default,
        base::names(input_args$value)))


    return(input_args)

  }
