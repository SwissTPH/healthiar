#' Check if arguments are identical before stop

# DESCRIPTION ##################################################################
#' @description
#' This function checks if two different sets of arguments are identical

# ARGUMENTS ####################################################################
#' @param args_a \code{List} first list of arguments
#' @param args_b \code{List} second list of arguments
#' @param names_to_check \code{Vector} with the names of arguments to be checked

# EXAMPLES #####################################################################
#' @examples
#' # TODO

#' @author Alberto Castro & Axel Luyten

#' @keywords internal



check_if_args_identical <- function(args_a, args_b, names_to_check) {

  # Compare values
  checked_values <-
    purrr::map_lgl(names_to_check, ~ identical(args_a[[.x]], args_b[[.x]])) |>
    setNames(names_to_check)  # Name the result

  return(checked_values)
}
