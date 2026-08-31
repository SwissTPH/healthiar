#' Check the values of function arguments

# DESCRIPTION ##################################################################
#' @description
#' Low-level helper used by the data validation of the healthiar functions
#' (e.g. \code{validate_input_attribute()}, \code{socialize()}, \code{monetize()}).
#' @description
#' It takes the list of argument values, the names of the arguments to be
#' checked and a predicate returning \code{TRUE} for the valid values.
#' Arguments that are \code{NULL} (i.e. not entered by the user) are skipped,
#' so the callers do not have to filter them beforehand.

# ARGUMENTS ####################################################################
#' @param args \code{List} with the argument values, i.e. \code{input_args$value}.
#' @param arg_names \code{Character vector} with the names of the arguments 
#' to be checked. Names that are not in \code{args} are ignored.
#' @param is_valid \code{Function} taking the argument value and returning 
#' \code{TRUE} for the valid values. It can be vectorized; 
#' the argument is invalid if \strong{any} of its values is not valid.
#' @param message \code{String} with the message. 
#' Write \code{\{arg\}} where the name(s) of the argument(s) concerned have to appear.
#' @param report \code{String} specifying which arguments are named in the message. 
#' Options: \code{"first"} (default, only the first argument found to be invalid), 
#' \code{"all"} (all invalid arguments, separated by commas).
#' @param type \code{String} specifying the condition to be signalled. 
#' Options: \code{"error"} (default), \code{"warning"}.

# DETAILS ######################################################################
#' @details
#' \strong{\code{report}}
#'
#' Use \code{"first"} when the message is specific to one argument
#' (e.g. the options of a categorical argument), so that the users are not
#' confronted with the options of all arguments at once.
#' Use \code{"all"} when the message is the same for all arguments
#' (e.g. "must not be lower than 0"), so that the users can correct all of them
#' in one go instead of one at a time.
#'
#' \strong{Order of the calls}
#'
#' The order of the calls matters: a predicate is only safe once the previous
#' calls have ruled out the value types it cannot handle. E.g. checking whether
#' a value is a whole number (\code{x == base::floor(x)}) requires that the
#' numeric check has already been passed.

# VALUE ########################################################################
#' @returns
#' This function returns nothing. It is called for its side effect, i.e. an
#' error or a warning if any of the argument values is not valid.

# EXAMPLES #####################################################################
#' @examples
#' \dontrun{
#' # Message about one single argument
#' validate_args(
#'   args = input_args$value,
#'   arg_names = c("n_quantile", "population"),
#'   is_valid = base::is.numeric,
#'   message = "{arg} must contain numeric value(s).")
#'
#' # Message listing all the arguments concerned
#' validate_args(
#'   args = input_args$value,
#'   arg_names = c("prop_pop_exp", "dw_central"),
#'   is_valid = function(x){x <= 1},
#'   message = "The values in the following arguments must not be higher than 1: {arg}.",
#'   report = "all")
#' }

#' @author Alberto Castro & Axel Luyten

#' @keywords internal


validate_args <-
  function(args,
           arg_names,
           is_valid,
           message,
           report = "first",
           type = "error"){

    invalid_arg_names <- base::character(0)

    # A loop (instead of e.g. purrr) to be able to stop as soon as possible
    # if report = "first" and to avoid building intermediate lists
    for(arg_name in arg_names){

      arg_value <- args[[arg_name]]

      # Skip the arguments that the user did not enter.
      # [[ ]] returns NULL (and no error) if the name is not in args at all
      if(base::is.null(arg_value)){ next }

      if(base::any(!is_valid(arg_value))){

        invalid_arg_names <- c(invalid_arg_names, arg_name)

        # No need to look at the remaining arguments
        if(report == "first"){ break }
      }
    }

    if(base::length(invalid_arg_names) > 0){

      # toString() returns the name itself if there is only one
      text <-
        base::gsub("{arg}",
                   base::toString(invalid_arg_names),
                   message,
                   fixed = TRUE)

      if(type == "error"){
        base::stop(text, call. = FALSE)
      } else if (type == "warning"){
        base::warning(text, call. = FALSE)
      }
    }
  }
