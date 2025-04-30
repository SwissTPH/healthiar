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
    if(!base::is.null(input$first_age_pop) && !base::is.null(input$last_age_pop)){
    input$age_range <- input$last_age_pop:input$first_age_pop
    }

    available_input <-
      purrr::keep(input, ~!base::is.null(.x))


    # Functions ###########

    get_length <- function(var){
      length <-
        base::ifelse(
          base::is.list(var),
          base::length(var[[1]]), # Take first element for example
          base::length(var))
      return (length)
    }

    same_length <- function(var_value_1, var_value_2){

      # Only if var_2 (e.g. prop_pop_exp) is not 1 (default value)
      if(!base::identical(var_value_2, 1)){
        get_length(var_value_1) == get_length(var_value_2)
      } else {TRUE}
    }

    error_if_different_length <- function(var_name_1, var_name_2){

      # Store var_value
      var_value_1 <- input[[var_name_1]]
      var_value_2 <- input[[var_name_2]]

      if(!base::is.null(var_value_1) && !base::is.null(var_value_2) && # Only if vars are available
         !same_length(var_value_1, var_value_2)){

          # Create error message
          stop(base::paste0(var_name_1,
                      " and ",
                      var_name_2,
                      " must have the same length."),
               call. = FALSE)
        }
      }

    error_if_lower_than_0 <- function(var_name){
      var_value <- input[[var_name]]

      if(!base::is.null(var_value) && # Only if available
         base::any(base::unlist(var_value) < 0)){ # base::any(unlist( To make it robust for lists
        # Create error message
          stop(base::paste0(var_name,
                            " cannot be lower than 0"),
               call. = FALSE)
        }
    }

    error_if_higher_than_1 <- function(var_name){
      var_value <- input[[var_name]]

      if(!base::is.null(var_value) && # Only if available
         base::any(unlist(var_value) > 1)){ # base::any(unlist( To make it robust for lists
        # Create error message
        stop(base::paste0(var_name,
                    " cannot be higher than 1"),
             call. = FALSE)
      }
    }

    error_if_sum_higher_than_1 <- function(var_name){
      var_value <- input[[var_name]]

      if(!base::is.null(var_value)){ # Only if available
         if((base::is.list(var_value) &&
            base::any(purrr::map_lgl(var_value, ~ base::sum(.x, na.rm = TRUE) > 1))) |

            (!base::is.list(var_value) &&
             base::sum(var_value, na.rm = TRUE) > 1)){

        # Create error message
        stop(base::paste0(
          "The sum of values in ",
          var_name,
          " cannot be higher than 1"),
          call. = FALSE)
      }
      }
    }

    error_if_not_increasing_lower_central_upper <-
      function(var_ci){
        # Store var_name from vector var_ci
        var_name_lower <- var_ci[base::grep("lower", var_ci)]
        var_name_central <- var_ci[base::grep("central", var_ci)]
        var_name_upper <- var_ci[base::grep("upper", var_ci)]

        # Store var_value
        var_value_lower <- input[[var_name_lower]]
        var_value_central <- input[[var_name_central]]
        var_value_upper <- input[[var_name_upper]]

        if(!base::is.null(var_value_central) &&
           !base::is.null(var_value_lower) &&
           !base::is.null(var_value_upper)){ # Only if available

          if((!base::is.list(var_value_central) &&
             ((base::any(var_value_central < var_value_lower)) |
              (base::any(var_value_central > var_value_upper)))) | #base::any() if vector
             (base::is.list(var_value_central) &&
              base::any(purrr::map2_lgl(var_value_lower, var_value_central, ~base::any(.x > .y))) |
              base::any(purrr::map2_lgl(var_value_upper, var_value_central, ~base::any(.x < .y))))){

            # Create error message
            stop(
              base::paste0(
                var_name_central,
                " must be higher than ",
                var_name_lower,
                " and lower than ",
                var_name_upper,
                "."),
              call. = FALSE)

            }

        }
      }

    error_if_only_lower_or_upper <- function(var_short){
      var_name_lower <- base::paste0(var_short, "_lower")
      var_name_upper <- base::paste0(var_short, "_upper")

      var_value_lower <- input[[var_name_lower]]
      var_value_upper <- input[[var_name_upper]]

      if((!base::is.null(var_value_lower) && base::is.null(var_value_upper)) |
         (base::is.null(var_value_lower) && !base::is.null(var_value_upper)) ){ # Only if available
        {
          # Create error message
          stop(
            base::paste0(
              "Either both, ",
              var_name_lower,
              " and ",
              var_name_upper,
              ", or non of them must entered, but not only one."),
            call. = FALSE)
        }
      }
    }

    # warning_if_only_central <- function(var_short){
    #   var_name_central <- base::paste0(var_short, "_central")
    #   var_name_lower <- base::paste0(var_short, "_lower")
    #   var_name_upper <- base::paste0(var_short, "_upper")
    #
    #   var_value_central <- input[[var_name_central]]
    #   var_value_lower <- input[[var_name_lower]]
    #   var_value_upper <- input[[var_name_upper]]
    #
    #   if((!base::is.null(var_value_central) &&
    #       base::is.null(var_value_lower) &&
    #       base::is.null(var_value_upper))){
    #       # Create warning message
    #       base::warning(
    #         base::paste0(
    #           "All fine. Just consider entering",
    #           var_name_lower,
    #           " and ",
    #           var_name_upper,
    #           "to take into account uncertainty"),
    #         call. = FALSE)
    #     }
    #   }




    error_if_ar_and_length_1_or_0 <- function(var_name){
      # Store var_value
      var_value <- input[[var_name]]

      if(!base::is.null(var_value) &&  # Only if available
         !get_length(var_value) > 1){
        # Create error message
          stop(
            base::paste0(
              "For absolute risk, the length of ",
              var_name ,
              " must be higher than 1."),
            call. = FALSE)
        }
    }



    warning_if_ar_and_existing <- function(var_name){

      # Store var_value
      var_value <- input[[var_name]]

      if(!base::is.null(var_value)){ # Only if available
        # Create warning message
        base::warning(
          base::paste0(
            "For absolute risk, the value of ",
            var_name,
            " is not considered (cutoff defined by exposure-response function)"),
          call. = FALSE)
      }
    }

    error_if_not_numeric <- function(var_name){
      var_value <- input_args[[var_name]]

      if(any(!is.numeric(unlist(var_value)))){

        base::stop(
          base::paste0(
            var_name,
            " must contain numeric value(s)."),
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

    args <- base::names(input_args)
    ci_args <- args[base::grep("_central|_lower|_upper", args)]
    ci_args_wo_eq <- ci_args[!base::grepl("erf_eq", ci_args)]
    numeric_args <-
      c(ci_args_wo_eq,
        "prop_pop_exp",
        "pop_exp",
        "rr_increment",
        "population",
        "year_of_analysis",
        "time_horizon",
        "min_age",
        "max_age",
        "first_age_pop",
        "last_age_pop",
        "population_midyear_male",
        "population_midyear_female",
        "deaths_male",
        "deaths_female")

    boolean_args <- "is_lifetable"

    string_args <- args[!args %in% c(numeric_args, boolean_args)]

    options_of_categorical_args <-
      list(
        approach_risk = c("relative_risk", "absolute_risk"),
        erf_shape = c("linear", "log_linear", "log_log", "linear_log"),
        approach_exposure = c("single_year", "exposure"),
        approach_newborns = c("without_newborns", "with_newborns")
      )

    categorical_args <- names(options_of_categorical_args)

    lifetable_var_names_with_same_length <-
      c("age_range",
        "deaths_male", "deaths_female",
        "population_midyear_male", "population_midyear_female")

    available_var_names <-
      names(available_input)

    available_numeric_var_names <-
      available_var_names[available_var_names %in% numeric_args]




    # All pathways #####
    # --> Error if numeric argument has non-numeric value
    for (x in available_numeric_var_names) {
      error_if_not_numeric(var_name = x)
    }



    # --> Error if rr if lower than 0

    # Check one-by-one in loop
    #(purrr does not allow deactivating part of the error message)
    for (x in numeric_var_names) {
      error_if_lower_than_0(x)
    }

    # --> Error if base::any(prop_pop_exp) and dw are higher than 1
    for (x in c("prop_pop_exp", base::paste0("dw", ci_suffix))) {
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

    # --> Error if base::sum(prop_pop_exp) > 1
    error_if_sum_higher_than_1(var_name = "prop_pop_exp")

    # --> Error if not lower>central>upper
    for (x in c("rr", "bhd", "exp", "cutoff", "dw", "duration")) {
      error_if_not_increasing_lower_central_upper(var_ci = base::paste0(x, ci_suffix))
    }

    # --> Error if lower but not upper (or vice versa)
    for (x in c("rr", "bhd", "exp", "cutoff", "dw", "duration")) {
      error_if_only_lower_or_upper(var_short = x)
    }

    # # --> Warning if only central
    # for (x in c("rr")) { #Vector and for loop if more vars
    #
    #   warning_if_only_central(var_short = x)
    # }




    # If relative risk #####

    # Exposure has to have the same length as prop_pop_exp
    # Only for relative risk
    if(input$approach_risk == "relative_risk"){
      # --> length(exp) and length(prop_pop_exp) must be the same
      for(exp_ci_suffix in base::paste0("exp", ci_suffix)){
        error_if_different_length(exp_ci_suffix, "prop_pop_exp")
      }
    }

    # if absolute_risk ###########
    if(input$approach_risk == "absolute_risk"){

      # --> Error if length(exp) is not higher than 1

      for(exp_ci_suffix in base::paste0("exp", ci_suffix)){
        error_if_ar_and_length_1_or_0(exp_ci_suffix)
      }

      # --> Warning if cutoff is entered (will not be considered)

      for(cutoff_ci_suffix in base::paste0("cutoff", ci_suffix)){
        warning_if_ar_and_existing(cutoff_ci_suffix)
      }
    }



  }
