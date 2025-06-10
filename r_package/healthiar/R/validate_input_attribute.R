#' Check the input data of attribute_master()

#' @description
#' Check the input data in attribute_master() and provides specific warnings or errors if needed.
#' @param input \code{List} with the argument names and values entered in the function.
#' @param unused_args \code{String vector} with the argument names that were not actively entered by the user.
#' @returns This function returns warning or error messages if needed.
#' @author Alberto Castro & Axel Luyten
#' @keywords internal

validate_input_attribute <-
  function(input_args, unused_args){

    # Data sets ###########

    # Create a copy of input args to modify data set when needed
    input <- input_args

    # Add age range (needed below)
    if(!base::is.null(input$first_age_pop) && !base::is.null(input$last_age_pop)){
    input$age_range <- input$last_age_pop:input$first_age_pop
    }

    available_input <-
      purrr::keep(input, ~!base::is.null(.x))



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
        approach_exposure = c("single_year", "constant"),
        approach_newborns = c("without_newborns", "with_newborns")
      )

    categorical_args <- names(options_of_categorical_args)

    lifetable_var_names_with_same_length <-
      c("deaths_male", "deaths_female",
        "population_midyear_male", "population_midyear_female")

    available_var_names <-
      names(available_input)

    available_numeric_var_names <-
      available_var_names[available_var_names %in% numeric_args]

    available_categorical_var_names <-
      available_var_names[available_var_names %in% categorical_args]


    # Functions and calls ###########

    ## Errors #####
    error_if_var_1_but_not_var_2 <- function(var_name_1, var_name_2){
      var_value_1 <- input_args[[var_name_1]]
      var_value_2 <- input_args[[var_name_2]]

      if(!is.null(var_value_1) && is.null(var_value_2)){
        stop(
          paste0(
            "If ",
            var_name_2,
            " is empty, you cannot use ",
            var_name_2,
            "."),
          call. = FALSE)
      }
    }

    error_if_var_1_but_not_var_2(var_name_1 = "geo_id_aggregated",
                                 var_name_2 = "geo_id_disaggregated")



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
    # --> Error if numeric argument has non-numeric value
    for (x in available_numeric_var_names) {
      error_if_not_numeric(var_name = x)
    }




    error_if_not_an_option <- function(var_name){
      var_value <- input_args[[var_name]]
      var_options <- options_of_categorical_args[[var_name]]

      if(!var_value %in% var_options){

        base::stop(
          base::paste0(
            "For ", var_name,",\n",
            "please, type (between quotation marks) one of this options: \n",
            base::paste0(var_options, collapse = ", ")),
          call. = FALSE)
      }
    }

    # --> Error if numeric argument has non-numeric value
    for (x in available_categorical_var_names) {
      error_if_not_an_option(var_name = x)
    }


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

      if(# Deactivated because only available var_names are passed below
        #!base::is.null(var_value_1) && !base::is.null(var_value_2) &&

         !same_length(var_value_1, var_value_2) &&
         # For the case of prop_pop_exp which can be NULL
         # and get default value in compile_input()
         !is.null(var_value_1) &&
         !is.null(var_value_2)){

          # Create error message
          stop(base::paste0(var_name_1,
                      " and ",
                      var_name_2,
                      " must have the same length."),
               call. = FALSE)
        }
    }

    # If rr --> length(exp) and length(prop_pop_exp) must be the same

    # Exposure has to have the same length as prop_pop_exp
    # Only for relative risk
    if(input$approach_risk == "relative_risk"){

      available_exp_var_names <-
        available_var_names[available_var_names %in%
                              base::paste0("exp", ci_suffix)]

      for(x in available_exp_var_names){
        error_if_different_length(x, "prop_pop_exp")
      }
    }

    # --> error if length of life table variables is different

    if(all(lifetable_var_names_with_same_length %in% available_var_names)){
      combi_vars <-
        utils::combn(lifetable_var_names_with_same_length, 2)|>
        base::t() |>
        base::as.data.frame() |>
        stats::setNames(c("var_1", "var_2"))

      for (i in 1:base::nrow(combi_vars)) {
        error_if_different_length(combi_vars$var_1[i],
                                  combi_vars$var_2[i])
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

    # --> Error if rr if lower than 0
    # Check one-by-one in loop
    #(purrr does not allow deactivating part of the error message)
    for (x in numeric_var_names) {
      error_if_lower_than_0(x)
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

    # --> Error if base::any(prop_pop_exp) and dw are higher than 1
    for (x in c("prop_pop_exp", base::paste0("dw", ci_suffix))) {
      error_if_higher_than_1(x)
    }


    error_if_sum_higher_than_1 <- function(var_name){
      var_value <- input[[var_name]]

      if(base::is.null(input[["geo_id_disaggregated"]])){
        geo_id_disaggregated <- as.character(1)
      } else { geo_id_disaggregated <- input[["geo_id_disaggregated"]]}

      input <- tibble::tibble(
        geo_id_disaggregated = geo_id_disaggregated,
        var = var_value
      )

      if(!base::is.null(var_value)){ # Only if available
         if(input |>
            dplyr::group_by(geo_id_disaggregated) |>
            dplyr::summarize(sum = base::sum(var, na.rm = TRUE) > 1) |>
            dplyr::pull(sum) |>
            base::any()){

        # Create error message
        stop(base::paste0(
          "The sum of values in ",
          var_name,
          " cannot be higher than 1 for each geo unit."),
          call. = FALSE)
      }
      }
    }

    # --> Error if base::sum(prop_pop_exp) > 1
    error_if_sum_higher_than_1(var_name = "prop_pop_exp")


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

    # --> Error if not lower>central>upper
    for (x in c("rr", "bhd", "exp", "cutoff", "dw", "duration")) {
      error_if_not_increasing_lower_central_upper(var_ci = base::paste0(x, ci_suffix))
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

    # --> Error if lower but not upper (or vice versa)
    for (x in c("rr", "bhd", "exp", "cutoff", "dw", "duration")) {
      error_if_only_lower_or_upper(var_short = x)
    }

    warning_if_ar_and_existing <- function(var_name){

      # Store var_value
      var_value <- input[[var_name]]

      if(!base::is.null(var_value) && !var_value == 0){ # Only if available
        # Create warning message
        base::warning(
          base::paste0(
            "For absolute risk, the value of ",
            var_name,
            " is not considered (",
            var_name,
            " defined by exposure-response function)"),
          call. = FALSE)
      }
    }

    if(input$approach_risk == "absolute_risk"){

      # --> Warning if cutoff is entered (will not be considered)

      for(cutoff_ci_suffix in base::paste0("cutoff", ci_suffix)){
        warning_if_ar_and_existing(cutoff_ci_suffix)
      }
    }

    # For absolute risk no cutoff is used (not relevant)
    if("cutoff_central" %in% unused_args &&
       input_args$approach_risk == "relative_risk"){

      base::warning(
        "You entered no value for cut-off. Therefore, zero has been assumed as cut-off. Be aware that this can determine your results.",
        call. = FALSE)

    }







  }
