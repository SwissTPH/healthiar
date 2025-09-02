#' Check the input_args data of attribute_master()

# DESCRIPTION ##################################################################
#' @description
#' Check the input_args data in attribute_master() and provides specific warnings or errors if needed.

# ARGUMENTS ####################################################################
#' @param input_args \code{List} with the argument names and values entered in the function.
#' @param is_lifetable \code{Boolean} INTERNAL argument specifying if the life table approach is applied (TRUE) or not (FALSE)

# VALUE ########################################################################
#' @returns This function returns warning or error messages if needed.

# EXAMPLES #####################################################################
#' @examples
#' # TODO
#'

#' @author Alberto Castro & Axel Luyten

#' @keywords internal

validate_input_attribute <-
  function(input_args, is_lifetable){

    # Relevant variables ###########

    input_args_value <- input_args$value

    arg_names_passed <-
      purrr::keep(input_args$is_entered_by_user, ~.x) |>
      base::names()

    # ci_suffix to avoid repetitions
    ci_suffix <- c("_central", "_lower", "_upper")

    # Arguments
    args <- base::names(input_args_value )

    ci_args <- args[base::grep("_central|_lower|_upper", args)]

    ci_args_wo_eq <- ci_args[!base::grepl("erf_eq", ci_args)]

    numeric_args <-
      c(ci_args_wo_eq,
        "prop_pop_exp", "pop_exp", "rr_increment", "population",
        "year_of_analysis", "time_horizon", "min_age", "max_age")

    # Only if is_lifetable, then age_group is numeric.
    # Otherwise, it can be a string e.g. for socialize()
    if(is_lifetable){
      numeric_args <- c(numeric_args, "age_group")
    }

    boolean_args <- "is_lifetable"

    string_args <- args[!args %in% c(numeric_args, boolean_args)]

    options_of_categorical_args <-
      base::list(
        approach_risk = c("relative_risk", "absolute_risk"),
        erf_shape = c("linear", "log_linear", "log_log", "linear_log"),
        approach_exposure = c("single_year", "constant"),
        approach_newborns = c("without_newborns", "with_newborns")
      )

    categorical_args <- base::names(options_of_categorical_args)

    lifetable_var_names_with_same_length <-
      c("bhd_central", "bhd_lower", "bhd_upper", "population", "age_range", "sex")


    var_names_available <-
      purrr::keep(input_args_value, ~!base::is.null(.x)) |>
      base::names()

    numeric_var_names_available <-
      base::intersect(var_names_available, numeric_args)

    categorical_var_names_available <-
      base::intersect(var_names_available, categorical_args)


    # Define approach_risk here because in the life table approach
    # approach_risk can only be relative_risk
    # and it is defined at the level of attribute_master()
    # and therefore not available input_args

    if(is_lifetable) {
      approach_risk <- "relative_risk"
      # Otherwise what is entered in input_args
      }else{ approach_risk <- input_args_value[["approach_risk"]]}


    # Functions and calls ###########

    ## Errors #####

    ### error_if_var_1_but_not_var_2 #####

    error_if_var_1_but_not_var_2 <- function(var_name_1, var_name_2){
      # Check arg_names_passed in case that there is a default value (safer)
      if(var_name_1 %in% arg_names_passed &&
         !var_name_2 %in% arg_names_passed){
        stop(
          base::paste0(
            "If you do not pass a value for ",
            var_name_2,
            ", you cannot use ",
            var_name_1,
            "."),
          call. = FALSE)
      }
    }


    # If users enter a value for geo_id_macro but not for geo_id_micro
    # the impact cannot be grouped accordingly (multiple geo_id_micro are needed)
    error_if_var_1_but_not_var_2(var_name_1 = "geo_id_macro",
                                 var_name_2 = "geo_id_micro")


    ### error_if_not_numeric #####
    error_if_not_numeric <- function(var_name){

      var_value <- input_args_value[[var_name]]

      if(! base::is.numeric(var_value)){

      # if(base::any( ! base::is.numeric(base::unlist(var_value)))){

        base::stop(
          base::paste0(
            var_name,
            " must contain numeric value(s)."),
          call. = FALSE)
      }
    }


    for (x in numeric_var_names_available) {
      error_if_not_numeric(var_name = x)
    }


    ### error_if_not_an_option #####

    error_if_not_an_option <- function(var_name){
      var_value <- input_args_value [[var_name]]
      var_options <- options_of_categorical_args[[var_name]]

      if(!var_value %in% var_options){

        base::stop(
          base::paste0(
            "For ", var_name,
            ", please, type (between quotation marks) one of these options: ",
            base::toString(var_options), "."),
          call. = FALSE)
      }
    }


    for (x in categorical_var_names_available) {
      error_if_not_an_option(var_name = x)
    }

    ### error_if_different_length #####


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
      var_value_1 <- input_args_value [[var_name_1]]
      var_value_2 <- input_args_value [[var_name_2]]

      if(# Deactivated because only available var_names are passed below
        #!base::is.null(var_value_1) && !base::is.null(var_value_2) &&

         !same_length(var_value_1, var_value_2) &&
         # For the case of prop_pop_exp which can be NULL
         # and get default value in compile_input()
         !base::is.null(var_value_1) &&
         !base::is.null(var_value_2)){

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

    if(approach_risk == "relative_risk"){

      available_exp_var_names <-
        base::intersect(var_names_available, base::paste0("exp", ci_suffix))

      for(x in available_exp_var_names){
        error_if_different_length(x, "prop_pop_exp")
      }
    }


    #### error_if_info_with_incompatible_length ####


    if(!base::is.null(input_args_value$info)){

      if(base::is.data.frame(input_args_value$info)){
        length_info <- base::nrow(input_args_value$info)
      } else if (base::is.vector(input_args_value$info)){
        length_info <- base::length(input_args_value$info)
      }

      max_length <- base::max(purrr::map_vec(input_args_value, base::length))

      if( !length_info == max_length && !length_info == 1){
        base::stop(
          base::paste0("For this assessment, the info vector or data frame columns must have a length of 1 or ", max_length, "."),
          call. = FALSE
        )
      }
    }

    ### error_if_0 #####
    error_if_0 <- function(var_name){
      var_value <- input_args$value[[var_name]]
      if(base::any(var_value < 1)) {
        base::stop(
          base::paste0("All values of ", var_name , " must be 1 or higher."),
          call. = FALSE
        )
      }
    }

    ### error_if_not_consecutive_sequence #####
    error_if_not_consecutive_sequence <- function(var_name){
      var_value <- base::as.numeric(input_args$value[[var_name]])

      if(# Check that values are integers
        base::any(var_value != base::floor(var_value)) &&
        # Check difference between consecutive elements is exactly 1
        base::all(base::diff(var_value))) {

        base::stop(
          base::paste0(var_name, " must be a consecutive sequence of integer values where the difference between elements if 1."),
          call. = FALSE
        )
      }
    }





    if(is_lifetable){

      # --> error if length of life table variables is different
      combi_vars <-
        utils::combn(lifetable_var_names_with_same_length, 2)|>
        base::t() |>
        base::as.data.frame() |>
        stats::setNames(c("var_1", "var_2"))

      for (i in 1:base::nrow(combi_vars)) {
        error_if_different_length(combi_vars$var_1[i],
                                  combi_vars$var_2[i])
      }




      for (x in lifetable_var_names_with_same_length) {
        error_if_0(var_name = x)
      }

      for (x in "age_group") {
        error_if_not_consecutive_sequence(var_name = x)
      }




    }

    ### error_if_erf_eq_not_function_or_string #####

    error_if_erf_eq_not_function_or_string <- function(erf_eq_name){
      erf_eq_value <- input_args$value[[erf_eq_name]]
      # If erf_eq_... is not null (user may not enter a value for this argument)
      if(! base::is.null(erf_eq_value)){
        # If it is a function (single function or multiple functions in a list)
        # and it is not a character
        if((! base::is.function(erf_eq_value) &&
           ! (base::is.list(erf_eq_value) && base::all(purrr::map_lgl(erf_eq_value, is.function)))) &&
           ! base::is.character(erf_eq_value)){

          base::stop(
            base::paste0(erf_eq_name , " must be a (list of) function(s) or a (vector of) string(s)."),
            call. = FALSE
          )
        }
      }
    }


      error_if_erf_eq_not_function_or_string(erf_eq_name = "erf_eq_central")
      error_if_erf_eq_not_function_or_string(erf_eq_name = "erf_eq_lower")
      error_if_erf_eq_not_function_or_string(erf_eq_name = "erf_eq_upper")





    ### error_if_lower_than_0 #####

    error_if_lower_than_0 <- function(var_name){
      var_value <- input_args_value [[var_name]]

      if(!base::is.null(var_value) && # Only if available
         base::any(base::unlist(var_value) < 0)){ # base::any(unlist( To make it robust for lists
        # Create error message
          stop(base::paste0(var_name,
                            " cannot be lower than 0."),
               call. = FALSE)
        }
    }


    for (x in numeric_var_names_available) {
      error_if_lower_than_0(x)
    }


    ### error_if_higher_than_1 #####

    error_if_higher_than_1 <- function(var_name){
      var_value <- input_args_value [[var_name]]

      if(!base::is.null(var_value) && # Only if available
         base::any(unlist(var_value) > 1)){ # base::any(unlist( To make it robust for lists
        # Create error message
        stop(base::paste0(var_name,
                    " cannot be higher than 1."),
             call. = FALSE)
      }
    }

    # Error if base::any(prop_pop_exp) and dw are higher than 1
    for (x in c("prop_pop_exp", base::paste0("dw", ci_suffix))) {
      error_if_higher_than_1(x)
    }

    ### error_if_multi_geo_and_different_length #####

    # Error if multiple geo units and length of some geo dependent variables are different
    # (geo_ids, exp_central, prop_pop_exp, pop_exp and bhd) must be the same
    # i.e. enter the data as in the table
    error_if_multi_geo_and_different_length  <- function(list, var_names){

      # Remove NULLs
      non_nulls <-
        list[var_names] |>
        purrr::discard(is.null)

      # Get lengths of non-NULLs
      lengths <- purrr::map_int(non_nulls, length)

      if (base::length(base::unique(list$geo_id_micro)) > 1 &&
          !base::all(lengths == base::length(list$geo_id_micro))) {

        base::stop(
          base::paste0("The following variables must all have the same length: ",
                       base::paste0(base::names(non_nulls),
                                    collapse = ", "),
                       "."),
          call. = FALSE
        )
      }
    }


    error_if_multi_geo_and_different_length(list = input_args_value ,
                                            var_names = c("geo_id_micro",
                                                          "exp_central",
                                                          "pop_exp",
                                                          "bhd_central",
                                                          "population"))

    ### error_if_sum_higher_than_1 #####
    error_if_sum_higher_than_1 <- function(var_name){

      var_value <- input_args_value [[var_name]]

      var_table <-
        tibble::tibble(
          exp_name = input_args_value$exp_name,
          geo_id_micro = input_args_value$geo_id_micro,
          age_group = input_args_value$age_group,
          sex = input_args_value$sex,
          exp_ci = input_args_value$exp_ci,
          cutoff_ci = input_args_value$cutoff_ci,
          erf_ci = input_args_value$erf_ci,
          bhd_ci = input_args_value$bhd_ci,
          dw_ci =  input_args_value$dw_ci,
          duration_ci = input_args_value$duration_ci,
          var = var_value)

      if(base::is.null(input_args_value [["pop_exp"]]) &&
         var_table |>
         dplyr::summarize(
           .by = c(-var),
           sum = base::sum(var, na.rm = TRUE) > 1) |>
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

    # Call function checking if base::sum(prop_pop_exp) > 1
    error_if_sum_higher_than_1(var_name = "prop_pop_exp")




    ### error_if_not_increasing_lower_central_upper #####
    error_if_not_increasing_lower_central_upper <-
      function(var_ci){
        # Store var_name from vector var_ci
        var_name_lower <- var_ci[base::grep("lower", var_ci)]
        var_name_central <- var_ci[base::grep("central", var_ci)]
        var_name_upper <- var_ci[base::grep("upper", var_ci)]

        # Store var_value
        var_value_lower <- input_args_value [[var_name_lower]]
        var_value_central <- input_args_value [[var_name_central]]
        var_value_upper <- input_args_value [[var_name_upper]]

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

    # Call function checking if error if not lower>central>upper
    for (x in c("rr", "bhd", "exp", "cutoff", "dw", "duration")) {
      error_if_not_increasing_lower_central_upper(var_ci = base::paste0(x, ci_suffix))
    }

    ### error_if_only_lower_or_upper #####
    error_if_only_lower_or_upper <- function(var_short){
      var_name_lower <- base::paste0(var_short, "_lower")
      var_name_upper <- base::paste0(var_short, "_upper")

      var_value_lower <- input_args_value [[var_name_lower]]
      var_value_upper <- input_args_value [[var_name_upper]]

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
              ", or none of them must entered, but not only one."),
            call. = FALSE)
        }
      }
    }

    # Call function checking if lower but not upper (or vice versa)
    for (x in c("rr", "bhd", "exp", "cutoff", "dw", "duration")) {
      error_if_only_lower_or_upper(var_short = x)
    }

    ### error_if_var_and_risk #####

    error_if_var_and_risk <- function(var_name, risk){

      # Identify the alternative options
      all_approach_risks <- c("relative_risk", "absolute_risk")
      all_var_names <- c("prop_pop_exp", "pop_exp")
      another_approach_risk <- base::setdiff(all_approach_risks, risk)
      another_var_name <- base::setdiff(all_var_names, var_name)

      if(var_name %in% arg_names_passed &&
         approach_risk == risk){
        stop(base::paste0("The argument ",
        var_name,
        " is aimed for ",
        # Remove the underscore
        base::gsub("_", " ", another_approach_risk),
        ". Use ",
        another_var_name,
        " instead."),
          call. = FALSE
        )
      }
    }

    # Call function
    error_if_var_and_risk(var_name = "pop_exp", risk = "relative_risk")
    error_if_var_and_risk(var_name = "prop_pop_exp", risk = "absolute_risk")

    ## NOTE 2024-08-08: the two error message tests for log-log and log-lin have been commented out, as with the new ERFs it's no problem to calculate RR's for exp=0 or when exp <= cutoff; once we've settled on these new ERFs remove these error messages
    ### error_if_any_cutoff_value_is_greater_or_equal_than_any_exp_value ####
    ### only for cases where the erf shape is log_log or lin_log
    # error_if_any_cutoff_value_is_greater_or_equal_than_any_exp_value <- function(
    # cutoff_vector,
    # exp_vector
    # ){
    #
    #   if (
    #     ( base::any( base::outer( cutoff_vector, exp_vector, `>=` ) ) ) &
    #     ( input_args$value$erf_shape == "log_log" | input_args$value$erf_shape == "linear_log" )
    #     ) {
    #     stop(
    #       "if the exposure-response function shape is log-log or linear-log then the values of cutoff_central, cutoff_lower and cutoff_upper must be lower than the values of exposure_central, exposure_lower and exposure_upper. please adjust.",
    #     call. = FALSE
    #     )
    #   }
    # }
    #
    # # Call function
    # ## only in rr cases with erf_shape specified (ar cases don't have a cutoff)
    # if ( input_args$value$approach_risk == "relative_risk" &
    #      !base::is.null(input_args$value$erf_shape) &
    #      !base::is.null(input_args$value$cutoff_central)
    #      ) {
    #   error_if_any_cutoff_value_is_greater_or_equal_than_any_exp_value(
    #     cutoff_vector = c(
    #         input_args$value$cutoff_lower,
    #         input_args$value$cutoff_central,
    #         input_args$value$cutoff_upper
    #       ),
    #     exp_vector = c(
    #       input_args$value$exp_lower,
    #       input_args$value$exp_central,
    #       input_args$value$exp_upper
    #     )
    #   )
    # }


    ### error_if_var_1_and_var_2 #####

    error_if_var_1_and_var_2 <- function(var_name_1, var_name_2){
      # Identify the alternative options

      if(var_name_1 %in% arg_names_passed &&
         var_name_2 %in% arg_names_passed){
        stop(base::paste0("The argument ",
                          var_name_1,
                          " cannot be used together with the argument ",
                          var_name_2,
                          " (either one or the other but not both)."),
             call. = FALSE
        )
      }
    }

    # Call function
    for (a in c("rr_central", "erf_shape", "rr_increment")){
      error_if_var_1_and_var_2(var_name_1 = a, var_name_2 = "erf_eq_central")
    }



    ## Warnings ########################

    ### warning_if_ar_and_var #####
    warning_if_ar_and_var <- function(var_name){

      # Store var_value
      var_value <- input_args_value [[var_name]]

      if(approach_risk == "absolute_risk" &&
         !base::is.null(var_value) && !var_value == 0){ # Only if available
        # Create warning message
        base::warning(
          base::paste0(
            "For absolute risk, the value of ",
            var_name,
            " is not considered; ",
            var_name,
            " is defined by the exposure-response function."),
          call. = FALSE)
      }
    }

    # Call function only if absolute risk

      for(cutoff_ci_suffix in base::paste0("cutoff", ci_suffix)){
        warning_if_ar_and_var(cutoff_ci_suffix)
    }


    ### warning_if_rr_and_no_var_with_default #####
    warning_if_rr_and_no_var_with_default <- function(var_name, default){

    # For absolute risk no cutoff is used (not relevant)
    if(! var_name %in% arg_names_passed &&
       approach_risk == "relative_risk"){

      base::warning(
        base::paste0("You entered no value for ",
        var_name,
        ". Therefore, ",
        default,
        " has been assumed as default. Be aware that this can determine your results."),
        call. = FALSE)

      }
    }

    warning_if_rr_and_no_var_with_default(var_name = "cutoff_central", default = 0)









  }
