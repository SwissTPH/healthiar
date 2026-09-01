#' Check the input_args data of attribute_master()

# DESCRIPTION ##################################################################
#' @description
#' Check the input_args data in attribute_master() and provides specific warnings or errors if needed.

# ARGUMENTS ####################################################################
#' @param input_args \code{List} with the argument names and values entered in the function.
#' @param is_lifetable \code{Boolean} INTERNAL argument specifying if the life table approach is applied (TRUE) or not (FALSE)

# VALUE ########################################################################
#' @returns This function returns warning or error messages if needed.

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
        "year_of_analysis", "time_horizon", "min_age", "max_age",
        "fraction_lived")

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

    lifetable_args_with_values_above_0 <- "population"  

    lifetable_args_with_values_0_or_above <-
      c("bhd_central", "bhd_lower", "bhd_upper", "fraction_lived")


    # Only needed where the number of non-NULL arguments matters
    # (the _ci prefixes below) or where non-NULL counts as entered
    # (validate_arg_pair). The validate_args() calls do NOT need it because
    # validate_args() skips the NULL arguments itself
    arg_names_available <-
      purrr::keep(input_args_value, ~!base::is.null(.x)) |>
      base::names()


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

    # Create validate_arg_pair() for checks that do not depend on the
    # VALUES of an argument but on WHICH arguments were entered.
    # present_arg_names: the names that count as entered, i.e. arg_names_passed
    # if a default value must not count and arg_names_available if any non-NULL
    # value counts.
    # relation: the relation that must hold between the two arguments.

    validate_arg_pair <-
      function(present_arg_names, arg_names, relation, message, type = "error"){

        is_present <- arg_names %in% present_arg_names

        is_valid <-
          base::switch(
            relation,
            # The two arguments exclude each other
            "not_both" = !base::all(is_present),
            # The first argument cannot be used without the second one
            "requires" = !is_present[1] || is_present[2],
            # Either both arguments or none of them
            "both_or_none" = is_present[1] == is_present[2])

        if(!is_valid){

          text <- base::gsub("{arg_1}", arg_names[1], message, fixed = TRUE)
          text <- base::gsub("{arg_2}", arg_names[2], text, fixed = TRUE)

          if(type == "error"){
            base::stop(text, call. = FALSE)
          } else if (type == "warning"){
            base::warning(text, call. = FALSE)
          }
        }
      }


    # If users enter a value for geo_id_macro but not for geo_id_micro
    # the impact cannot be grouped accordingly (multiple geo_id_micro are needed)
    # arg_names_passed in case that there is a default value (safer)
    validate_arg_pair(
      present_arg_names = arg_names_passed,
      arg_names = c("geo_id_macro", "geo_id_micro"),
      relation = "requires",
      message = "If you do not pass a value for {arg_2}, you cannot use {arg_1}.")


    ### error_if_not_numeric #####

    # Find the arguments that should be numeric but are not
    # report = "all" (and not the default "first") because it is expected to
    # review quite a lot of arguments here
    # and it is nice to have all incorrect args at once
    validate_args(
      args = input_args_value,
      arg_names = numeric_args,
      is_valid = function(x){base::is.numeric(x) & !base::is.na(x)},
      message = "The following arguments should be numeric without NAs: {arg}.",
      report = "all")


    ### error_if_not_an_option #####
    # One call per argument (and not one call with all of them) because
    # the options and therefore also the message differ per argument.
    # In this way the users see the options of the argument they got wrong
    # instead of the options of all categorical arguments at once

    for (x in categorical_args) {

      var_options <- options_of_categorical_args[[x]]

      validate_args(
        args = input_args_value,
        arg_names = x,
        # validate_args() applies any(), so this also covers the case that
        # people enter this argument as column with repeated (or multiple) values
        is_valid = function(v){v %in% var_options},
        message =
          base::paste0(
            "For {arg}, please, type (between quotation marks) one of these options: ",
            base::toString(var_options), "."))
    }



    ### error_if_different_length #####

    # Obtain the length of all arguments
    length_args <- purrr::map_vec(input_args_value, base::length)
    # Remove erf_eq lengths because they are not vectors (not to be evaluated)
    length_args <-
      length_args[! base::names(length_args) %in%
                    c("erf_eq_central", "erf_eq_lower", "erf_eq_upper")]

    # If info is a data frame the length is actually the number of rows
    if(base::is.data.frame(input_args_value$info)){
      length_args["info"] <- base::nrow(input_args_value$info)
    }


    # Get length that all arguments should have (apart from 0 or 1)
    relevant_length_args <-
      length_args[base::names(length_args) %in%
                    c("geo_id_micro", "exp_central", "sex", "age_group")]
    # Get length that all arguments should have (apart from 0 or 1)
    # If all relevant lengths are 1, then 1
    if(base::all(relevant_length_args == 1)){
      required_length <- 1
      # Otherwise
    } else {
      # Otherwise, the unique length that is not 1
      required_length <- base::unique(base::setdiff(relevant_length_args, 1))
    }

    # Get the names
    # setdiff() cannot be used here because it drops the names of the vector
    # and they are important here
    names_required_length <-
      base::names(relevant_length_args[relevant_length_args %in% required_length])

    # Get the names of the outliers
    # i.e. args not complying with the required length

    names_not_complying_with_required_length <-
      base::names(length_args[!length_args %in% c(0, 1, required_length)])

    # The length must be 0 (NULL), 1 or the same as required_length
    # If there are multiple different required_length --> error.
    # It must be clarified
    if(base::length(required_length)> 1){
      base::stop(
        base::paste0(
          "Not clear what is the maximal length of your arguments: ",
          base::toString(required_length),
          ". Check: ",
          base::toString(names_required_length),
          "."))
    } else if (base::length(names_not_complying_with_required_length) > 0) {
      # If it clear the unique required_length but there are outliers
      # --> error

      base::stop(
        base::paste0(
          "All function arguments must have the same length (here ",
          required_length,
          ") or length 1. Check: ",
          base::toString(names_not_complying_with_required_length),
          "."))
    }

    ### error_if_ambiguous_allocation #####

    # Some arguments (e.g. bhd_central, rr_central) must have exactly one value
    # for each combination of the id arguments (e.g. geo_id_micro, sex, age_group
    # and the columns of info). Otherwise it is not clear which value has to be
    # allocated to which combination.
    # The callers check first whether the argument and at least one id argument
    # were entered. Otherwise there is nothing to check
    # (and rr_central needs to know it to apply an alternative check).

    error_if_ambiguous_allocation <- function(var_name, id_arg_names){

      # Add info columns as list element for the operation below
      input_args_value_flat <-
        c(input_args_value,
          base::as.list(input_args_value$info))

      arguments_for_combination <-
        base::intersect(
          base::names(input_args_value_flat),
          c(id_arg_names, base::names(input_args_value$info)))

      # Find all ids which were used
      valid_ids <-
        purrr::map_lgl(
          input_args_value_flat[arguments_for_combination],
          ~ base::length(.x) == base::length(input_args_value_flat[[var_name]]))

      # Create data frame with used ids and var_name as cols
      df_id_structure <-
        base::as.data.frame(
          input_args_value_flat[c(var_name,
                                  arguments_for_combination[valid_ids])])

      if(base::nrow(df_id_structure) > 0){

        # Check if every id combination has only one assigned value
        id_ambiguity <- df_id_structure |>
          dplyr::group_by(dplyr::across(!dplyr::all_of(var_name))) |>
          dplyr::summarize(not_same = dplyr::n_distinct(.data[[var_name]]) != 1)

        if(base::any(id_ambiguity$not_same)){
          base::stop(
            base::paste0(
              "Allocation from ", var_name, " to ",
              base::toString(arguments_for_combination[valid_ids]), " is ambiguous.\n",
              "The following combinations have multiple ", var_name, " values: \n",
              base::toString(
                base::do.call(
                  base::paste,
                  c(id_ambiguity[id_ambiguity$not_same, 1:(base::ncol(id_ambiguity) - 1)],
                    sep = "_"))),
              "\n",
              "Within every combination, the ", var_name, " values need to be the same."),
            call. = FALSE)
        }
      }
    }


    bhd_id_arg_names <-
      # geo_id_macro is left out because it does not interact with bhd_central
      c("geo_id_micro", "sex", "age_group")

    rr_id_arg_names <-
      c("geo_id_macro", "geo_id_micro", "sex", "age_group")


    ### error_if_bhd_unique_longer_than_id_unique #####
    # info is in the condition because entering it is also a way of identifying
    # subgroups, but not in id_arg_names because there the COLUMNS of info count
    if(input_args$is_entered_by_user$bhd_central &&
       base::any(base::unlist(
         input_args$is_entered_by_user[c(bhd_id_arg_names, "info")]))){

      error_if_ambiguous_allocation(
        var_name = "bhd_central",
        id_arg_names = bhd_id_arg_names)
    }


    ### error_if_multiple_rr_in_one_exp_category #####
    if(input_args$is_entered_by_user$rr_central &&
       base::any(base::unlist(
         input_args$is_entered_by_user[c(rr_id_arg_names, "info")]))){

      error_if_ambiguous_allocation(
        var_name = "rr_central",
        id_arg_names = rr_id_arg_names)

      # If the allocation cannot be checked (no id argument entered by the user)
      # then rr_central can only have one single value
    } else if (input_args$is_entered_by_user$rr_central &&
               base::length(base::unique(input_args_value$rr_central)) > 1) {

      base::stop(
        "rr_central must be the same for all exposures.",
        call. = FALSE)
    }


    if(is_lifetable){

      ### error_if_not_positive #####

      # No life table arguments currently require values > 0 at validation stage
      validate_args(
        args = input_args_value,
        arg_names = lifetable_args_with_values_above_0,
        is_valid = function(x){x > 0},
        message = "The values in the following arguments must be higher than 0: {arg}.",
        report = "all")

      ### error_if_negative #####

      # Population and baseline health data may be 0 but not negative in life
      # table calculations; structural zero-population cases are handled later
      validate_args(
        args = input_args_value,
        arg_names = lifetable_args_with_values_0_or_above,
        is_valid = function(x){x >= 0},
        message = "The values in the following arguments must be 0 or higher: {arg}.",
        report = "all")


      ### error_if_not_consecutive_sequence #####
      error_if_not_consecutive_sequence <- function(var_name){
        var_value <- input_args_value[[var_name]]
        # Here a function because it expected to use it in one or two arguments
        # (not like e.g. the check of is.numeric)

        if(# Check that values are integers
          base::any(var_value != base::floor(var_value)) &&
          # Check difference between consecutive elements is exactly 1
          base::all(base::diff(var_value))) {

          base::stop(
            base::paste0(var_name, " must be a consecutive sequence of integer values where the difference between elements is 1."),
            call. = FALSE
          )
        }
      }

      error_if_not_consecutive_sequence(var_name = "age_group")

      ### warning if bhd = 0 #####
      # arg_names_passed (and not arg_names_available) because only the bhd_
      # arguments that the users entered themselves are of interest here
      validate_args(
        args = input_args_value,
        arg_names =
          base::intersect(arg_names_passed,
                          c("bhd_central", "bhd_lower", "bhd_upper")),
        is_valid = function(x){x != 0},
        message =
          "Zeros in bhd_ arguments are theoretically possible,
          but they lack conceptual logic,
          because survival probability become 100% in the age group with zero deaths",
        type = "warning")

    }

    ### error_if_erf_eq_not_function_or_string #####
    # If it is a function (single function or multiple functions in a list)
    # and it is not a character
    validate_args(
      args = input_args_value,
      arg_names = base::paste0("erf_eq", ci_suffix),
      is_valid = function(x){base::is.function(x) || base::is.character(x)},
      message = "{arg} must be a function or a character string.")



    ### error_if_lower_than_0 #####

    # Find the arguments with values <0
    validate_args(
      args = input_args_value,
      arg_names = numeric_args,
      is_valid = function(x){x >= 0},
      message = "The values in the following arguments must not be lower than 0: {arg}.",
      report = "all")



    ### error_if_higher_than_1 #####

    validate_args(
      args = input_args_value,
      arg_names = c("prop_pop_exp", "fraction_lived", base::paste0("dw", ci_suffix)),
      is_valid = function(x){x <= 1},
      message = "The values in the following arguments must not be higher than 1: {arg}.",
      report = "all")


    ### error_if_sum_higher_than_1 #####

    # If not all values of prop_pop_exp are 1, then check below
    # Otherwise this step is not excecuted and speed increases
    if(! base::all(input_args_value[["prop_pop_exp"]] == 1)){

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
    }




    ### error_if_not_increasing_lower_central_upper #####

    # Identify the argument names with all CI suffixes (_central, _lower_, _upper)
    arg_names_with_ci <- arg_names_available|>
      base::grep("_central|_lower|_upper", x= _, value = TRUE) |>
      # Remove erf_eq because it is not numeric
      base::setdiff(c("erf_eq_central", "erf_eq_lower", "erf_eq_upper"))

    arg_names_with_ci_prefix <- arg_names_with_ci|>
      base::gsub("_central|_lower|_upper", "", x = _)

    arg_names_with_all_ci_prefix <- arg_names_with_ci_prefix |>
      base::table() |>
      purrr::keep(~ . == 3) |>
      base::names()



    # Check if error if not lower>central>upper.
    # One call per prefix (rr, exp, bhd...) because the message names
    # the three arguments of that prefix
    for (x in arg_names_with_all_ci_prefix) {

      validate_args(
        args = input_args_value,
        arg_names = base::paste0(x, "_central"),
        is_valid = function(v){
          base::all(v >= input_args_value[[base::paste0(x, "_lower")]]) &&
            base::all(v <= input_args_value[[base::paste0(x, "_upper")]])},
        message =
          base::paste0("{arg} must be higher than ", x, "_lower",
                       " and lower than ", x, "_upper."))
    }



    ### error_if_only_lower_or_upper #####
    arg_names_with_two_ci_prefix <- arg_names_with_ci_prefix |>
      base::table() |>
      purrr::keep(~ . == 2) |>
      base::names()

    # Check if lower but not upper (or vice versa).
    # arg_names_available (and not arg_names_passed) because here any
    # non-NULL value counts as entered
    for (x in arg_names_with_two_ci_prefix) {

      validate_arg_pair(
        present_arg_names = arg_names_available,
        arg_names = base::paste0(x, c("_lower", "_upper")),
        relation = "both_or_none",
        message = "Either both, {arg_1} and {arg_2}, or none of them must entered, but not only one.")
    }


    error_if_var_and_risk <- function(var_name, risk){

      # Identify the alternative options
      all_approach_risks <- c("relative_risk", "absolute_risk")
      all_var_names <- c("prop_pop_exp", "pop_exp")
      another_approach_risk <- base::setdiff(all_approach_risks, risk)
      another_var_name <- base::setdiff(all_var_names, var_name)

      if(var_name %in% arg_names_passed &&
         # Use all() for the case of approach_risk entered as vector
         base::all(approach_risk == risk)){
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

    # The erf can be defined either by rr_ (and shape and increment) or by erf_eq_
    for (a in c("rr_central", "erf_shape", "rr_increment")){

      validate_arg_pair(
        present_arg_names = arg_names_passed,
        arg_names = c(a, "erf_eq_central"),
        relation = "not_both",
        message = "The argument {arg_1} cannot be used together with the argument {arg_2} (either one or the other but not both).")
    }



    ## Warnings ########################

    ### warning_if_ar_and_cutoff #####
    warning_if_ar_and_cutoff <- function(var_names){

      # Store var_value
      available_var_values <- input_args_value[var_names] |>
        purrr::discard(base::is.null)
      available_var_names <- base::names(available_var_values)


      if(base::any(approach_risk == "absolute_risk") &
         base::length(available_var_names) > 0 &
         base::any(!base::unlist(available_var_values) == 0)){ # Only if available
        # Create warning message
        base::warning(
          base::paste0(
            "You entered a value for: ", paste(available_var_names, collapse = ", "), " alongside absolute risk.\n",
            "Be aware that healthiar shifts the exposure in 'erf_eq' as c = (exp - cutoff).\n"),
          call. = FALSE)
      }
    }

    # Call function only if absolute risk

    warning_if_ar_and_cutoff(var_names = base::paste0("cutoff", ci_suffix))



    ### warning_if_rr_and_no_var_with_default #####
    warning_if_rr_and_no_var_with_default <- function(var_name, default){

    # For absolute risk no cutoff is used (not relevant)
    if(! var_name %in% arg_names_passed &&
       # Use all() for the case of approach_risk entered as vector
       base::all(approach_risk == "relative_risk")){

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
