#' Compile input

#' @description
#' This function compiles the input data of the main function and calculates the population attributable fraction based on the input data (all in one data frame)

#' @param input_args
#' \code{List} with all input data by argument
#' @param is_lifetable
#' \code{Boolean} INTERNAL argument specifying if the life table approach is applied (TRUE) or not (FALSE)

#' @returns
#' This function returns a \code{data.frame} with all input data together
#' Moreover, the data frame includes columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }

#' @examples
#' TBD

#' @author Alberto Castro & Axel Luyten

#' @note Experimental function

#' @keywords internal



compile_input <-
  function(input_args, is_lifetable){


    args_for_lifetable <-
      c("approach_exposure", "approach_newborns",
        "year_of_analysis",
        "min_age", "max_age")


    input_args_edited <- input_args$value

    # PROCESS GEO ID ###################################################################
    # geo_ids need to be character because
    # a) no operations are expected
    # b) otherwise error somewhere else in the package when mixing character and numeric
    for (geo_id_ in c("geo_id_disaggregated", "geo_id_aggregated")) {
      if (!is.null(input_args_edited[[geo_id_]])){
        input_args_edited[[geo_id_]] <- base::as.character(input_args_edited[[geo_id_]])
      }
    }

    # PROCESS ERF ######################################################################
    # erf can be defined by one of the following combination of data
    # a) rr, increment, shape and cutoff
    # b) an erf function as string
    # c) an erf function as function
    # If a function is entered (case c), it must be encapsulated in a list
    # to make the code work below

    for (erf_eq_ in c("erf_eq_central", "erf_eq_lower", "erf_eq_upper")) {
      if (!is.null(input_args_edited[[erf_eq_]]) &&
          base::is.function(input_args_edited[[erf_eq_]])) {
        input_args_edited[[erf_eq_]] <- list(input_args_edited[[erf_eq_]])
      }
    }

    # Remove list elements that are null,
    # otherwise the list cannot be converted into a tibble
    input_args_edited <-
      purrr::discard(input_args_edited , is.null)

    # ARGUMENTS ################################################################
    # Store all arguments excluding those for life table
    # (done below only if life table approach)
    # Use input_args_edited as basis because of the required edits

    input_wo_lifetable <- input_args_edited |>
      # Remove arguments for life table and info.
      # Info is to added later with a function add_info()
      # because it can be a data frame.
      purrr::discard(names(input_args_edited) %in% c("info")) |>
      # Convert into a tibble
      tibble:::as_tibble() |>
      # Add info
      healthiar:::add_info(info = input_args_edited$info)|>
      # Keep only unique rows
      # (they can be duplicated in case of multiple geo units, exposure categories..)
      base::unique()

    # Obtain the exposure dimension and exposure type in a separate table
    exp_dimension_table <-
      input_wo_lifetable |>
      dplyr::select(dplyr::any_of(c("geo_id_disaggregated", "age_group", "sex", "exp_central"))) |>
      # Add population_midyear_male (it could be any of the other life table arguments)
      # Add it in a separated mutate because
      # if it is NULL then it is not added
      dplyr::group_by(dplyr::across(dplyr::any_of(c("geo_id_disaggregated", "age_group", "sex"))))|>
      dplyr::mutate(exposure_dimension = 1 : base::length(exp_central),
                       exposure_type =
                         base::ifelse(length(exp_central) == 1,
                                      "population_weighted_mean",
                                      "exposure_distribution"))

    # Join with exposure dimension and type
    input_wo_lifetable <-
      dplyr::left_join(
        input_wo_lifetable,
        exp_dimension_table,
        by = base::intersect(c("geo_id_disaggregated", "age_group", "sex", "exp_central"),
                             base::names(input_wo_lifetable)))

    # PIVOT LONGER ###########################################################
    # I.e. increase nr of rows to show all combinations of
    # central, lower and upper estimates (relevant for iteration)

    # Identify the variable to pivot longer
    vars_to_pivot <-
      c("exp", "bhd", "cutoff", "duration", "dw")

    # Loop over the variables pivoting longer
    for (var in vars_to_pivot) {
      if (base::paste0(var, "_central") %in% base::names(input_wo_lifetable)){

        input_wo_lifetable <-
          tidyr::pivot_longer(
            data = input_wo_lifetable,
            cols = dplyr::any_of(base::paste0(var, c("_central", "_lower", "_upper"))),
            names_to = paste0(var, "_ci"),
            names_prefix = paste0(var, "_"),
            values_to = var)
      }
    }

    if ("rr_central" %in% base::names(input_wo_lifetable)) {
      # Out of the loop for exposure response function
      # because both rr_ and erf_eq_ ends with a variable erf_ci

      input_wo_lifetable <-
        tidyr::pivot_longer(data = input_wo_lifetable,
                            cols = dplyr::any_of(c("rr_central", "rr_lower", "rr_upper")),
                            names_to = "erf_ci",
                            names_prefix = "rr_",
                            values_to = "rr")
    } else if ("erf_eq_central" %in% base::names(input_wo_lifetable)) {

      input_wo_lifetable <-
        tidyr::pivot_longer(data = input_wo_lifetable,
                            cols = dplyr::starts_with("erf_eq_"),
                            names_to = "erf_ci",
                            names_prefix = "erf_eq_",
                            values_to = "erf_eq")
    }

    # CREATE LIFE TABLES ##########################################################
    # As nested tibble
#
#     if (is_lifetable) {
#
#
#       # Build the data set for lifetable-related data
#       # The life table has to be provided (by sex)
#       # Rename column names to standard names
#
#       # Define variables to be used below
#       age_sequence_start <- base::as.numeric(input_args_edited$age_group)
#       age_sequence_end <- age_sequence_start + 1
#       # Define the arguments for life table excluding those that are sex-specific
#       non_age_specific_lifetable_args <-
#         args_for_lifetable[!grepl("male|female", args_for_lifetable)]
#       # Now including those arguments that are sex-specific
#       age_specific_lifetable_args <-
#         args_for_lifetable[grepl("male|female", args_for_lifetable)]
#
#       # Store arguments for life table
#       # Use input_args_edited as basis because of the required edits
#       non_age_specific_input_for_lifetable <- input_args_edited |>
#         # Also keep geo_id_disaggragated to enable join below
#         purrr::keep(base::names(input_args_edited) %in%
#                       c(non_age_specific_lifetable_args, "geo_id_disaggregated")) |>
#         # Convert into a tibble
#         tibble:::as_tibble() |>
#         # If min_age or max_age are NULL,
#         # then use value of first_age_pop and last_age_pop resp.
#         dplyr::mutate(
#           year_of_analysis = input_args_edited$year_of_analysis,
#           min_age = if(base::is.null(input_args_edited$min_age)){
#             input_args_edited$first_age_pop} else {min_age},
#           max_age = if(base::is.null(input_args_edited$max_age)){
#             input_args_edited$last_age_pop} else {max_age}) |>
#         # Keep only unique rows
#         # (they can be duplicated in case of multiple geo units, exposure categories..)
#         base::unique()
#
#       age_specific_input_for_lifetable  <- input_args_edited |>
#         # Also keep geo_id_disaggragated to enable join below
#         purrr::keep(base::names(input_args_edited) %in%
#                       c(age_specific_lifetable_args, "geo_id_disaggregated")) |>
#         # Convert into a tibble
#         tibble:::as_tibble() |>
#         # Keep only unique rows
#         # (they can be duplicated in case of multiple geo units, exposure categories..)
#         base::unique() |>
#         # Add age columns to age specific data
#         dplyr::mutate(
#           age = base::rep(age_sequence_start, length.out = dplyr::n()),
#           age_end = base::rep(age_sequence_end, length.out = dplyr::n())) |>
#         # Rename population to make it shorter
#         dplyr::rename_with(~ base::gsub("population_midyear_", "population_", .x))
#
#       # Pivot longer sex
#       # Define first the life table (age-specific) data based on the input to loop
#       lifetable_with_pop <- age_specific_input_for_lifetable |>
#         # Calculate totals
#         dplyr::mutate(
#           deaths_total = deaths_male + deaths_female,
#           population_total = population_male + population_female)
#
#
#       # First pivot longer
#       lifetable_with_pop <-
#         tidyr::pivot_longer(
#         data = lifetable_with_pop,
#         cols = dplyr::all_of(
#           c(base::paste0("deaths_", c("male", "female", "total")),
#             base::paste0("population_", c("male", "female", "total")))),
#         names_sep = "_",
#         names_to = c("var", "sex"),
#         values_to = "value") |>
#         # And second pivot wider to keep the variables as columns
#         tidyr::pivot_wider(
#           names_from = var,
#           values_from = value) |>
#         # Add total population by geo unit
#         # For impacts per 100k inhab.
#         dplyr::group_by(geo_id_disaggregated) |>
#         dplyr::mutate(population_total = sum(population, rm.na = TRUE)) |>
#         # Next age-specific information to get the table smaller
#         tidyr::nest(
#           lifetable_with_pop_nest =
#             c(age, age_end, population, deaths)) |>
#         # Rename population_total to population
#         # now that age-specific information is nested it is already clear
#         dplyr::rename("population" = "population_total")
#
#
#       # JOIN TIBBLES ###########################################################
#
#         # Calculate total population for impacts per 100k inhab.
#         # population <-
#         #   lifetable_with_pop_total |>
#         #   dplyr::group_by(geo_id_disaggregated) |>
#         #   dplyr::summarize(population = sum(population, rm.na = TRUE))
#
#       # Join the input without and with lifetable variable into one tibble
#       input_table <-
#         dplyr::left_join(input_wo_lifetable,
#                          non_age_specific_input_for_lifetable,
#                          by = "geo_id_disaggregated",
#                          relationship = "many-to-many") |>
#         dplyr::left_join(lifetable_with_pop,
#                          by = "geo_id_disaggregated",
#                          relationship = "many-to-many")
#       # Add approach_risk
#       # This is not in input_args for life table because
#       # it is first defined in attribute_master()
#       # not an option in attribute_lifetable()
#       input_table <- input_table |>
#         dplyr::mutate(approach_risk = "relative_risk")
#
#
#
#       } else {
#       # If no lifetable, only use input_wo_lifetable
#       input_table <- input_wo_lifetable}


    ## Add is_lifetable
    input_table <- input_wo_lifetable |>
      dplyr::mutate(is_lifetable = is_lifetable,
                    approach_risk = "relative_risk")



  return(input_table)

  }
