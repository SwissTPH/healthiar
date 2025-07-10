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

    if (is_lifetable) {
      # # Calculate totals across sex
      # input_wo_lifetable_totals <- input_wo_lifetable |>
      #   dplyr::summarize(
      #     bhd = base::sum(bhd),
      #     population = bhd::sum(population)
      #   )
      #

      input_table <- input_wo_lifetable |>
        dplyr::mutate(
          # Add approach risk which cannot be entered by the user
          # TODO: To be removed if attribute_health() and attribute_lifetable() are merged
          approach_risk = "relative_risk",
          # Convert age_group to numeric (obligatory in life table approach)
          age_group = base::as.numeric(age_group),
          # Duplicate age_group for life table calculations
          age_start = age_group,
          # Obtain the end age summing one because the function only works with
          # single-year age
          age_end = age_group + 1,
          min_age = if(base::is.null(input_args_edited$min_age)){
            dplyr::first(base::unique(age_start))} else {min_age},
          max_age = if(base::is.null(input_args_edited$max_age)){
            dplyr::last(base::unique(age_start))} else {max_age},
          # Duplicate bhd for life table calculations
          deaths = bhd) |>

        # Nest life tables
        tidyr::nest(
          lifetable_with_pop_nest =
          c(age_group, age_start, age_end, population, bhd, deaths))


    } else {
      # If no lifetable, only use input_wo_lifetable
      input_table <- input_wo_lifetable
    }

    ## Add is_lifetable
    input_table <- input_table |>
      dplyr::mutate(is_lifetable = is_lifetable)


  return(input_table)

  }
