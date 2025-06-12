#' Compile input

#' @description
#' This function compiles the input data of the main function and calculates the population attributable fraction based on the input data (all in one data frame)

#' @inheritParams attribute_master

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
  function(input_args){


    args_for_lifetable <-
      c("approach_exposure", "approach_newborns",
        "first_age_pop", "last_age_pop",
        "population_midyear_male", "population_midyear_female",
        "deaths_male", "deaths_female",
        "year_of_analysis",
        "min_age", "max_age")


    input_args_edited <- input_args

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
          base::is.function(input_args[[erf_eq_]])) {
        input_args_edited[[erf_eq_]] <- list(input_args_edited[[erf_eq_]])
      }
    }

    # ARGUMENTS ################################################################
    # Store all arguments excluding those for life table
    # (done below only if life table approach)
    # Use input_args_edited as basis because of the required edits
    input_wo_lifetable <- input_args_edited |>
      # Remove arguments for life table and info.
      # Info is to added later with a function add_info()
      # because it can be a data frame.
      purrr::discard(names(input_args_edited) %in% c(args_for_lifetable, "info")) |>
      # Remove list elements that are null,
      # otherwise the list cannot be converted into a tibble
      purrr::discard(is.null) |>
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
      dplyr::select(geo_id_disaggregated, exp_central) |>
      # Add population_midyear_male (it could be any of the other life table arguments)
      # Add it in a separated mutate because
      # if it is NULL then it is not added
      dplyr::group_by(dplyr::across(dplyr::any_of(c("geo_id_disaggregated"))))|>
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
        by = c("geo_id_disaggregated", "exp_central"))


      # PIVOT LONGER ###########################################################
      # I.e. increase nr of rows to show all combinations of
      # central, lower and upper estimates (relevant for iteration)

      ## For exposure,
    input_wo_lifetable <-
      input_wo_lifetable |>
      tidyr::pivot_longer(cols = dplyr::starts_with("exp_"),
                          names_to = "exp_ci",
                          names_prefix = "exp_",
                          values_to = "exp")

    if ("rr_central" %in% base::names(input_wo_lifetable)) {
      ## Exposure response function

      input_wo_lifetable <-
        tidyr::pivot_longer(data = input_wo_lifetable,
                            cols = dplyr::any_of(c("rr_central", "rr_lower", "rr_upper")),
                            names_to = "erf_ci",
                            names_prefix = "rr_",
                            values_to = "rr")
    } else {

      input_wo_lifetable <-
        tidyr::pivot_longer(data = input_wo_lifetable,
                            cols = dplyr::starts_with("erf_eq_"),
                            names_to = "erf_ci",
                            names_prefix = "erf_eq_",
                            values_to = "erf_eq")
    }


    ## Baseline health data
    if("bhd_central" %in% names(input_wo_lifetable)) {
      input_wo_lifetable <-
        input_wo_lifetable |>
        tidyr::pivot_longer(cols = dplyr::starts_with("bhd_"),
                            names_to = "bhd_ci",
                            names_prefix = "bhd_",
                            values_to = "bhd")}

    ## Cutoff health data
    if("cutoff_central" %in% names(input_wo_lifetable)) {
      input_wo_lifetable <-
        input_wo_lifetable |>
        tidyr::pivot_longer(cols = dplyr::starts_with("cutoff_"),
                            names_to = "cutoff_ci",
                            names_prefix = "cutoff_",
                            values_to = "cutoff")}


    ## Disability weight
    if ("dw_central" %in% names(input_wo_lifetable)) {
      input_wo_lifetable <-
        input_wo_lifetable |>
        tidyr::pivot_longer(cols = dplyr::starts_with("dw_"),
                            names_to = "dw_ci",
                            names_prefix = "dw_",
                            values_to = "dw")}

    ## Duration (of morbidity health outcome)
    if ("duration_central" %in% names(input_wo_lifetable)) {
      input_wo_lifetable <-
        input_wo_lifetable |>
        tidyr::pivot_longer(cols = dplyr::starts_with("duration_"),
                            names_to = "duration_ci",
                            names_prefix = "duration_",
                            values_to = "duration")}


    # CREATE LIFE TABLES ##########################################################
    # As nested tibble

    if (input_args$is_lifetable) {

      # Define the arguments for life table excluding those that are sex-specific
      args_for_lifetable_wo_sex <-
        args_for_lifetable[!grepl("male|female", args_for_lifetable)]

      # Store arguments for life table
      # Use input_args_edited as basis because of the required edits
      input_for_lifetable  <- input_args_edited |>
        # Also keep geo_id_disaggragated to enable join below
        purrr::keep(names(input_args_edited) %in% c(args_for_lifetable_wo_sex, "geo_id_disaggregated")) |>
        # Remove list elements that are null,
        # otherwise the list cannot be converted into a tibble
        purrr::discard(is.null) |>
        # Convert into a tibble
        tibble:::as_tibble() |>
        # If min_age or max_age are NULL,
        # then use value of first_age_pop and last_age_pop resp.
        dplyr::mutate(
          year_of_analysis = input_args_edited$year_of_analysis,
          min_age = if(base::is.null(input_args_edited$min_age)){
            input_args_edited$first_age_pop} else {min_age},
          max_age = if(base::is.null(input_args_edited$max_age)){
            input_args_edited$last_age_pop} else {max_age}) |>
        # Keep only unique rows
        # (they can be duplicated in case of multiple geo units, exposure categories..)
        base::unique()
      # |>
      #   dplyr::mutate(
      #     age = age_sequence,
      #     age_end = age_end_sequence)


      # Build the data set for lifetable-related data
      # The life table has to be provided (by sex)
      # Rename column names to standard names

      # Define variables to be used below
      age_sequence <- seq(from = input_args_edited$first_age_pop,
                          to = input_args_edited$last_age_pop,
                          by = 1)
      age_end_sequence <- seq(from = input_args_edited$first_age_pop + 1,
                              to = input_args_edited$last_age_pop + 1,
                              by = 1)



      # Create a template of the lifetables
      # Use crossing to enable all combination of the vectors
      lifetable_with_pop_template <-
        tidyr::crossing(
          geo_id_disaggregated = input_args_edited$geo_id_disaggregated,
          age = age_sequence) |>
        # Add age end with mutate instead of crossing
        # because no additional combinations are needed (already included in age)
        dplyr::mutate(
          age_end = base::rep(age_end_sequence, length.out = dplyr::n()))




      # Based on the template create lifetable for male
      lifetable_with_pop_male <-
        lifetable_with_pop_template[,c("geo_id_disaggregated", "age", "age_end")] |>
        dplyr::mutate(
          sex = "male",
          deaths = input_args_edited$deaths_male,
          population = input_args_edited$population_midyear_male)

      # The same for female
      lifetable_with_pop_female <-
        lifetable_with_pop_template[,c("geo_id_disaggregated", "age", "age_end")] |>
        dplyr::mutate(
          sex = "female",
          deaths = input_args_edited$deaths_female,
          population = input_args_edited$population_midyear_female)

      lifetable_with_pop_male_female <-
        # Bind male and female tibbles
        dplyr::bind_rows(
          lifetable_with_pop_male,
          lifetable_with_pop_female)

      lifetable_with_pop <-
        lifetable_with_pop_male_female |>
        # Nest the lifetable elements
        tidyr::nest(
          lifetable_with_pop_nest =
            c(age, age_end, population))

        # The same for total
        lifetable_with_pop_total <-
          lifetable_with_pop_template[,c("geo_id_disaggregated", "age", "age_end")] |>
          dplyr::mutate(
            sex = "total",
            population = lifetable_with_pop_male$population + lifetable_with_pop_female$population,
            deaths = lifetable_with_pop_male$deaths + lifetable_with_pop_female$deaths)

        lifetable_with_pop_male_female_total <-
          dplyr::bind_rows(lifetable_with_pop_male,
                           lifetable_with_pop_female,
                           lifetable_with_pop_total)

        lifetable_with_pop <-
          lifetable_with_pop_male_female_total |>
          # Nest the lifetable elements
          tidyr::nest(
            lifetable_with_pop_nest =
              c(age, age_end, population, deaths))


      # }


      # JOIN TIBBLES ###########################################################

        # Calculate total population for impacts per 100k inhab.
        population <-
          lifetable_with_pop_total |>
          dplyr::group_by(geo_id_disaggregated) |>
          dplyr::summarize(population = sum(population, rm.na = TRUE))

        # Join the input without and with lifetable variable into one tibble
        input_table <-
          dplyr::left_join(input_wo_lifetable,
                           input_for_lifetable,
                           by = "geo_id_disaggregated",
                           relationship = "many-to-many") |>
          dplyr::left_join(lifetable_with_pop,
                           by = "geo_id_disaggregated",
                           relationship = "many-to-many") |>
          dplyr::left_join(population,
                           by = "geo_id_disaggregated",
                           relationship = "many-to-many")

      } else {
      # If no lifetable, only use input_wo_lifetable
      input_table <- input_wo_lifetable}



  return(input_table)

  }
