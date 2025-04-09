#' Compile input

#' @description Compiles the input data of the main function and calculates the population attributable fraction based on the input data (all in one data frame)
#' @inheritParams attribute_master
#'
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
#' @author Alberto Castro
#' @note Experimental function
#' @keywords internal

compile_input <-
  function(is_lifetable = NULL,
           approach_risk = NULL,
           exp_central, exp_lower = NULL, exp_upper = NULL,
           pop_exp = NULL,
           prop_pop_exp = NULL,
           cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
           rr_central, rr_lower = NULL, rr_upper = NULL,
           rr_increment = NULL,
           erf_shape = NULL,
           erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
           bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
           min_age = NULL,
           max_age = NULL,
           geo_id_disaggregated = NULL,
           geo_id_aggregated = NULL,
           info = NULL,
           population = population,
           # YLD
           duration_central = NULL, duration_lower = NULL, duration_upper = NULL,
           dw_central = NULL, dw_lower = NULL, dw_upper = NULL,
           # Lifetable data
           health_outcome = NULL,
           approach_exposure = NULL,
           approach_newborns = NULL,
           year_of_analysis = NULL,
           time_horizon = NULL,
           first_age_pop = NULL, last_age_pop = NULL,
           population_midyear_male = NULL, population_midyear_female = NULL,
           deaths_male = NULL, deaths_female = NULL){

    # Check input data
    # stopifnot(exprs = {
    #   length(exp) == length(prop_pop_exp)
    #   is.null(min_age) == FALSE
    #   is.null(max_age) == FALSE
    # })



    # Otherwise default value of prop_pop_exp (1) is wrongly assumed for exposure distribution.


    # PROCESS GEO ID ###################################################################
    # If no geo_id_disaggregated is provided (if is NULL) then assign some value.
    ## geo_id_disaggregated is needed to group results in case of multiple geo_ids
    if(is.null(geo_id_disaggregated)){
      geo_id_disaggregated <-
        ifelse(is.list({{exp_central}}), 1:length({{exp_central}}), 1) |>
        as.character()
    } else {
      # geo_ids need to be character because
      # a) no operations are expected
      # b) otherwise error somewhere else in the package when mixing character and numeric
      geo_id_disaggregated <- as.character(geo_id_disaggregated)
    }

    if(!is.null(geo_id_aggregated)){
      # For geo_id_aggegated there is no default value as for geo_id_aggregated
      # so it needs the if statement below.
      # Otherwise, instead of not including the column when NULL
      # no the tibble becomes empty and then error somewhere else
      geo_id_aggregated <- as.character(geo_id_aggregated)
    }

    # PROCESS ERF ######################################################################

    # If the erf is defined by rr, increment, shape and cutoff
    if(is.null(erf_eq_central)){

      erf_data <- # 1 x 6 tibble
        dplyr::tibble(
          rr_increment = rr_increment,
          erf_shape = erf_shape,
          rr_central = rr_central,
          rr_lower =  rr_lower,
          rr_upper = rr_upper)
    }

    # If it is defined by the erf function as string
    if(!is.null(erf_eq_central) & is.character(erf_eq_central)){

        erf_data <- # 1 x 3 tibble
          dplyr::tibble(
            erf_eq_central = erf_eq_central,
            erf_eq_lower = erf_eq_lower,
            erf_eq_upper = erf_eq_upper)
    }

    # If it is defined by the erf function as function
    if(!is.null(erf_eq_central) & is.function(erf_eq_central)){

        erf_data <- # 1 x 3 tibble
          dplyr::tibble(
            ## Functions can't be saved raw in column -> save as list
            erf_eq_central = list(erf_eq_central))
        }

    # If a confidence interval for the erf is provided, add the erf columns
    if (!is.null(erf_eq_lower) & !is.null(erf_eq_upper) & is.function(erf_eq_central)){
      erf_data <-
        erf_data |>
         dplyr::mutate(
           erf_eq_lower = list(erf_eq_lower),
           erf_eq_upper = list(erf_eq_upper))}






    # ARGUMENTS ################################################################

    # Store the length of the exposure argument (to be used below)
    length_exp_dist <-
      ifelse(is.list(exp_central),
             length(exp_central[[1]]), # If exposure distribution
             length(exp_central))      # If single exposure

    length_exp_list <-
      ifelse(is.list(exp_central),
             length(exp_central),      # If multiple geo units
             1)                        # If only one geo unit

    input_wo_lifetable <-
      # Tibble removed columns that are NULL.
      # So if variable is NULL, column not initiated
      dplyr::tibble(
        ## First compile input data that are geo-dependent
        ## Use rep() match dimensions
        geo_id_disaggregated = base::rep(geo_id_disaggregated,
                                   each = length_exp_dist),
        geo_id_aggregated = base::rep(geo_id_aggregated,
                                each = length_exp_dist),
        bhd_central = base::rep(unlist(bhd_central), each = length_exp_dist),
        bhd_lower = base::rep(unlist(bhd_lower), each = length_exp_dist),
        bhd_upper = base::rep(unlist(bhd_upper), each = length_exp_dist),
        # min_age = rep(min_age, each = length_exp_dist),
        # max_age = rep(max_age, each = length_exp_dist),
        # approach_exposure = rep(approach_exposure, each = length_exp_dist),
        # approach_newborns = rep(approach_newborns, each = length_exp_dist),
        population = base::rep(unlist(population), each = length_exp_dist),

        ## Second those variables with length = 1 (non-problematic)
        cutoff_central = cutoff_central,
        cutoff_lower = cutoff_lower,
        cutoff_upper = cutoff_upper,
        approach_exposure = approach_exposure,
        approach_newborns = approach_newborns,
        is_lifetable = is_lifetable,
        year_of_analysis = year_of_analysis,
        time_horizon = time_horizon,
        min_age = ifelse(
          test = !is.null( min_age ),
          yes = min_age,
          no = ifelse(
            test = ( is.null ( min_age ) & {{is_lifetable}} ),
            yes = first_age_pop,
            # no = NULL)
          no = NA)
      ),
        max_age = ifelse(
          test = !is.null( max_age ),
          yes = max_age,
          no = ifelse(
            test = ( is.null ( max_age ) & {{is_lifetable}} ),
            yes = last_age_pop,
            # no = NULL)
          no = NA)),
      duration_central = duration_central,
      duration_lower = duration_lower,
      duration_upper = duration_upper,
      dw_central = dw_central,
      dw_lower = dw_lower,
      dw_upper = dw_upper,

      ## Finally, those variables that are multi-dimensional (exposure distribution)
      exp_central = unlist(exp_central),
      exp_lower = unlist(exp_lower),
      exp_upper = unlist(exp_upper),
      pop_exp = unlist(pop_exp),
      prop_pop_exp = unlist(prop_pop_exp)) |>

      ## Remove min_age & max_age columns if they are NA
      dplyr::select(-where(~ all(is.na(x = .))))

      # Add erf data
    input_wo_lifetable <-
      dplyr::bind_cols(input_wo_lifetable, erf_data) |>
      # Add additional (meta-)information
      healthiar:::add_info(info = info) |>
      # Information derived from input data
      dplyr::mutate(
        approach_risk = approach_risk, # RR or AR
        health_outcome = health_outcome,
        exposure_dimension =
          base::rep(1:length_exp_dist, length_exp_list),
        exposure_type =
          base::ifelse(length_exp_dist == 1,
                 "population_weighted_mean",
                 "exposure_distribution")) |>
      # Remove all columns with all values being NA
      # dplyr::select(where(~ !all(is.na(.)))) |>

      # Add lifetable-related data as nested tibble
      # Build the data set
      # The life table has to be provided (by sex)
      # Rename column names to standard names


      # PIVOT LONGER ###########################################################
      # I.e. increase nr of rows to show all combinations of
      # central, lower and upper estimates (relevant for iteration)

      ## For exposure,
      tidyr::pivot_longer(cols = starts_with("exp_"),
                          names_to = "exp_ci",
                          names_prefix = "exp_",
                          values_to = "exp")

    if (is.null(erf_eq_central)) {
      ## Exposure response function

      input_wo_lifetable <-
        tidyr::pivot_longer(data = input_wo_lifetable,
                            cols = any_of(c("rr_central", "rr_lower", "rr_upper")),
                            names_to = "erf_ci",
                            names_prefix = "rr_",
                            values_to = "rr")
    } else {
      input_wo_lifetable <-
        tidyr::pivot_longer(data = input_wo_lifetable,
                            cols = starts_with("erf_eq_"),
                            names_to = "erf_ci",
                            names_prefix = "erf_eq_",
                            values_to = "erf_eq") }

    ## Baseline health data
    if(!is.null(bhd_central)) {
      input_wo_lifetable <-
        input_wo_lifetable |>
        tidyr::pivot_longer(cols = starts_with("bhd_"),
                            names_to = "bhd_ci",
                            names_prefix = "bhd_",
                            values_to = "bhd")}

    ## Cutoff health data
    if(!is.null(cutoff_central)) {
      input_wo_lifetable <-
        input_wo_lifetable |>
        tidyr::pivot_longer(cols = starts_with("cutoff_"),
                            names_to = "cutoff_ci",
                            names_prefix = "cutoff_",
                            values_to = "cutoff")}


    ## Disability weight
    if (!is.null(dw_central)) {
      input_wo_lifetable <-
        input_wo_lifetable |>
        tidyr::pivot_longer(cols = starts_with("dw_"),
                            names_to = "dw_ci",
                            names_prefix = "dw_",
                            values_to = "dw")}

    ## Duration (of morbidity health outcome)
    if (!is.null(duration_central)) {
      input_wo_lifetable <-
        input_wo_lifetable |>
        tidyr::pivot_longer(cols = starts_with("duration_"),
                            names_to = "duration_ci",
                            names_prefix = "duration_",
                            values_to = "duration")}


    # CREATE LIFETABLES ##########################################################
    # As nested tibble

    if (is_lifetable) {

      # Build the data set for lifetable-related data
      # The life table has to be provided (by sex)
      # Rename column names to standard names

      # Define variables to be used below
      age_sequence <- seq(from = first_age_pop,
                          to = last_age_pop,
                          by = 1)
      age_end_sequence <- seq(from = first_age_pop + 1,
                              to = last_age_pop + 1,
                              by = 1)



      # Create a template of the lifetables
      # Use crossing to enable all combination of the vectors
      lifetable_with_pop_template <-
        tidyr::crossing(
          geo_id_disaggregated,
          age = age_sequence) |>
        # Add age end with mutate instead of crossing
        # because no additional combinations are needed (already included in age)
        # The function rep(, length.out=)
        # is needed to ensure that the vector length matches with number of rows of the tibble.
        dplyr::mutate(
          age_end = base::rep(age_end_sequence, length.out = dplyr::n()))

      # Based on the template create lifetable for male
      lifetable_with_pop_male <-
        lifetable_with_pop_template |>
        dplyr::mutate(
          sex = "male",
          deaths = base::rep(unlist(deaths_male), length.out = dplyr::n()),
          population = base::rep(unlist(population_midyear_male), length.out = dplyr::n()))

      # The same for female
      lifetable_with_pop_female <-
        lifetable_with_pop_template |>
        dplyr::mutate(
          sex = "female",
          deaths = base::rep(unlist(deaths_female), length.out = dplyr::n()),
          population = base::rep(unlist(population_midyear_female), length.out = dplyr::n()))

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
          lifetable_with_pop_template |>
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
                           lifetable_with_pop,
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
