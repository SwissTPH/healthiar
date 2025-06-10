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

    is_lifetable <- input_args$is_lifetable
    approach_risk <- input_args$approach_risk
    rr_central <- input_args$rr_central
    rr_lower <- input_args$rr_lower
    rr_upper <- input_args$rr_upper
    rr_increment <- input_args$rr_increment
    erf_shape <- input_args$erf_shape
    erf_eq_central <- input_args$erf_eq_central
    erf_eq_lower <- input_args$erf_eq_lower
    erf_eq_upper <- input_args$erf_eq_upper
    pop_exp <- input_args$pop_exp
    prop_pop_exp <- input_args$prop_pop_exp
    exp_central <- input_args$exp_central
    exp_lower <- input_args$exp_lower
    exp_upper <- input_args$exp_upper
    cutoff_central <- input_args$cutoff_central
    cutoff_lower <- input_args$cutoff_lower
    cutoff_upper <- input_args$cutoff_upper
    bhd_central <- input_args$bhd_central
    bhd_lower <- input_args$bhd_lower
    bhd_upper <- input_args$bhd_upper
    geo_id_disaggregated <- input_args$geo_id_disaggregated
    geo_id_aggregated <- input_args$geo_id_aggregated
    health_outcome <- input_args$health_outcome
    population_midyear_male <- input_args$population_midyear_male
    population_midyear_female <- input_args$population_midyear_female
    deaths_male <- input_args$deaths_male
    deaths_female <- input_args$deaths_female
    first_age_pop <- input_args$first_age_pop
    last_age_pop <- input_args$last_age_pop
    min_age <- input_args$min_age
    max_age <- input_args$max_age
    year_of_analysis <- input_args$year_of_analysis
    approach_newborns <- input_args$approach_newborns
    time_horizon <- input_args$time_horizon
    dw_central <- input_args$dw_central
    dw_lower <- input_args$dw_lower
    dw_upper <- input_args$dw_upper
    duration_central <- input_args$duration_central
    duration_lower <- input_args$duration_lower
    duration_upper <- input_args$duration_upper
    approach_exposure <- input_args$approach_exposure
    population <- input_args$population
    info <- input_args$info

    # Alternatives that do not in summarize_uncertainty
    # probably because of the environment

    #base::list2env(input_args, envir = base::environment())

    # for (a in names(input_args)) {
    #   base::assign(a, input_args[[a]], envir = base::environment())
    # }

    # for (a in names(input_args)) {
    #   base::eval( base::bquote(.(base::as.name(a)) <- input_args[[.(a)]]),
    #               envir = base::environment())
    # }



    # PROCESS GEO ID ###################################################################
    # If no geo_id_disaggregated is provided (if is NULL) then
    # single case (only 1 geo unit) is assumed
    # because an id is always needed to link information
    if(is.null(geo_id_disaggregated)){
      geo_id_disaggregated <- base::as.character(1)
    } else {
      # geo_ids need to be character because
      # a) no operations are expected
      # b) otherwise error somewhere else in the package when mixing character and numeric
      geo_id_disaggregated <- base::as.character(geo_id_disaggregated)
    }

    if(!is.null(geo_id_aggregated)){
      # geo_id_aggegated there is no default value as for geo_id_aggregated
      # so it needs the if statement below.
      # Otherwise, instead of not including the column when NULL
      # no the tibble becomes empty and then error somewhere else
      geo_id_aggregated <- as.character(geo_id_aggregated)
    }

    # PROCESS ERF ######################################################################

    # If the erf is defined by rr, increment, shape and cutoff
    if(is.null(erf_eq_central)){

      erf_data <- # 1 x 6 tibble
        tibble::tibble(
          rr_increment = rr_increment,
          erf_shape = erf_shape,
          rr_central = rr_central,
          rr_lower =  rr_lower,
          rr_upper = rr_upper)
    }

    # If it is defined by the erf function as string
    if(!is.null(erf_eq_central) & is.character(erf_eq_central)){

        erf_data <- # 1 x 3 tibble
          tibble::tibble(
            erf_eq_central = erf_eq_central,
            erf_eq_lower = erf_eq_lower,
            erf_eq_upper = erf_eq_upper)
    }

    # If it is defined by the erf function as function
    if(!is.null(erf_eq_central) & is.function(erf_eq_central)){

        erf_data <- # 1 x 3 tibble
          tibble::tibble(
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
      tibble::tibble(
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
      dplyr::select(-dplyr::where(~ all(is.na(x = .))))

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
      tidyr::pivot_longer(cols = dplyr::starts_with("exp_"),
                          names_to = "exp_ci",
                          names_prefix = "exp_",
                          values_to = "exp")

    if (is.null(erf_eq_central)) {
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
                            values_to = "erf_eq") }

    ## Baseline health data
    if(!is.null(bhd_central)) {
      input_wo_lifetable <-
        input_wo_lifetable |>
        tidyr::pivot_longer(cols = dplyr::starts_with("bhd_"),
                            names_to = "bhd_ci",
                            names_prefix = "bhd_",
                            values_to = "bhd")}

    ## Cutoff health data
    if(!is.null(cutoff_central)) {
      input_wo_lifetable <-
        input_wo_lifetable |>
        tidyr::pivot_longer(cols = dplyr::starts_with("cutoff_"),
                            names_to = "cutoff_ci",
                            names_prefix = "cutoff_",
                            values_to = "cutoff")}


    ## Disability weight
    if (!is.null(dw_central)) {
      input_wo_lifetable <-
        input_wo_lifetable |>
        tidyr::pivot_longer(cols = dplyr::starts_with("dw_"),
                            names_to = "dw_ci",
                            names_prefix = "dw_",
                            values_to = "dw")}

    ## Duration (of morbidity health outcome)
    if (!is.null(duration_central)) {
      input_wo_lifetable <-
        input_wo_lifetable |>
        tidyr::pivot_longer(cols = dplyr::starts_with("duration_"),
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
