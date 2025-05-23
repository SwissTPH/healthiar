#' Consider socio-economic aspects in healthiar assessments

#' @description
#' This function considers socio-economic aspects (e.g. multiple deprivation index) in the attributable health impacts. If nothing is entered in the argument \code{listed_output_healthiar}, it is assumed that all data come from a table and the argument refer to the columns of that table.

#' @inheritParams attribute_master
#' @param listed_output_healthiar \code{List} containing \code{sub-lists} with the results of \code{healthiar::attribute_health()} for each age group. Each list element should refer to one specific age group.
#' @param impact \code{Numeric vector} containing the attributable health impacts by both age group and geo id.
#' @param population \code{Integer vector} containing the population by age group and geographic unit.
#' @param age_group \code{String vector} with the age groups included in the age standardization. Each vector element refers to each of the list elements of \code{listed_output_healthiar}.
#' @param ref_prop_pop \code{Numeric vector} with the reference proportion of population for each age group. If this argument is empty, the proportion of \code{population} by age group in the provided data will be used.
#' @param social_indicator \code{Numeric vector} showing the social indicator used for the analysis, e.g. a deprivation score (indicator of economic wealth) for each geographic unit. Based on this and \code{n_quantile}, \code{social_quantile} will be calculated.
#' @param n_quantile \code{Integer value} specifying to the number of quantiles in the analysis.
#' @param social_quantile \code{Numeric vector} showing the values from 1 to the number of quantiles assigned to each geographic unit. Either enter \code{social_indicator} and \code{n_quantile} or \code{social_quantile}
#' @param increasing_deprivation \code{Boolean} variable (TRUE/FALSE) that specifies if deprivation is higher when the \code{social_indicator} is higher (TRUE) or lower (FALSE). Default value = TRUE.



#' @returns Returns the impact (absolute and relative) theoretically attributable to the difference in the social indicator (e.g. degree of deprivation) between the quantiles.

#' @author
#' TODO

#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)

#' @export



socialize <- function(listed_output_healthiar = NULL,
                      impact = NULL,
                      geo_id_disaggregated,
                      social_indicator = NULL,
                      increasing_deprivation = TRUE,
                      n_quantile = NULL, ## by default: decile
                      social_quantile = NULL,
                      population = NULL,
                      bhd = NULL,
                      exp = NULL,
                      pop_fraction = NULL,
                      age_group,
                      ref_prop_pop = NULL
                      ) {

  # Create readable variables for if statements below
  has_output_healthiar <- base::is.null(impact) & !base::is.null(listed_output_healthiar)
  has_impact <- !base::is.null(impact) & base::is.null(listed_output_healthiar)

  has_social_quantile <-
    base::is.null(social_indicator) && base::is.null(n_quantile) && !base::is.null(social_quantile)
  has_social_indicator <-
    !base::is.null(social_indicator) && !base::is.null(n_quantile) && base::is.null(social_quantile)

  has_ref_prop_pop <- !is.null(ref_prop_pop)

  decreasing_deprivation <- !increasing_deprivation


  # * If user enters listed_output_healthiar########
  if ( has_output_healthiar ) {

    # * Convert listed_output_healthiar in a tibble
    output_healthiar <-
      healthiar:::flatten_by_age(listed_output_healthiar = listed_output_healthiar,
                                 age_group = age_group)

    # Compile input data
    # without social component
    input_data <-
      tibble::tibble(
        geo_id_disaggregated = output_healthiar$geo_id_disaggregated,
        age_group = output_healthiar$age_group,
        population = output_healthiar$population,
        impact = output_healthiar$impact,
        exp = output_healthiar$exp,
        bhd = output_healthiar$bhd,
        pop_fraction = output_healthiar$pop_fraction)

    # Compile social data
    # Here use unique() because the user will enter a vector with unique values
    # and not a vector that fits with a table
    # (the user entered the output of healthiar)
    social_component_before_quantile <-
      tibble::tibble(
        geo_id_disaggregated = base::unique(input_data$geo_id_disaggregated),
        social_indicator = social_indicator)

    # * Add international or derived ref_prop_pop ################


    # If ref_prop_pop is not entered by the user, calculate it using populations
    # Otherwise, keep it
    if(base::is.null(ref_prop_pop)){

    } else if (has_ref_prop_pop){
      ref_prop_pop_table <-
        tibble::tibble(
          age_group = base::unique(input_data$age_group),
          ref_prop_pop = base::unique(ref_prop_pop))
    }

        healthiar:::get_ref_prop_pop(df = input_data)



  } else if ( has_impact ) {
    # Using user-entered impacts in vector format ##############################


    # * Merge social_indicator, geo_id_disaggregated and impacts #########################

    # Compile input data
    # without social component
    input_data <-
      tibble::tibble(
        geo_id_disaggregated = geo_id_disaggregated,
        age_group = age_group,
        population = population,
        impact = impact,
        exp = exp,
        bhd = bhd,
        pop_fraction = pop_fraction)

    # Here no unique() in the variables is needed because the users enter the data from a table
    # If they enter the healthiar_output, they do not have a table with the social indicator
    social_component_before_quantile <-
      tibble::tibble(
        geo_id_disaggregated = geo_id_disaggregated,
        social_indicator = social_indicator) |>
      # Use unique after putting both variables together
      # because social_indicator has the same length of geo_id and the table where
      # the users read their data.
      # Doing unique(social_indicator) has the risk that several geo_ids have
      # the same value for the social_indicator
      base::unique()


    # If ref_prop_pop is not entered by the user, calculate it using populations
    # Otherwise, keep it
    if(is.null(ref_prop_pop)){

      ref_prop_pop_table <-
        healthiar:::get_ref_prop_pop(df = input_data)

    } else if(has_ref_prop_pop) {
      # Here without unique() because the ref_prop_pop comes probably from another table
      # so age_group and ref_prop_pop have the same length (including likely repetitions)
      # because of geo_ids
      ref_prop_pop_table <-
        tibble::tibble(
          age_group = input_data$age_group,
          ref_prop_pop = ref_prop_pop)
    }

  }


    # * Create quantiles and add rank based on social indicator ################

    # If social_indicator and n_quantile

    if(has_social_indicator){

      social_component_before_quantile <-
        social_component_before_quantile |>
        # Remove rows with NA in social_indicator
        dplyr::filter( !is.na(social_indicator) )

      if (increasing_deprivation) {
        social_component <- social_component_before_quantile |>
          dplyr::mutate(
            social_ranking = dplyr::dense_rank(social_indicator),
            social_quantile = dplyr::ntile(social_indicator, n = n_quantile))
      } else if(decreasing_deprivation) {
        social_component <- social_component_before_quantile |>
          dplyr::mutate(
            social_ranking = dplyr::dense_rank(dplyr::desc(social_indicator)),
            social_quantile = dplyr::ntile(dplyr::desc(social_indicator), n = n_quantile))
      }

    # If social_decile

    } else if (has_social_quantile){
      social_component <-
        tibble::tibble(geo_id_disaggregated = geo_id_disaggregated,
                       social_quantile = social_quantile) |>
        base::unique()
    }


    age_order <-
      tibble::tibble(
        age_group = base::unique(age_group),
        age_order = 1 : base::length(age_group))


    data <-
      # Add social_quantile (removing the other columns in social_component)
      dplyr::left_join(
        input_data,
        social_component[, c("geo_id_disaggregated", "social_quantile")],
        by = "geo_id_disaggregated") |>
      # Add age_order
      dplyr::left_join(
        age_order,
        by = "age_group") |>
      # Add ref_prop_pop
      dplyr::left_join(
        ref_prop_pop_table,
        by = "age_group"
      )


    # Add ranking_social to input data_with_std


    # * Calculate statistics summed/averaged #################

    impact_rates_by_quantile_and_age <-
      data |>
      ## Group by geo_id and age group to obtain impact rates at that level
      dplyr::group_by(social_quantile, age_group, age_order, ref_prop_pop) |>
      dplyr::summarize(
        population_sum = base::sum(population, na.rm = TRUE),
        impact_sum = base::sum(impact, na.rm = TRUE)) |>
      dplyr::mutate(
        impact_rate = impact_sum / population_sum * 1e5,
        impact_rate_std = impact_rate * ref_prop_pop) |>
      dplyr::ungroup()

    impact_rates_by_quantile <-
      impact_rates_by_quantile_and_age |>
      dplyr::group_by(social_quantile) |>
      dplyr::summarize(impact_rate_sum = base::sum(impact_rate, na.rm = TRUE),
                       impact_rate_std_sum = base::sum(impact_rate_std, na.rm = TRUE))|>
      dplyr::ungroup()

    # Other indicators beyond impact_rates do not need to aggregate in two steps
    # Only one step by quantile

    has_bhd <- "bhd" %in% names(data)
    has_population <- "population" %in% names(data)
    has_exp <- "exp" %in% names(data)
    has_pop_fraction <- "pop_fraction" %in% names(data)

    other_parameters_by_quantile <-
      data |>
      ## Group by geo_id to ensure that you get one result per geo_id
      ## keeping uncertainties
      dplyr::group_by(social_quantile) |>
      dplyr::summarize(
        impact_mean = base::mean(impact, na.rm = TRUE),
        impact_sum = base::sum(impact, na.rm = TRUE),
        bhd_sum = if (has_bhd) base::sum(bhd, na.rm = TRUE) else NA,
        population_sum = if (has_population) sum(population, na.rm = TRUE) else NA,
        bhd_mean = if (has_bhd) base::mean(bhd, na.rm = TRUE) else NA,
        exp_mean = if (has_exp) base::mean(exp, na.rm = TRUE) else NA,
        exp_sd = if (has_exp) stats::sd(exp, na.rm = TRUE) else NA,
        pop_fraction_mean = if (has_pop_fraction) base::mean(pop_fraction, na.rm = TRUE) else NA,
        .groups = "drop") |>
      dplyr::mutate(
        bhd_rate = if (has_bhd && has_population) bhd_sum * 1e5 / population_sum else NA
      )


    parameters_by_quantile <-
      dplyr::left_join(impact_rates_by_quantile,
                       other_parameters_by_quantile,
                       by = "social_quantile")



    other_parameters_overall <-
      data |>
      ## Group by geo_id to ensure that you get one result per geo_id
      ## keeping uncertainties
      dplyr::summarize(
        impact_mean = base::mean(impact, na.rm = TRUE),
        impact_sum = base::sum(impact, na.rm = TRUE),
        bhd_sum = if (has_bhd) base::sum(bhd, na.rm = TRUE) else NA,
        population_sum = if (has_population) sum(population, na.rm = TRUE) else NA,
        bhd_mean = if (has_bhd) base::mean(bhd, na.rm = TRUE) else NA,
        exp_mean = if (has_exp) base::mean(exp, na.rm = TRUE) else NA,
        exp_sd = if (has_exp) stats::sd(exp, na.rm = TRUE) else NA,
        pop_fraction_mean = if (has_pop_fraction) base::mean(pop_fraction, na.rm = TRUE) else NA,
        .groups = "drop") |>
      dplyr::mutate(
        bhd_rate = if (has_bhd && has_population) bhd_sum * 1e5 / population_sum else NA
      )

    impact_rates_overall <-
      impact_rates_by_quantile_and_age |>
      dplyr::summarize(impact_rate_sum = base::sum(impact_rate, na.rm = TRUE),
                       impact_rate_std_sum = base::sum(impact_rate_std, na.rm = TRUE))

    parameters_overall <-
      dplyr::bind_cols(impact_rates_overall,
                       other_parameters_overall)

    parameters_overall_prepared <-
      parameters_overall |>
      ## Pivot longer to prepare data to be joined below
      tidyr::pivot_longer(
        cols = tidyr::everything(),
        names_to = "parameter",
        values_to = "overall")

    # * Determine differences in statistics between quantiles ##################

    social_calculation <-
      parameters_by_quantile |>
      # Remove columns age_group and ref_prop_pop
      # They are not needed and avoid flattening the table
      #dplyr::select(-age_group, -ref_prop_pop) |>
      ## Pivot longer to prepare the data and have a column for parameter
      tidyr::pivot_longer(cols = -social_quantile,
                          names_to = "parameter",
                          values_to = "difference_value") |>
      ## Put column parameter first
      dplyr::select(parameter, everything()) |>
      ## Order columns by parameter
      dplyr::arrange(parameter) |>
      ## Obtain the first (most deprived) and last (least deprived) values
      ## for each parameter
      dplyr::group_by(parameter) |>
      dplyr::summarize(
        first = dplyr::first(difference_value),
        last = dplyr::last(difference_value)) |>
      ## Add the overall (not by quantile) sums and means
      dplyr::left_join(
        x = _,
        y = parameters_overall_prepared,
        by = "parameter") |>
      ## Calculate absolute and relative differences
      dplyr::mutate(
        absolute_quantile = first - last,
        relative_quantile = absolute_quantile / last,
        absolute_overall = overall - last,
        relative_overall = absolute_overall / overall)

    # * Prepare main output table ##############################################
    # browser()
    social_results <-
      social_calculation |>
      ## Filter for relevant rows
      dplyr::filter(
        parameter %in% c("exp_mean",
                         "bhd_rate",
                         "pop_fraction_mean",
                         "impact_rate_sum",
                         "impact_rate_std_sum")) |>
      ## Long instead of wide layout
      tidyr::pivot_longer(
        cols = contains("_"),
        names_to = c("difference_type", "difference_compared_with"),
        values_to = "difference_value",
        names_sep = "_") |>
      ## Change and add columns
      dplyr::mutate(
        parameter_string =
          dplyr::case_when(
            base::grepl("exp_", parameter) ~ "exposure",
            base::grepl("bhd_", parameter) ~ "baseline health data",
            base::grepl("pop_fraction_", parameter) ~ "population attributable fraction",
            base::grepl("impact_", parameter) ~ "impact"),
        ## Replace "quantile" with "bottom_quantile"
        difference_compared_with =
          base::gsub("quantile", "bottom_quantile", difference_compared_with),
        ## Flag attributable fraction
        is_paf_from_deprivation =
          difference_type == "relative" & difference_compared_with == "overall",
        is_attributable_from_deprivation =
          difference_type == "absolute" & difference_compared_with == "overall",
        ## Add comment to clarify
        comment =
          dplyr::case_when(
            is_paf_from_deprivation ~
              base::paste0("It can be interpreted as fraction attributable to deprivation"),
            is_attributable_from_deprivation ~
              base::paste0("It can be interpreted as ", parameter_string, " attributable to deprivation"))) |>
      ## Remove columns that are not needed anymore
      dplyr::select(-is_paf_from_deprivation, -is_attributable_from_deprivation,
                    -parameter_string)

    if ( has_output_healthiar ) {
    output_social <-
      base::list(health_main = output_healthiar[["healt_main"]],
           health_detailed = output_healthiar[["healt_detailed"]])

    } else if (has_impact ){
      output_social <-
        base::list()
    }

    output_social[["social_main"]] <-
      social_results |>
      ## Keep only impact as parameter
      ## This is the most relevant result.
      ## The other paramenters can be stored in detailed
      ## (just in case some users have interest on this)
      ## NOTE: if the population argument is not specified (i.e. it is NULL), then this table is full of NA's 3 ####
      dplyr::filter(parameter == "impact_rate_std_sum")

    output_social[["social_detailed"]][["results_all_parameters"]] <- social_results
    output_social[["social_detailed"]][["parameters_per_quantile"]] <- parameters_by_quantile
    output_social[["social_detailed"]][["parameters_overall"]] <- parameters_overall
    output_social[["social_detailed"]][["input_table"]] <- data


    return(output_social)



}
