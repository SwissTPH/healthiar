#' Consider socio-economic aspects in healthiar assessments

#' @description
#' This function considers socio-economic aspects (e.g. multiple deprivation index) in the attributable health impacts

#' @inheritParams attribute_master
#' @param listed_output_healthiar \code{List} containing \code{sub-lists} with the results of \code{healthiar::attribute_health()} for each age group. Each list element should refer to one specific age group.
#' @param impact \code{Numeric vector} containing the attributable health impacts by both age group and geo id.
#' @param population \code{Integer vector} containing the population by age group and geographic unit.
#' @param age_group \code{String vector} with the age groups included in the age standardization. Each vector element refers to each of the list elements of \code{listed_output_healthiar}.
#' @param ref_prop_pop \code{Numeric vector} with the reference proportion of population for each age group.
#' @param social_indicator \code{Numeric vector} showing social indicator, e.g. the deprivation score (indicator of economic wealth), of the fine geographical area.
#' @param increasing_deprivation \code{Boolean} variable (TRUE/FALSE) that specifies if deprivation is higher when the \code{social_indicator} is higher (TRUE) or lower (FALSE). Default value = TRUE.
#' @param n_quantile \code{Integer value} specifying to the number quantiles in the analysis.


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
                      social_indicator,
                      increasing_deprivation = TRUE,
                      n_quantile = 10, ## by default: decile
                      population = NULL,
                      bhd = NULL,
                      exp = NULL,
                      pop_fraction = NULL,
                      age_group,
                      ref_prop_pop
                      ) {


  # Using the output of attribute ##############################################

  if ( is.null(impact) & !is.null(listed_output_healthiar) ) {

    # * Convert listed_output_healthiar in a tibble
    output_healthiar <-
      healthiar::standardize(listed_output_healthiar = listed_output_healthiar,
                             age_group = age_group,
                             ref_prop_pop = ref_prop_pop)$health_main

    # * Add social_indicator to detailed output ################################

    output_social <-
      dplyr::left_join(
        output_healthiar,
        dplyr::tibble(geo_id_disaggregated = geo_id_disaggregated,
                      social_indicator = unlist(social_indicator)),
        by = "geo_id_disaggregated")

    # * Create quantiles and add rank based on social indicator ################

    output_social <-
      output_social |>
      ## Remove NAs
      dplyr::filter(!is.na(social_indicator)) |>
      ## Add ranking of deprivation score and quantiles
      dplyr::mutate(
        ranking = dplyr::min_rank(dplyr::desc(social_indicator)),
        quantile = dplyr::ntile(dplyr::desc(social_indicator), n = n_quantile))

    # * Calculate statistics summed/averaged over all geographic units #########

    overall <-
      output_social |>
      dplyr::summarize(
        ## Sum of baseline health data in the overall data set
        bhd_sum = sum(bhd, na.rm = TRUE),
        ## Sum of population in the overall data set
        population_sum = sum(population, na.rm = TRUE),
        ## Baseline health data per 100k inhab.
        bhd_rate = bhd_sum * 1e5 / population_sum,
        ## Average baseline health data in the overall data set
        bhd_mean = mean(bhd, na.rm = TRUE),
        ## Average exposure in the overall data set
        exp_mean = mean(exp, na.rm = TRUE),
        ## Standard deviation of exposure
        exp_sd = sd(exp, na.rm = TRUE),
        ## Average attributable fraction in the overall data set
        pop_fraction_mean = mean(pop_fraction, na.rm = TRUE),
        ## Average impact in the overall data set
        impact_mean = mean(impact, na.rm = TRUE),
        ## Absolute impact and population (sum across all geo units),
        impact_sum = sum(impact, na.rm = TRUE),
        ## Impact rate in all geographical units (without stratification by quantile)
        ## per 100'000 inhabitants
        impact_rate = (impact_sum / population_sum) * 1e5) |>
      ## Pivot longer to prepare data to be joined below
      tidyr::pivot_longer(
        cols = everything(),
        names_to = "parameter",
        values_to = "overall")

    # * Calculate statistics summed/averaged for each quantile #################
browser()
    output_social_by_quantile <-
      output_social |>
      ## Group by geo_id to ensure that you get one result per geo_id
      ## keeping uncertainties
      dplyr::group_by(quantile) |>
      dplyr::summarize(
        bhd_sum = sum(bhd, na.rm = TRUE),
        population_sum = sum(population, na.rm = TRUE),
        bhd_rate = bhd_sum * 1e5 / population_sum,
        bhd_mean = mean(bhd, na.rm = TRUE),
        exp_mean = mean(exp, na.rm = TRUE),
        exp_sd = sd(exp, na.rm = TRUE),
        pop_fraction_mean = mean(pop_fraction, na.rm = TRUE),
        impact_mean = mean(impact, na.rm = TRUE),
        impact_sum = sum(impact, na.rm = TRUE),
        impact_rate = impact_sum * 1e5 / population_sum)


    # * Determine differences in statistics between quantiles ##################

    social_calculation <-
      output_social_by_quantile |>
      ## Pivot longer to prepare the data and have a column for parameter
      tidyr::pivot_longer(cols = contains("_"),
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
        y = overall,
        by = "parameter") |>
      ## Calculate absolute and relative differences
      dplyr::mutate(
        absolute_quantile = first - last,
        relative_quantile = absolute_quantile / last,
        absolute_overall = overall - last,
        relative_overall = absolute_overall / overall)


    # * Prepare main output table ##############################################

    social_results <-
      social_calculation |>
      ## Filter for relevant rows
      dplyr::filter(
        parameter %in% c("exp_mean",
                         "bhd_rate",
                         "pop_fraction_mean",
                         "impact_rate")) |>
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
            grepl("exp_", parameter) ~ "exposure",
            grepl("bhd_", parameter) ~ "baseline health data",
            grepl("pop_fraction_", parameter) ~ "population attributable fraction",
            grepl("impact_", parameter) ~ "impact"),
        ## Replace "quantile" with "bottom_quantile"
        difference_compared_with =
          gsub("quantile", "bottom_quantile", difference_compared_with),
        ## Flag attributable fraction
        is_paf_from_deprivation =
          difference_type == "relative" & difference_compared_with == "overall",
        is_attributable_from_deprivation =
          difference_type == "absolute" & difference_compared_with == "overall",
        ## Add comment to clarify
        comment =
          dplyr::case_when(
            is_paf_from_deprivation ~
              paste0("It can be interpreted as fraction attributable to deprivation"),
            is_attributable_from_deprivation ~
              paste0("It can be interpreted as ", parameter_string, " attributable to deprivation"))) |>
      ## Remove columns that are not needed anymore
      dplyr::select(-is_paf_from_deprivation, -is_attributable_from_deprivation,
                    -parameter_string)

    output_social <-
      listed_output_healthiar

    output_social[["social_main"]] <-
      social_results |>
      ## Keep only impact as parameter
      ## This is the most relevant result.
      ## The other paramenters can be stored in detailed
      ## (just in case some users have interest on this)
      dplyr::filter(parameter == "impact_rate")

    output_social[["social_detailed"]][["results_detailed"]] <- social_results
    output_social[["social_detailed"]][["overview_quantiles"]] <- output_social_by_quantile

    return(output_social)

  } else if ( !is.null(impact) & is.null(listed_output_healthiar) ) {

    # Using user-entered impacts in vector format ##############################

    # * Merge social_indicator, geo_id_disaggregated and impacts #########################
browser()

    # Compile input data
    input_data <-
      tibble::tibble(
        geo_id_disaggregated = geo_id_disaggregated,
        social_indicator = social_indicator,
        age_group = age_group,
        population = population,
        ref_prop_pop = ref_prop_pop,
        impact = impact,
        exp = exp,
        bhd = bhd,
        pop_fraction = pop_fraction)


    # output_social <-
    #   tibble::tibble(
    #     geo_id_disaggregated = geo_id_disaggregated,
    #     impact = impact,
    #     social_indicator = social_indicator,
    #     population = if ( !is.null({{ population }}) ) { population } else { NA },
    #     bhd = if ( !is.null({{ bhd }}) ) { bhd } else { NA },
    #     exp = if ( !is.null({{ exp }}) ) { exp } else { NA },
    #     pop_fraction = if ( !is.null({{ pop_fraction }}) ) { pop_fraction } else { NA }
    #   )

    # * Create quantiles and add rank based on social indicator ################

    social_component <-
      input_data |>
      ## Select only geo_id as id for social indicator
      ## Social indicator is not age-group specific but geo_id specific
      dplyr::select(geo_id_disaggregated, social_indicator) |>
      ## Keep only unique row (avoiding duplicates from age_group clasification)
      base::unique() |>
      ## Remove NAs
      dplyr::filter(!is.na(social_indicator))

    if (increasing_deprivation) {
      social_component <- social_component |>
        dplyr::mutate(
          ranking = dplyr::dense_rank(social_indicator),
          quantile = dplyr::ntile(social_indicator, n = n_quantile))
    } else {
      social_component <- social_component |>
        dplyr::mutate(
          ranking = dplyr::dense_rank(dplyr::desc(social_indicator)),
          quantile = dplyr::ntile(dplyr::desc(social_indicator), n = n_quantile))
    }

    age_order <-
      tibble::tibble(
        age_group = base::unique(age_group),
        age_order = 1 : base::length(age_group)
      )


    data <-
      social_component |>
      # Remove social_indicator
      # (not needed and creates a conflict with the existing column in data)
      dplyr::select(-social_indicator) |>
      # Add input data
      dplyr::right_join(input_data,
                        by = "geo_id_disaggregated") |>
      # Add age_order
      dplyr::left_join(
        age_order,
        by = "age_group")


    # Add ranking_social to input data_with_std

    # * Calculate statistics summed/averaged over all geographic units #########

    overall <-
      data |>
      dplyr::group_by(age_group, age_order, ref_prop_pop) |>
      dplyr::summarize(
        ## Sum of baseline health data in the overall data set
        bhd_sum = if ( !is.null({{ bhd }}) ) {
          sum(bhd, na.rm = TRUE)
        } else { NA },
        ## Sum of population in the overall data set
        population_sum = if ( !is.null({{population}}) ) {
          sum(population, na.rm = TRUE)
          } else { NA },
        ## Baseline health data per 100k inhab.
        bhd_rate = if ( !is.na( bhd_sum ) & !is.na( population_sum ) ) {
          bhd_sum * 1e5 / population_sum
        } else { NA },
        ## Average baseline health data in the overall data set
        bhd_mean = if ( !is.null({{ bhd }})) {
          mean(bhd, na.rm = TRUE)
        } else { NA },
        ## Average exposure in the overall data set
        exp_mean = if ( !is.null({{ exp }})) {
          mean(exp, na.rm = TRUE)
        } else { NA },
        ## Standard deviation of exposure
        exp_sd = if ( !is.na( exp_mean ) ) {
          sd(exp, na.rm = TRUE)
          } else { NA },
        ## Average attributable fraction in the overall data set
        pop_fraction_mean = if ( !is.null({{ pop_fraction }})) {
          mean(pop_fraction, na.rm = TRUE)
        } else { NA },
        ## Average impact in the overall data set
        impact_mean = mean(impact, na.rm = TRUE),
        ## Absolute impact and population (sum across all geo units),
        impact_sum = sum(impact, na.rm = TRUE))|>
      dplyr::mutate(
        impact_rate = impact_sum / population_sum * 1e5,
        impact_rate_std = impact_rate * ref_prop_pop) |>
      ## Pivot longer to prepare data to be joined below
      tidyr::pivot_longer(
        cols = bhd_sum:impact_rate_std,
        names_to = "parameter",
        values_to = "overall")

    # * Calculate statistics summed/averaged for each quantile #################

    by_quantile_and_age <-
      data |>
      ## Group by geo_id to ensure that you get one result per geo_id
      ## keeping uncertainties
      dplyr::group_by(quantile, age_group, age_order, ref_prop_pop) |>
      dplyr::summarize(
        bhd_sum = if ( !is.null( {{ bhd }} ) ) {
          sum(bhd, na.rm = TRUE) } else { NA },
        population_sum = if ( !is.null( {{population}} ) ) {
          sum(population, na.rm = TRUE) } else { NA },
        bhd_rate = if ( !is.null( {{ bhd }} ) ) {
          bhd_sum * 1e5 / population_sum } else { NA },
        bhd_mean = if ( !is.null({{ bhd }})) {
          mean(bhd, na.rm = TRUE) } else { NA },
        exp_mean = if ( !is.null({{ exp }})) {
          mean(exp, na.rm = TRUE) } else { NA },
        exp_sd = if ( !is.na( exp_mean ) ) {
          sd(exp, na.rm = TRUE) } else { NA },
        pop_fraction_mean = if ( !is.null({{ pop_fraction }})) {
          mean(pop_fraction, na.rm = TRUE) } else { NA },
        impact_mean = mean(impact, na.rm = TRUE),
        impact_sum = sum(impact, na.rm = TRUE))|>
      dplyr::mutate(
        impact_rate = impact_sum / population_sum * 1e5,
        # impact_rate = if ( !is.null( {{ population }} ) ) {
        #   (impact_sum / population_sum) * 1e5
        # } else {NA},
        impact_rate_std = impact_rate * ref_prop_pop)

    by_quantile <-
      ## Add impact_rate and impact_rate_std
      by_quantile_and_age |>
      ## Group by geo_id to ensure that you get one result per geo_id
      ## keeping uncertainties
      dplyr::group_by(quantile) |>
      dplyr::summarize(
        bhd_sum = if ( !is.null( {{ bhd }} ) ) {
          sum(bhd, na.rm = TRUE) } else { NA },
        population_sum = if ( !is.null( {{population}} ) ) {
          sum(population, na.rm = TRUE) } else { NA },
        bhd_rate = if ( !is.null( {{ bhd }} ) ) {
          bhd_sum * 1e5 / population_sum } else { NA },
        bhd_mean = if ( !is.null({{ bhd }})) {
          mean(bhd, na.rm = TRUE) } else { NA },
        exp_mean = if ( !is.null({{ exp }})) {
          mean(exp, na.rm = TRUE) } else { NA },
        exp_sd = if ( !is.na( exp_mean ) ) {
          sd(exp, na.rm = TRUE) } else { NA },
        pop_fraction_mean = if ( !is.null({{ pop_fraction }})) {
          mean(pop_fraction, na.rm = TRUE) } else { NA },
        impact_mean = mean(impact, na.rm = TRUE),
        impact_sum = sum(impact, na.rm = TRUE),
        impact_rate_sum = sum(impact_rate, na.rm = TRUE),
        impact_rate_std_sum = sum(impact_rate_std, na.rm = TRUE))



    # * Determine differences in statistics between quantiles ##################

    social_calculation <-
      by_quantile_and_age |>
      # Remove columns age_group and ref_prop_pop
      # They are not needed and avoid flattening the table
      #dplyr::select(-age_group, -ref_prop_pop) |>
      ## Pivot longer to prepare the data and have a column for parameter
      tidyr::pivot_longer(cols = bhd_sum:impact_rate_std,
                          names_to = "parameter",
                          values_to = "difference_value") |>
      ## Put column parameter first
      dplyr::select(parameter, everything()) |>
      ## Order columns by parameter
      dplyr::arrange(age_order, parameter) |>
      ## Obtain the first (most deprived) and last (least deprived) values
      ## for each parameter
      dplyr::group_by(parameter) |>
      dplyr::summarize(
        first = dplyr::first(difference_value),
        last = dplyr::last(difference_value)) |>
      ## Add the overall (not by quantile) sums and means
      dplyr::left_join(
        x = _,
        y = overall,
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
                         "impact_rate_std_mean")) |>
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
            grepl("exp_", parameter) ~ "exposure",
            grepl("bhd_", parameter) ~ "baseline health data",
            grepl("pop_fraction_", parameter) ~ "population attributable fraction",
            grepl("impact_", parameter) ~ "impact"),
        ## Replace "quantile" with "bottom_quantile"
        difference_compared_with =
          gsub("quantile", "bottom_quantile", difference_compared_with),
        ## Flag attributable fraction
        is_paf_from_deprivation =
          difference_type == "relative" & difference_compared_with == "overall",
        is_attributable_from_deprivation =
          difference_type == "absolute" & difference_compared_with == "overall",
        ## Add comment to clarify
        comment =
          dplyr::case_when(
            is_paf_from_deprivation ~
              paste0("It can be interpreted as fraction attributable to deprivation"),
            is_attributable_from_deprivation ~
              paste0("It can be interpreted as ", parameter_string, " attributable to deprivation"))) |>
      ## Remove columns that are not needed anymore
      dplyr::select(-is_paf_from_deprivation, -is_attributable_from_deprivation,
                    -parameter_string)

    output_social <-
      list()

    output_social[["social_main"]] <-
      social_results |>
      ## Keep only impact as parameter
      ## This is the most relevant result.
      ## The other paramenters can be stored in detailed
      ## (just in case some users have interest on this)
      ## NOTE: if the population argument is not specified (i.e. it is NULL), then this table is full of NA's 3 ####
      dplyr::filter(parameter == "impact_rate_std")

    output_social[["social_detailed"]][["results_detailed"]] <- social_results
    output_social[["social_detailed"]][["impact_per_quantile"]] <- by_quantile_and_age
    output_social[["social_detailed"]][["input_table"]] <- input_data

    return(output_social)

  }

}
