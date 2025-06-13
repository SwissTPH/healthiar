#' Consider socio-economic aspects in healthiar assessments

#' @description
#' This function considers socio-economic aspects (e.g. multiple deprivation index) in the attributable health impacts. If nothing is entered in the argument \code{listed_output_attribute}, it is assumed that all data come from a table and the argument refer to the columns of that table.

#' @param listed_output_attribute
#' \code{List} containing \code{sub-lists} with the results of \code{healthiar::attribute_health()} for each age group. Each list element should refer to one specific age group.

#' @param age_group
#' \code{String vector} with the age groups included in the age standardization. Each vector element refers to each of the list elements of \code{listed_output_attribute}.

#' @param social_indicator
#' \code{Numeric vector} showing the social indicator used for the analysis, e.g. a deprivation score (indicator of economic wealth) for each geographic unit. Based on this and \code{n_quantile}, \code{social_quantile} will be calculated.

#' @param increasing_deprivation
#' \code{Boolean} variable (\code{TRUE}/\code{FALSE}) specifying whether an increase in \code{social_indicator} corresponds to an increase (\code{TRUE}) or decrease \code{FALSE} in deprivation. Default: \code{TRUE}.

#' @param n_quantile
#' \code{Integer value} specifying the number of quantiles in the analysis.

#' @param social_quantile
#' \code{Numeric vector} showing the values from 1 to the number of quantiles assigned to each geographic unit. Either enter \code{social_indicator} and \code{n_quantile} or \code{social_quantile}

#' @param geo_id_disaggregated,
#' \code{Numeric vector} or \code{string vector} specifying the unique ID codes of each geographic area considered in the assessment (\code{geo_id_disaggregated}) Argument must be entered for iterations. See Details for more info.

#' @param population
#' \code{Integer vector} specifying the population by age group and geographic unit.

#' @param ref_prop_pop
#' \code{Numeric vector} specifying with the reference proportion of population for each age group. If this argument is empty, the proportion of \code{population} by age group in the provided data will be used.

#' @param impact
#' \emph{(only if \code{listed_output_attribute} not specified)} \code{Numeric vector} containing the attributable health impacts by both age group and geo id.

#' @param bhd
#' \emph{(only if \code{listed_output_attribute} not specified)} \code{Numeric vector} specifying the baseline health data of the health outcome of interest per age group. See Details for more info.

#' @param exp
#'\emph{(only if \code{listed_output_attribute} not specified)} \code{Numeric vector} specifying the exposure level(s) to the environmental stressor.

#' @param pop_fraction
#' \emph{(only if \code{listed_output_attribute} not specified)} \code{Numeric vector} specifying the population attributable fraction by age group and geographic unit.

#' @returns Returns the impact (absolute and relative) theoretically attributable to the difference in the social indicator (e.g. degree of deprivation) between the quantiles.

#' @author
#' TODO

#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)

#' @export

socialize <- function(listed_output_attribute = NULL,
                      age_group,
                      geo_id_disaggregated,
                      social_indicator = NULL,
                      increasing_deprivation = TRUE,
                      n_quantile = NULL, ## by default: decile
                      social_quantile = NULL,
                      population = NULL,
                      ref_prop_pop = NULL,
                      impact = NULL,
                      exp = NULL,
                      bhd = NULL,
                      pop_fraction = NULL
                      ) {

  # Variables for ifs #####################

  # Create readable variables for if statements below

  # output from healthiar or impact directly entered by user (without healthiar)
  has_output_attribute <- base::is.null(impact) & !base::is.null(listed_output_attribute)
  has_impact <- !base::is.null(impact) & base::is.null(listed_output_attribute)

  # already social quantile (e.g. 1-10) or
  # social indicator (e.g. 1-986) which has to be transformed into quantile
  has_social_quantile <-
    base::is.null(social_indicator) && base::is.null(n_quantile) && !base::is.null(social_quantile)
  has_social_indicator <-
    !base::is.null(social_indicator) && !base::is.null(n_quantile) && base::is.null(social_quantile)

  # Available ref_prop_pop
  has_ref_prop_pop <- !is.null(ref_prop_pop)

  # Decreasing order in social_indicator or quantile
  decreasing_deprivation <- !increasing_deprivation

  # Compile data (except social) ##########

  # * If available listed_output_attribute ########
  if ( has_output_attribute ) {


    # Convert listed_output_attribute in a tibble
    output_attribute <-
      healthiar:::flatten_by_age(listed_output_attribute = listed_output_attribute,
                                 age_group = age_group)

    # Compile input data
    # without social component
    input_data <-
      output_attribute |>
      dplyr::select(
        dplyr::any_of(c("geo_id_disaggregated", "age_group", "population",
                        "impact", "exp", "bhd", "pop_fraction")))

    # Compile social component
    # Here use unique() because the user will enter a vector with unique values
    # and not a vector that fits with a table
    # (the user entered the output from healthiar)
    social_component_before_quantile <-
      tibble::tibble(
        geo_id_disaggregated = base::unique(input_data$geo_id_disaggregated),
        social_indicator = social_indicator)

    ## * If available ref_prop_pop ################

    # If ref_prop_pop is entered by the user, use it
    if(has_ref_prop_pop){
      ref_prop_pop_table <-
        tibble::tibble(
          age_group = base::unique(input_data$age_group),
          ref_prop_pop = base::unique(ref_prop_pop)) |>
        base::unique()

      # * If NOT available ref_prop_pop ################
      # Calculate it using populations
      } else if (base::is.null(ref_prop_pop)){

        ref_prop_pop_table <-
          healthiar:::get_ref_prop_pop(df = input_data)
      }



    } else if ( has_impact ) {

      # * If NOT available list_output_attribute, i.e. if argument impact #########

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
      # If they enter the output_attribute, they do not have a table with the social indicator
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

        #### * If available ref_prop_pop ################

        if(has_ref_prop_pop){
          # Here without unique() because the ref_prop_pop comes probably from another table
          # so age_group and ref_prop_pop have the same length (including likely repetitions)
          # because of geo_ids
          ref_prop_pop_table <-
            tibble::tibble(
              age_group = input_data$age_group,
              ref_prop_pop = ref_prop_pop) |>
            base::unique()

          ### * If NOT available ref_prop_pop ################
        } else if(is.null(ref_prop_pop)) {
          ref_prop_pop_table <-
            healthiar:::get_ref_prop_pop(df = input_data)
          }
    }


  # Compile social data ##########

  # Create quantiles and add rank based on social indicator

  # * If available social_quantile #########
  if(has_social_quantile){
    social_component <-
      tibble::tibble(geo_id_disaggregated = geo_id_disaggregated,
                     social_quantile = social_quantile) |>
      base::unique()


    # * If NOT available social_decile, then social_indicator and n_quantile #########
    } else if (has_social_indicator){
      social_component_before_quantile <-
        social_component_before_quantile |>
        # Remove rows with NA in social_indicator
        dplyr::filter( !is.na(social_indicator) )

      ## * If increasing_deprivation #########
      if (increasing_deprivation) {
        social_component <- social_component_before_quantile |>
          dplyr::mutate(
            social_ranking = dplyr::dense_rank(dplyr::desc(social_indicator)),
            social_quantile = dplyr::ntile(dplyr::desc(social_indicator), n = n_quantile))


      } else if(decreasing_deprivation) {
        # * If NOT increasing_deprivation, i.e. decreasing #########
        social_component <- social_component_before_quantile |>
          dplyr::mutate(
            social_ranking = dplyr::dense_rank(social_indicator),
            social_quantile = dplyr::ntile(social_indicator, n = n_quantile))
      }


    }

  # Put all data together ####################

  data <-
    # Add social_quantile (removing the other columns in social_component)
    dplyr::left_join(
      input_data,
      social_component[, c("geo_id_disaggregated", "social_quantile")],
      by = "geo_id_disaggregated") |>
    # Add age_order
    dplyr::left_join(
      tibble::tibble(
        age_group = base::unique(age_group),
        age_order = 1 : base::length(age_group)),
      by = "age_group") |>
    # Add ref_prop_pop
    dplyr::left_join(
      ref_prop_pop_table,
      by = "age_group"
    )

  # Calculate statistics summed/averaged #################

  ## _by_quantile ############

  ### impact_rates #################
  # Two steps:
  # 1) by quantile and age to calculte impact rates
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

  # 1) by quantile (as other parameters)
  impact_rates_by_quantile <-
    impact_rates_by_quantile_and_age |>
    # Group only by social_quantile to get impact_rates by quantile (higher level)
    dplyr::group_by(social_quantile) |>
    dplyr::summarize(impact_rate_sum = base::sum(impact_rate, na.rm = TRUE),
                     impact_rate_std_sum = base::sum(impact_rate_std, na.rm = TRUE))|>
    dplyr::ungroup()


  ### other parameters (beyond impact rates) #################

  # These parameters do not need to aggregate in two steps
  # Only one step by quantile

  # Define first the variables for if statements
  has_bhd <- "bhd" %in% names(data)
  has_population <- "population" %in% names(data)
  has_exp <- "exp" %in% names(data)
  has_pop_fraction <- "pop_fraction" %in% names(data)


  # Define function to get_other_parameters()
  get_other_parameters <- function(df){
    other_parameters <- df |>
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
    return(other_parameters)
  }


  other_parameters_by_quantile <-
    data |>
    ## Group by social_quantile
    dplyr::group_by(social_quantile) |>
    get_other_parameters()

  ### All parameters together
  # Put together input rates and other parameters
  parameters_by_quantile <-
    dplyr::left_join(impact_rates_by_quantile,
                     other_parameters_by_quantile,
                     by = "social_quantile")

  ## _overall ############

  ### impact_rates #################

  impact_rates_overall <-
    # Two steps but for the overall level impact_rates_by_quantile_and_age can be reused
    impact_rates_by_quantile_and_age |>
    # Without grouping because it is overall
    dplyr::summarize(impact_rate_sum = base::sum(impact_rate, na.rm = TRUE),
                     impact_rate_std_sum = base::sum(impact_rate_std, na.rm = TRUE))

  ### other parameters (beyond impact rates) #################
  other_parameters_overall <-
    data |>
    # Without grouping because it is overall
    get_other_parameters()

  ### All parameters together
  parameters_overall <-
    dplyr::bind_cols(impact_rates_overall,
                     other_parameters_overall)

  # Prepared to be joined below
  parameters_overall_prepared <-
    parameters_overall |>
    tidyr::pivot_longer(
      cols = tidyr::everything(),
      names_to = "parameter",
      values_to = "overall")


  # Find differences between quantiles ##################

  social_calculation <-
    parameters_by_quantile |>
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
      parameters_overall_prepared,
      by = "parameter") |>
    ## Calculate absolute and relative differences
    dplyr::mutate(
      absolute_quantile = first - last,
      relative_quantile = absolute_quantile / last,
      absolute_overall = overall - last,
      relative_overall = absolute_overall / overall)

  # Obtain main results ##############################################

  social_results <-
    social_calculation |>
    # Filter for relevant rows
    dplyr::filter(
      parameter %in% c("exp_mean",
                       "bhd_rate",
                       "pop_fraction_mean",
                       "impact_rate_sum",
                       "impact_rate_std_sum")) |>
    # Long instead of wide layout
    tidyr::pivot_longer(
      cols = contains("_"),
      names_to = c("difference_type", "difference_compared_with"),
      values_to = "difference_value",
      names_sep = "_") |>
    dplyr::mutate(
      # Write the readable names fo the parameters
      parameter_string =
        dplyr::case_when(
          base::grepl("exp_", parameter) ~ "exposure",
          base::grepl("bhd_", parameter) ~ "baseline health data",
          base::grepl("pop_fraction_", parameter) ~ "population attributable fraction",
          base::grepl("impact_", parameter) ~ "impact"),
      # Replace "quantile" with "bottom_quantile"
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
    dplyr::select(-is_paf_from_deprivation,
                  -is_attributable_from_deprivation,
                  -parameter_string)


  # List output ##############################################


  ## * If available output_attribute ######
  if ( has_output_attribute ) {
    output_social <-
      base::list(health_main = output_attribute[["healt_main"]],
                 health_detailed = output_attribute[["healt_detailed"]])

    ## * If NOT available output_attribute, i.e. if argument impact #######

  } else if (has_impact ){

    output_social <-
      base::list()
  }

  output_social[["social_main"]] <-
    social_results |>
    # Keep only impact_rate_std as parameter
    # This is the most relevant result.
    dplyr::filter(parameter == "impact_rate_std_sum")

  # The other parameters can be stored in detailed
  # (just in case some users have interest on this)
  output_social[["social_detailed"]][["results_all_parameters"]] <- social_results
  output_social[["social_detailed"]][["parameters_per_quantile"]] <- parameters_by_quantile
  output_social[["social_detailed"]][["parameters_overall"]] <- parameters_overall
  output_social[["social_detailed"]][["input_table"]] <- data


  return(output_social)

}
