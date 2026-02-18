#' Consider socio-economic aspects in the attributable health impacts

# DESCRIPTION ##################################################################
#' @description
#' This function analyzes differences in attributable health impacts across study areas
#' looking at the value of a socio-economic indicator (e.g. multiple deprivation index).
#' If nothing is entered in the argument \code{output_attribute},
#' it is assumed that all data come from a table and the argument refer to the columns of that table.

# ARGUMENTS ####################################################################
#' @param output_attribute
#' \code{List} containing the outputs of the \code{healthiar::attribute_health()} assessments for each age group (each list element should be an age group-specific assessment).
#' @param age_group
#' \code{String vector} with the age groups included in the age standardization. The vector refers to age-dependent data in this function and to \code{output_attribute} (if provided).
#' @param social_indicator
#' \code{Numeric vector} showing the social indicator used for the analysis, e.g. a deprivation score (indicator of economic wealth) for each geographic unit. The length and the values must correspond with \code{geo_id_micro}. If \code{geo_id_micro} is not entered when using argument \code{output_attribute}, \code{social_indicator} must correspond to the column \code{geo_id_micro} in \code{results_by_age_group} of \code{output_attribute}.
#' @param increasing_deprivation
#' \code{Boolean} variable (\code{TRUE}/\code{FALSE}) specifying whether an increase in \code{social_indicator} corresponds to an increase (\code{TRUE}) or decrease \code{FALSE} in deprivation. Default: \code{TRUE}.
#' @param n_quantile
#' \code{Integer value} specifying the number of quantiles in the analysis.
#' @param social_quantile
#' \code{Integer vector} showing the values from 1 to the number of quantiles assigned to each geographic unit. Either enter \code{social_indicator} and \code{n_quantile} or \code{social_quantile}
#' @param geo_id_micro,
#' \code{Numeric vector} or \code{string vector} specifying the unique ID codes of each geographic area considered in the assessment (\code{geo_id_micro}).
#' @param population
#' \code{Numeric vector} specifying the population by age group and geographic unit.
#' @param ref_prop_pop
#' \code{Numeric vector} specifying with the reference proportion of population for each age group. If this argument is empty, the proportion of \code{population} by age group in the provided data will be used.
#' @param impact
#' \emph{(only if \code{output_attribute} not specified)} \code{Numeric vector} containing the attributable health impacts by both age group and geo id.
#' @param bhd
#' \emph{(only if \code{output_attribute} not specified)} \code{Numeric vector} specifying the baseline health data of the health outcome of interest per age group.
#' @param exp
#'\emph{(only if \code{output_attribute} not specified)} \code{Numeric vector} specifying the exposure level(s) to the environmental stressor.
#' @param pop_fraction
#' \emph{(only if \code{output_attribute} not specified)} \code{Numeric vector} specifying the population attributable fraction by age group and geographic unit.

# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' This function estimates the absolute and relative differences in attributable health impacts
#' comparing study areas with different values for a socio-economic indicator
#' \insertCite{Renard2019_bmc,Otavova_2022_bmc}{healthiar}.
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#health-impact-attributable-to-social-indicator}{Health impact attributable to social indicator}}
#'
# VALUE ########################################################################
#' @returns
#' This function returns a \code{list} containing the impact (absolute and relative) theoretically attributable to the difference in the social indicator (e.g. degree of deprivation) between the quantiles:
#' @returns
#' 1) \code{social_main} (\code{tibble}) containing the main results;
#' \itemize{
#'  \item \code{difference_value} (\code{numeric} column) attributable health burden/impact due to differences in deprivation levels
#'  \item And more
#' }
#' @returns
#' 2) \code{social_detailed} (\code{list}) containing detailed (and interim) results.
#' \itemize{
#'  \item \code{input_data_with_quantile} (\code{tibble}) containing input data and information about the social quantile
#'  \item \code{results_all_parameters} (\code{tibble}) containing deprivation-related results
#'  \item \code{parameters_overall} (\code{tibble}) containing overall results for different input variables
#'  \item \code{parameters_per_quantile} (\code{tibble}) containing quantile-specific results for different input variables
#' }
#' If the argument \code{output_attribute} was specified, then the two lists are added next to the existing attribute output.

# EXAMPLES #####################################################################
#' @examples
#' # Goal: determine fraction of attributable health impact that can
#' # be attributed to differences in deprivation between the geographic
#' # units under analysis
#'
#' ## Create assessments for multiple geographic units for the age group
#' ## 40 years and younger
#' results_age_groups <-
#'    healthiar::attribute_health(
#'      age_group = exdat_socialize$age_group,
#'      exp_central = exdat_socialize$pm25_mean,
#'      cutoff_central = 0,
#'      rr_central = exdat_socialize$rr,
#'      erf_shape = "log_linear",
#'      rr_increment = 10,
#'      bhd_central = exdat_socialize$mortality,
#'      population = exdat_socialize$population,
#'      geo_id_micro = exdat_socialize$geo_unit)
#'
#' ## Difference in attributable impacts between geographic units
#' ## that is attributable to differences in deprivation
#' results <- socialize(
#'   output_attribute = results_age_groups,
#'   age_group = exdat_socialize$age_group, # They have to be the same in socialize() and in attribute_health()
#'   ref_prop_pop = exdat_socialize$ref_prop_pop,
#'   geo_id_micro = exdat_socialize$geo_unit,
#'   social_indicator = exdat_socialize$score,
#'   n_quantile = 10,
#'   increasing_deprivation = TRUE)
#'
#'
#' results$social_main |>
#'   dplyr::filter(difference_type == "relative") |>
#'   dplyr::filter(difference_compared_with == "overall") |>
#'   dplyr::select(first, last, difference_type, difference_value, comment)
#'
#'
#' @seealso
#' \itemize{
#'   \item Upstream:
#'     \code{\link{attribute_health}}, \code{\link{attribute_lifetable}},
#'      \code{\link{prepare_mdi}},
#' }
#'
#' @references
#'
#' \insertAllCited{}
#'
#'
#' @author Alberto Castro & Axel Luyten
#'
#' @export

socialize <- function(output_attribute = NULL,
                      age_group,
                      geo_id_micro,
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

  # Data validation ######################

  input_args_value <-
    get_input_args(environment = base::environment(),
                   call = match.call())$value

  # Identify available_vars
  # i.e. variables/arguments that have been entered by the user
  available_vars <- input_args_value |>
    purrr::discard(~ base::is.null(.x)) |>
    base::names()

  # All variables by type
  numeric_vars <- c("social_indicator", "pop_fraction", "ref_prop_pop", "exp", "impact")
  integer_vars <- c("social_quantile", "n_quantile")
  boolean_vars <- c("increasing_deprivation")
  fraction_vars <- c("ref_prop_pop", "pop_fraction")

  # Available variables by type
  available_numeric_vars <- base::intersect(numeric_vars, available_vars)
  available_integer_vars <- base::intersect(integer_vars, available_vars)
  available_numeric_and_integer_vars <- c(available_numeric_vars, available_integer_vars)
  ## social_indicator and impact might be lower than 0, therefore excluded here
  available_positive_vars <-
    base::setdiff(available_numeric_and_integer_vars, c("social_indicator", "impact"))
  available_boolean_vars <- base::intersect(boolean_vars, available_vars)
  available_fraction_vars <- base::intersect(fraction_vars, available_vars)

  ## error_if_not_numeric #####
  error_if_not_numeric <- function(var_name){

    var_value <- input_args_value [[var_name]]

    if(base::any(!base::is.numeric(var_value))){

      base::stop(
        base::paste0(
          var_name,
          " must contain numeric value(s)."),
        call. = FALSE)
    }
  }

  if(base::length(available_numeric_and_integer_vars) > 0){
    for (x in available_numeric_and_integer_vars) {
      error_if_not_numeric(var_name = x)
    }
  }

  ## error_if_not_whole number #####
  error_if_not_whole_number <- function(var_name){
    var_value <- input_args_value [[var_name]]

    if(base::any(base::is.numeric(var_value)) &
       base::any(var_value != base::floor(var_value))){

      base::stop(
        base::paste0(
          var_name,
          " must contain whole numeric value(s)."),
        call. = FALSE)
    }
  }

  if(base::length(available_integer_vars) > 0){
    for (x in available_integer_vars) {
      error_if_not_whole_number(var_name = x)
    }
  }

  ## error_if_not_fraction #####
  error_if_not_fraction <- function(var_name){

    var_value <- input_args_value [[var_name]]

    if(base::any(var_value < 0 | var_value > 1)){

      base::stop(
        base::paste0(
          var_name,
          " must have values between 0 and 1."),
        call. = FALSE)
    }
  }

  if(base::length(available_fraction_vars) > 0){
    for (x in available_fraction_vars) {
      error_if_not_fraction(var_name = x)
    }
  }

  ## error_if_lower_than_0 #####
  error_if_lower_than_0 <- function(var_name){
    var_value <- input_args_value [[var_name]]

    if(base::any(var_value < 0)){

      base::stop(
        base::paste0(
          "The value(s) of ",
          var_name,
          " cannot be lower than 0."),
        call. = FALSE)
    }
  }

  if(base::length(available_positive_vars) > 0){
    for (x in available_positive_vars) {
      error_if_lower_than_0(var_name = x)
    }
  }

  ## error_if_not_boolean #####
  error_if_not_boolean <- function(var_name){
    var_value <- input_args_value [[var_name]]

    if(base::any(!base::is.logical(var_value))){

      base::stop(
        base::paste0(
          var_name,
          " must be TRUE or FALSE."),
        call. = FALSE)
    }
  }

  if(base::length(available_boolean_vars) > 0){
    for (x in available_boolean_vars) {
      error_if_not_boolean(var_name = x)
    }
  }

  ## error_if_no_match #####
  error_if_no_match <- function(var_name){
    var_value_user <- base::unique(input_args_value[[var_name]])
    var_value_attribute <-
      base::unique(output_attribute$health_detailed$results_raw$age_group)

    if(! base::identical(var_value_user, var_value_attribute)){

      base::stop(
        base::paste0(
          var_name,
          " must be identical to the values in the column ",
          var_name,
          " in output_attribute."),
        call. = FALSE)

    }
  }

  if(! base::is.null(output_attribute)){
    for (x in c("age_group")) {
      error_if_no_match(var_name = x)
    }
  }



  # Variables for ifs #####################

  ## Create readable variables for if statements below

  ## output from healthiar or impact directly entered by user (without healthiar)
  has_output_attribute <- base::is.null(impact) & !base::is.null(output_attribute)
  has_impact <- !base::is.null(impact) & base::is.null(output_attribute)

  ## already social quantile (e.g. 1-10) or
  ## social indicator (e.g. 1-986) which has to be transformed into quantile
  has_social_quantile <-
    base::is.null(social_indicator) && base::is.null(n_quantile) && !base::is.null(social_quantile)
  has_social_indicator <-
    !base::is.null(social_indicator) && !base::is.null(n_quantile) && base::is.null(social_quantile)

  ## Available ref_prop_pop
  has_ref_prop_pop <- !base::is.null(ref_prop_pop)

  ## Decreasing order in social_indicator or quantile
  decreasing_deprivation <- !increasing_deprivation

  # Compile data (except social) ##########

  # * If available output_attribute ########
  if ( has_output_attribute ) {

    ## Compile input data
    ## without social component
    input_data <-
      output_attribute$health_detailed$results_by_age_group |>
      dplyr::select(
        dplyr::any_of(c("geo_id_micro", "age_group", "population",
                        "impact", "exp", "bhd", "pop_fraction")))

    ## Compile social component
    ## Here use unique() because the user will enter a vector with unique values
    ## and not a vector that fits with a table
    ## (the user entered the output from healthiar)

    social_component_before_quantile <-
      tibble::tibble(
        geo_id_micro = geo_id_micro,
        social_indicator = social_indicator) |>
      dplyr::distinct()

    # * * If available ref_prop_pop ################

    ## If ref_prop_pop is entered by the user, use it
    if(has_ref_prop_pop){
      ref_prop_pop_table <-
        tibble::tibble(
          age_group = age_group,
          ref_prop_pop = ref_prop_pop) |>
        base::unique()

      # * * If NOT available ref_prop_pop ################
      ## Calculate it using populations
      } else if (base::is.null(ref_prop_pop)){

        ref_prop_pop_table <-
          get_ref_prop_pop(df = input_data)
      }



    } else if ( has_impact ) {

      # * If NOT available output_attribute, i.e. if argument impact #########

      ## Compile input data
      ## without social component
      input_data <-
        tibble::tibble(
          geo_id_micro = geo_id_micro,
          age_group = age_group,
          population = population,
          impact = impact,
          exp = exp,
          bhd = bhd,
          pop_fraction = pop_fraction)

      ## Here no unique() in the variables is needed because the users enter the data from a table
      ## If they enter the output_attribute, they do not have a table with the social indicator
      social_component_before_quantile <-
        tibble::tibble(
          geo_id_micro = geo_id_micro,
          social_indicator = social_indicator) |>
        ## Use unique after putting both variables together
        ## because social_indicator has the same length of geo_id and the table where
        ## the users read their data.
        ## Doing unique(social_indicator) has the risk that several geo_ids have
        ## the same value for the social_indicator
        base::unique()

        # * *  If available ref_prop_pop ################

        if(has_ref_prop_pop){
          ## Here without unique() because the ref_prop_pop comes probably from another table
          ## so age_group and ref_prop_pop have the same length (including likely repetitions)
          ## because of geo_ids
          ref_prop_pop_table <-
            tibble::tibble(
              age_group = input_data$age_group,
              ref_prop_pop = ref_prop_pop) |>
            base::unique()

          # * * If NOT available ref_prop_pop ################
        } else if(base::is.null(ref_prop_pop)) {
          ref_prop_pop_table <-
            get_ref_prop_pop(df = input_data)
          }
    }


  # Compile social data ##########

  ## Create quantiles and add rank based on social indicator

  # * If available social_quantile #########
  if(has_social_quantile){
    social_component <-
      tibble::tibble(geo_id_micro = geo_id_micro,
                     social_quantile = social_quantile) |>
      base::unique()


    # * If NOT available social_decile, then social_indicator and n_quantile #########
    } else if (has_social_indicator){
      social_component_before_quantile <-
        social_component_before_quantile |>
        ## Remove rows with NA in social_indicator
        dplyr::filter( !base::is.na(social_indicator) )

      # * * If increasing_deprivation #########
      if (increasing_deprivation) {

        social_component <- social_component_before_quantile |>
          dplyr::mutate(
            social_ranking = base::rank(-social_indicator, na.last = "keep", ties.method = "random"))


      } else if(decreasing_deprivation) {
        # * * If NOT increasing_deprivation, i.e. decreasing #########
        social_component <- social_component_before_quantile |>
          dplyr::mutate(
            social_ranking = base::rank(social_indicator, na.last = "keep", ties.method = "random"))
      }

      # Add quantile which is common for both case increasing and decreasing deprivation
      social_component <- social_component|>
        dplyr::mutate(
          social_quantile = base::cut(
            social_ranking,
            breaks = stats::quantile(social_ranking, probs = seq(0, 1, by = 0.1)),
            labels = FALSE, include.lowest = TRUE
          ))

    }

  # Put all data together ####################
  input_data_with_quantile <-
    ## Add social_quantile (removing the other columns in social_component)
    dplyr::left_join(
      input_data,
      base::unique(social_component[, c("geo_id_micro", "social_quantile")]),
      by = "geo_id_micro") |>
    ## Add age_order
    dplyr::left_join(
      tibble::tibble(
        age_group = base::unique(input_data$age_group),
        age_order = 1 : base::length(age_group)),
      by = "age_group") |>
    ## Add ref_prop_pop
    dplyr::left_join(
      ref_prop_pop_table,
      by = "age_group"
    )

  # Calculate statistics summed/averaged #################

  # * _by_quantile ############

  # * * impact_rates #################
  ## Two steps:
  ## 1) by quantile and age to calculte impact rates
  impact_rates_by_quantile_and_age <-
    input_data_with_quantile |>
    ## Group by geo_id and age group to obtain impact rates at that level
    dplyr::summarize(
      .by = c(social_quantile, age_group, age_order, ref_prop_pop),
      population_sum = base::sum(population, na.rm = TRUE),
      impact_sum = base::sum(impact, na.rm = TRUE)) |>
    dplyr::mutate(
      impact_rate = impact_sum / population_sum * 1e5,
      impact_rate_std = impact_rate * ref_prop_pop)

  ## 2) by quantile (as other parameters)
  impact_rates_by_quantile <-
    impact_rates_by_quantile_and_age |>
    # Group only by social_quantile to get population and impact by quantile (higher level)
    # and impact_rate_std (but not impact_rate)
    dplyr::summarize(
      .by = social_quantile,
      population_sum = base::sum(population_sum, na.rm = TRUE),
      impact_sum = base::sum(impact_sum, na.rm = TRUE),
      impact_rate_std = base::sum(impact_rate_std, na.rm = TRUE))|>
    # Order rows by social quantile
    dplyr::arrange(social_quantile)|>
    # Calculate impact rate based on population and impact
    # Not summing!
    dplyr::mutate(impact_rate = impact_sum / population_sum * 1e5, .before = impact_rate_std)



  # * * other parameters (beyond impact rates) #################

  ## These parameters do not need to aggregate in two steps
  ## Only one step by quantile

  ## Define first the variables for if statements
  has_bhd <- "bhd" %in% base::names(input_data_with_quantile)
  has_population <- "population" %in% base::names(input_data_with_quantile)
  has_exp <- "exp" %in% base::names(input_data_with_quantile)
  has_pop_fraction <- "pop_fraction" %in% base::names(input_data_with_quantile)


  ## Define function to get_other_parameters()
  get_other_parameters <- function(df){

    other_parameters <- df |>

      dplyr::summarize(
        impact_mean = base::mean(impact, na.rm = TRUE),
        bhd_sum = if (has_bhd) base::sum(bhd, na.rm = TRUE) else NULL,
        population_sum = if (has_population) base::sum(population, na.rm = TRUE) else NULL,
        bhd_mean = if (has_bhd) base::mean(bhd, na.rm = TRUE) else NULL,
        exp_mean = if (has_exp & is.numeric(exp)) base::mean(exp, na.rm = TRUE) else NULL, # In absolute risk exp is string (pasted)
        exp_sd = if (has_exp & is.numeric(exp)) stats::sd(exp, na.rm = TRUE) else NULL, # In absolute risk exp is string (pasted)
        pop_fraction_mean = if (has_pop_fraction) base::mean(pop_fraction, na.rm = TRUE) else NULL,
        .groups = "drop") |>
        dplyr::mutate(
          bhd_rate = if (has_bhd && has_population) bhd_sum * 1e5 / population_sum else NULL
        )
    return(other_parameters)
  }


  other_parameters_by_quantile <-
    input_data_with_quantile |>
    ## Group by social_quantile
    dplyr::group_by(social_quantile) |>
    get_other_parameters()

  ## All parameters together
  ## Put together input rates and other parameters
  parameters_by_quantile <-
    dplyr::left_join(impact_rates_by_quantile,
                     other_parameters_by_quantile,
                     # common columns
                     by = c("social_quantile", "population_sum"))

  # * _overall ############

  # * * impact_rates #################

  impact_rates_overall <-
    ## Two steps but for the overall level impact_rates_by_quantile_and_age can be reused
    impact_rates_by_quantile_and_age |>
    # group_by() instead of .by= to keep groups in summarize() and mutate() below
    dplyr::group_by(age_group, age_order, ref_prop_pop) |>
    ## Without grouping because it is overall
    dplyr::summarize(
      impact_sum = base::sum(impact_sum, na.rm = TRUE),
      population_sum = base::sum(population_sum, na.rm = TRUE)) |>
    dplyr::mutate(
      impact_rate = impact_sum / population_sum * 1E5,
      impact_rate_std = impact_rate * ref_prop_pop) |>
    dplyr::ungroup() |>
    # Now total
    dplyr::summarize(
      impact_sum = base::sum(impact_sum, na.rm = TRUE),
      population_sum = base::sum(population_sum, na.rm = TRUE),
      impact_rate_std = base::sum(impact_rate_std, na.rm = TRUE)) |>
    dplyr::mutate(
      impact_rate = impact_sum / population_sum * 1E5)



  # * * other parameters (beyond impact rates) #################
  other_parameters_overall <-
    input_data_with_quantile |>
    ## Without grouping because it is overall
    get_other_parameters()

  ## All parameters together
  parameters_overall <-
    dplyr::left_join(impact_rates_overall,
                     other_parameters_overall,
                     # common columns
                     by = c("population_sum"))

  ## Prepared to be joined below
  parameters_overall_prepared <-
    parameters_overall |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "parameter",
      values_to = "overall")


  # Find differences between quantiles ##################

  social_calculation <-
    parameters_by_quantile |>
    ## Pivot longer to prepare the data and have a column for parameter
    tidyr::pivot_longer(cols = -social_quantile,
                        names_to = "parameter",
                        values_to = "value") |>
    ## Put column parameter first
    dplyr::select(parameter, dplyr::everything()) |>
    ## Order columns by parameter
    dplyr::arrange(parameter) |>
    ## Obtain the first (most deprived) and last (least deprived) values
    ## for each parameter
    dplyr::summarize(
      .by = parameter,
      first = dplyr::first(value),
      last = dplyr::last(value)) |>
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
    ## Filter for relevant rows
    dplyr::filter(
      parameter %in% c("exp_mean",
                       "bhd_rate",
                       "pop_fraction_mean",
                       "impact_rate",
                       "impact_rate_std")) |>
    ## Long instead of wide layout
    tidyr::pivot_longer(
      cols = dplyr::contains("_"),
      names_to = c("difference_type", "difference_compared_with"),
      values_to = "difference_value",
      names_sep = "_") |>
    dplyr::mutate(
      ## Write the readable names fo the parameters
      parameter_string =
        dplyr::case_when(
          base::grepl("exp_", parameter) ~ "exposure",
          base::grepl("bhd_", parameter) ~ "baseline health data",
          base::grepl("pop_fraction_", parameter) ~ "population attributable fraction",
          base::grepl("impact_", parameter) ~ "impact"),
      ## Replace "quantile" with "last_quantile"
      difference_compared_with =
        base::gsub("quantile", "last_quantile", difference_compared_with),
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
      base::list(health_main = output_attribute[["health_main"]],
                 health_detailed = output_attribute[["health_detailed"]])

    ## * If NOT available output_attribute, i.e. if argument impact #######

  } else if (has_impact ){

    output_social <-
      base::list()
  }

  output_social[["social_main"]] <-
    social_results |>
    ## Keep only impact_rate_std as parameter
    ## This is the most relevant result.
    dplyr::filter(parameter == "impact_rate_std")

  ## The other parameters can be stored in detailed
  ## (just in case some users have interest on this)
  output_social[["social_detailed"]][["results_all_parameters"]] <- social_results
  output_social[["social_detailed"]][["parameters_per_quantile"]] <- parameters_by_quantile
  output_social[["social_detailed"]][["parameters_overall"]] <- parameters_overall
  output_social[["social_detailed"]][["input_data_with_quantile"]] <- input_data_with_quantile


  return(output_social)

}
