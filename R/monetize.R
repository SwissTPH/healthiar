#' Monetize health impacts

# DESCRIPTION ##################################################################
#' @description
#' This function monetizes health impacts

# ARGUMENTS ####################################################################
#' @param output_attribute \code{List} produced by \code{healthiar::attribute_health()}, \code{healthiar::attribute_lifetable()} or \code{healthiar::compare()} as results.
#' @param impact \code{Numberic value} referring to the health impacts to be monetized (without attribute function). If a \code{Numberic vector} is entered multiple assessments (by year) will be carried out. Be aware that the value for year 0 (current) must be entered, while n_years does not include the year 0. Thus, length of impact = n_years + 1.
#' @param valuation \code{Numberic value} referring to unit value of a health impact.
#' @param discount_rate \code{Numeric value} showing the discount rate for future years.
#' @param discount_shape \code{String} referring to the assumed equation for the discount factor. By default: \code{"exponential"}. Otherwise: \code{"hyperbolic_harvey_1986"} or \code{"hyperbolic_mazur_1987"}.
#' @param n_years \code{Numeric value} referring to number of years in the future to be considered in the discounting and/or inflation. Be aware that the year 0 (without discounting/inflation, i.e. the present) is not be counted here. If a vector is entered in the argument impact, n_years does not need to be entered (length of impact = n_years + 1).
#' @param inflation_rate \code{Numeric value} between 0 and 1 referring to the annual inflation (increase of prices). Default value = NULL (assuming no nominal discount rate).
#' @param discount_is_nominal \code{Boolean value} (i.e. TRUE vs. FALSE).
#' If \code{TRUE} (default), the \code{discount_rate} is assumed to be nominal (includes inflation).
#' The \code{inflation_rate} will then be used as a deflator to convert
#' future impacts into constant prices (Green Book approach).
#' If \code{FALSE}, the \code{discount_rate} is assumed to be real.
#' In this case, providing an \code{inflation_rate} will grow the valuation over time
#' (Social Welfare approach).
#' @param info \code{String}, \code{data frame} or \code{tibble} providing \strong{information about the assessment}. Only attached if \code{impact} is entered by the users. If \code{output_attribute} is entered, use \code{info} in that function or add the column manually. \emph{Optional argument.}

# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' This function monetize health impacts valuating them and
#' applying discounting \insertCite{Frederick2002_jel,Harvey1986_ms,Mazur1987_book}{healthiar}
#' and/or inflation \insertCite{Brealey2023_book}{healthiar}.
#'
#' The function handles inflation based on the \code{discount_is_nominal} argument:
#' \itemize{
#'   \item \strong{Growth Pathway} (\code{discount_is_nominal = FALSE}): Used when the
#'   valuation is expected to grow over time (e.g., rising societal value of health).
#'   In that case, the valuation will be inflated
#'   \insertCite{OECD2012_book}{healthiar}.
#'   \item \strong{Deflator Pathway} (\code{discount_is_nominal = TRUE}): Used when
#'   the input valuation is already a future nominal price.
#'   In that case, the valuation will be deflated to return to constant Year 0 prices
#'  \insertCite{HMTreasury2022_greenbook}{healthiar}.
#' }
#'
#' One of the following three discount shapes can be selected:
#' \itemize{
#'  \item Exponential \insertCite{Frederick2002_jel}{healthiar}
#'  \item Hyperbolic as \insertCite{Harvey1986_ms;textual}{healthiar}
#'  \item Hyperbolic as \insertCite{Mazur1987_book;textual}{healthiar}}
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#monetization}{Monetization}}
#'
# VALUE ########################################################################
#' @returns
#' This function returns a \code{list} containing:
#'
#' 1) \code{monetization_main} (\code{tibble}) containing the main monetized results;
#' \itemize{
#'  \item \code{monetized_impact} (\code{numeric} column)
#'  \item \code{discount_factor} (\code{numeric} column) calculated based on the entered \code{discount_rate}
#'  \item And many more
#' }
#'
#' 2) \code{monetization_detailed} (\code{list}) containing detailed (and interim) results.
#' \itemize{
#'  \item \code{results_by_year} (\code{tibble})
#'  \item \code{health_raw} (\code{tibble}) containing the monetized results for each for each combination of input uncertainty that were provided to the initial \code{attribute_health()} call
#' }
#'
#' If the argument \code{output_attribute} was specified, then the two results elements are added to the existing output.
#'
# EXAMPLES #####################################################################
#' @examples
#' # Goal: monetize the attributable impacts of an existing healthiar
#' # assessment
#' output_attribute <- attribute_health(
#' erf_shape = "log_linear",
#' rr_central = exdat_pm$relative_risk,
#' rr_increment = 10,
#' exp_central = exdat_pm$mean_concentration,
#' cutoff_central = exdat_pm$cut_off_value,
#' bhd_central = exdat_pm$incidence
#' )
#'
#' results <- monetize(
#'   output_attribute = output_attribute,
#'   discount_shape = "exponential",
#'   discount_rate = 0.03,
#'   n_years = 5,
#'   valuation = 50000 # E.g. EURO
#' )
#'
#' # Attributable COPD cases its monetized impact
#' results$monetization_main |>
#'   dplyr::select(impact, monetized_impact)
#'
#'
#' @seealso
#' \itemize{
#'   \item Upstream: \code{\link{attribute_health}}, \code{\link{attribute_lifetable}}, \code{\link{compare}}
#'   \item Alternative: \code{\link{get_inflation_factor}},
#'     \code{\link{get_discount_factor}}, \code{\link{cba}}
#' }
#'
#'
#' @references
#'
#' \insertAllCited{}
#'
#'
#' @author Alberto Castro & Axel Luyten
#'
#' @export



monetize <- function(output_attribute = NULL,
                     impact = NULL,
                     valuation,
                     discount_rate = NULL,
                     discount_shape = "exponential",
                     n_years = NULL,
                     inflation_rate = NULL,
                     discount_is_nominal = TRUE,
                     info = NULL) {


  # Store input_args
  input_args <-
    get_input_args(environment = base::environment(),
                   call = match.call())

  # Define variables ####
  # Store variables to increase readability of conditions
  #from healthiar
  using_impact_from_healthiar <-
    !base::is.null(output_attribute) & base::is.null(impact)



  # Using compare() before monetize()
  is_compare <-
    "input_args_scen_1" %in% base::names(output_attribute$health_detailed$input_args)

  if(is_compare){

    approach_comparison <-
      base::unique(output_attribute$health_detailed$input_args$approach_comparison)

  } else {
    approach_comparison <- "no_comparison"
  }


  # is_lifetable only can exist if output_attribute is provided
  # and then it has to be checked of is_lifetable is TRUE or FALSE

  if (!base::is.null(output_attribute)) {

    input_table <- output_attribute[["health_detailed"]][["input_table"]]

    # If after attribute_x(), then input table is a tibble
    if (!is_compare) {
      is_lifetable <- base::unique(input_table$is_lifetable)

    # If after compare(), input table is a list
    } else if (approach_comparison == "delta") {

        # When delta, two input tables (one per scenario)

        is_lifetable <- base::unique(input_table[["input_table_scen_1"]]$is_lifetable)

      } else { # If approach comparison "pif" or if no_comparison

        is_lifetable <- base::unique(input_table$is_lifetable)
      }

  } else {
    is_lifetable <- FALSE
    }



  is_not_lifetable <- ! is_lifetable


  # With and without lifetable
  using_impact_from_healthiar_with_lifetable <-
    using_impact_from_healthiar & is_lifetable
  using_impact_from_healthiar_without_lifetable <-
    using_impact_from_healthiar & is_not_lifetable


  # Impact from user input
  using_impact_from_user <- !using_impact_from_healthiar
  # Pay attention: one is vector (multiple values) and the other value (single value)
  using_impact_vector_from_user <- using_impact_from_user & base::length(impact)>1
  using_impact_value_from_user <- using_impact_from_user & !using_impact_vector_from_user


  # Definition of calculation pathways
  # In the case of no life table and single value, only the monetized value of the last year is taken
  taking_last_discounted_year <-
    using_impact_from_healthiar_without_lifetable | using_impact_value_from_user
  # In the case of life table or multiple impact values entered, results must be summed
  summing_across_years <-
    using_impact_vector_from_user | using_impact_from_healthiar_with_lifetable



  # If a vector is entered in impact
  # The discount years are already defined by the length of the vector
  # Users do not need to enter it.
  if(using_impact_vector_from_user){
    n_years <- base::length(impact)-1
  }

  # Validate input data ####

  ## Error if value lower than 0 ####
  for(var_name in c("valuation", "n_years")){

    if(!base::is.null(base::get(var_name)) &&
       base::get(var_name) < 0){

      stop(base::paste0(var_name, " must be higher than 0."),
           call. = FALSE)
    }

  }

  ## Error if value higher than 1 and lower than 0 ####
  for(var_name in c("discount_rate", "inflation_rate")){

    if(!base::is.null(base::get(var_name)) &&
       (base::get(var_name) < 0 | base::get(var_name) > 1)){

      stop(base::paste0(var_name, " must be higher than 0 and lower than 1."),
           call. = FALSE)
    }

  }



  ## Error if values for both impact and output_attribute are passed ####

  if(!base::is.null(impact) && !base::is.null(output_attribute)){
    stop(base::paste0("Enter a value for impact or for output_attribute but not both."),
         call. = FALSE)
  }

  ## Error if no right category is passed passed ####

  if(!discount_shape %in%
     c("exponential", "hyperbolic_harvey_1986", "hyperbolic_mazur_1987")){

    stop(base::paste0("Please, check spelling. discount_shape must have one of this values: ",
                      "exponential, hyperbolic_harvey_1986, hyperbolic_mazur_1987."),
         call. = FALSE)
  }

  ## Error if different year of analysis in life table approach ####

  # if different year of analysis in scen_1 and scen_2


  if(is_compare){

    # Store values of scenarios
    arg_values_scen_1 <- output_attribute$health_detailed$input_args$input_args_scen_1$value
    arg_values_scen_2 <- output_attribute$health_detailed$input_args$input_args_scen_2$value


    error_if_different_baseline <- function(var){
      # If year of analysis are different
      if(!base::identical(arg_values_scen_1[v], arg_values_scen_2[v])){

        # Error because monetize() aims to monetize health impacts from interventions
        # and health impacts from different years cannot be attributed to the intervention

        stop(
          base::paste0("Please, enter the same ", v ,
                       " in both scenarios of the healthiar function compare. ",
                       "Otherwise, the monetization cannot be attributed to an intervention."),
          call. = FALSE)
      }
    }

    for(v in c("bhd_central", "bhd_lower", "bhd_upper")){

      error_if_different_baseline(var = v)

    }

    if(is_lifetable){

      for(v in c("year_of_analysis")){

        error_if_different_baseline(var = v)

      }
    }
  }



  #### error_if_info_with_incompatible_length ####

  if(! base::is.null(info) &&
     ! base::is.null(impact)){

    if(base::is.data.frame(info)){
      length_info <- base::nrow(info)
    } else if (base::is.vector(info)){
      length_info <- base::length(info)
    }

    if( !length_info == base::length(impact) && !length_info == 1){
      base::stop(
        base::paste0("The info vector or data frame columns must have a length of 1 or the same length as impact."),
        call. = FALSE
      )
    }
  }




  ## Warning if user pass n_years with impact ####

  # Then the value will be ignored and the length of impact will be used as n_years

  if( ! base::is.null(input_args$value$n_years)  &&
     base::length(impact) > 1 &&
     !base::is.null(impact)){
    warning(
      base::paste0("n_years is aimed for output_attribute (excluding life table)",
      " and for impact (excluding vector form).",
      " Therefore n_years is ignored here and the length of the vector impact is used instead."),
      call. = FALSE)
  }

  ## Warning if user pass n_years with impact ####

  # Then the value will be ignored and the length of impact will be used as n_years



  if( ! base::is.null(input_args$value$n_years) &&
     is_lifetable){
    warning(
      base::paste0("n_years is aimed for any output_attribute",
                   " and for impact with single value (no vector).",
                   " Therefore n_years is ignored here and the length life table is used instead."),
      call. = FALSE)
  }




  ## Warning if no value for n_years, but discount_rate####

  # Then discount values are ignored because no discount is happening (by default `n_years = 0`)
  # discount_shape has a default value, so it is never NULL
  if(base::is.null(n_years) &&
     base::any(!base::is.null(discount_rate))&&
     # Exclude life table because the n_years are calculated based on life table
     !is_lifetable){
    warning(
      base::paste0("You entered some value in discount_rate,",
                   " but n_years is 0 (default value).",
                   " Therefore no discount is applied."),
      call. = FALSE)
  }






  # Monetize ####

  ## Create function add_monetized_impact() ###############
  # To be used below

  add_monetized_impact  <-
    function(df,
             valuation,
             discount_rate,
             n_years,
             discount_shape,
             inflation_rate,
             info = NULL) {

      # Create function to obtain the real_discount_rate
      # i.e. discount_rate adjusted for inflation



      # Define discount years
      if(base::is.null(n_years)){
        n_years_vector <- 0
      } else {
        n_years_vector <- 0:n_years}

      df_with_input <-
        df |>
        # Add columns for input data in the table
        dplyr::mutate(valuation = valuation,
                      discount_rate = discount_rate,
                      n_years = n_years,
                      discount_shape = discount_shape,
                      inflation_rate = inflation_rate) |>
        # Add info
        add_info(info = info)


      # Add year
      if(summing_across_years){
        # If lifetable or
        # if impact is inserted as vector to refer to different monetized impacts by year
        # (case of real costs, not applicable for discount_is_nominal costs)

        df_by_year <-  df_with_input
        df_by_year$year <-
          base::rep(n_years_vector, len = base::nrow(df_with_input))

      } else if(taking_last_discounted_year){
        df_by_year <-
          # Split by year
          dplyr::cross_join(x = tibble::tibble(year = n_years_vector),
                            y = df_with_input)
      }


      df_by_year <-
        df_by_year |>
        dplyr::mutate(
          # 1. Project: Future Nominal Value (Inflation Growth)
          inflation_factor = get_inflation_factor(
            n_years = year,
            inflation_rate = inflation_rate,
            is_deflation = FALSE
          ),

          # 2. Standardize: Convert back to Real terms
          # We only apply the deflator if the user wants a "Real" present value (discount_is_nominal = FALSE)
          deflator = if(discount_is_nominal == TRUE) {
            get_inflation_factor(n_years = year,
                                 inflation_rate = inflation_rate,
                                 is_deflation = TRUE)
          } else {
            1
          },

          # 3. Discount: Apply time preference
          discount_factor = get_discount_factor(
            discount_rate = if(base::is.null(discount_rate)) 0 else discount_rate,
            n_years = year,
            discount_shape = discount_shape
          ),

          # 4. Final Calculation
          # If discount_is_nominal = TRUE: Value grows by inflation, then is discounted.
          # If discount_is_nominal = FALSE: Growth and Deflator cancel out (Green Book standard).
          monetized_impact = impact * valuation * inflation_factor * deflator * discount_factor,

          monetized_impact_without_discount_and_inflation = impact * valuation,
          .after = impact
        )



      # If taking last discounted year ####
      if(taking_last_discounted_year){
        df_relevant <-
          df_by_year|>
          # Keep only the last year
          dplyr::filter(year == max(year)) |>
          # Remove the variable discount year because it is not anymore relevant
          # (not by-year results)
          dplyr::select(-year)

        # If summing across discounted years ####
      }else if(summing_across_years){

        grouping_variables <-
          df_by_year |>
          dplyr::select(-dplyr::any_of(c("year")),
                        -dplyr::contains("discount_factor"),
                        -dplyr::contains("impact")) |>
          base::names()

        df_relevant <-
          df_by_year |>
          dplyr::summarize(
            .by = dplyr::any_of(grouping_variables),
            dplyr::across(dplyr::contains("impact"), sum)
          )
      }

      monetization_main <-
        df_relevant |>
        # Round monetized impacts
        dplyr::mutate(
          monetized_impact_rounded = base::round(monetized_impact),
          .after = monetized_impact)

      ##### Output ####
      monetization <-
        base::list(
          monetization_main = monetization_main,
          monetization_detailed = base::list(results_by_year = df_by_year)
        )

      return(monetization)

    }


  #* IF OUTPUT of attribute ####

  if(using_impact_from_healthiar){

    ##** IF LIFE TABLE method for the health assessment #######

    # If life table.
    if(is_lifetable){

      health_outcome <-
        output_attribute[["health_detailed"]][["input_args"]][["value"]]$health_outcome

      # Store the original data (they refer to health)
      output_health <- output_attribute

      # Obtain n_years
      # Ignore user defined n_years
      # Here the difference between year of analysis and
      # last year of mortality data is to be used
      impact_detailed <- output_health[["health_detailed"]][["results_by_year"]] |>
        dplyr::mutate(
        # Convert year to numeric
        year = base::as.numeric(year))

      # Extract year of analysis
      # If monetizing after compare(), then take year_of_analysis_scen_1
      # year_of_analysis must be the same in scen_1 and scen_2 to be able to monetize
      # See validation above

      if(approach_comparison == "delta"){

          year_of_analysis <- base::unique(impact_detailed$year_of_analysis_scen_1)

        #If monetizing after attribute, then just take the value
      } else { #If pif or no_comparison
        year_of_analysis <- base::unique(impact_detailed$year_of_analysis)
      }


      n_years <- base::max(impact_detailed$year) - year_of_analysis


      # Output will be adapted according to monetized impacts
      impact_detailed <-
        impact_detailed |>
        ## Calculate total, discounted life years (single value) per sex & ci
        dplyr::mutate(
          n_years = n_years,
          discount_rate = discount_rate,
          discount_shape = discount_shape,
          inflation_rate = inflation_rate)

      impact_detailed  <-
        add_monetized_impact(
          df = impact_detailed,
          discount_rate = discount_rate,
          n_years = n_years,
          discount_shape = discount_shape,
          inflation_rate = inflation_rate,
          valuation = valuation)[["monetization_main"]]


      impact_detailed <- impact_detailed |>
        # Round results
        dplyr::mutate(
          # Round impacts and monetized impacts
          impact_rounded = base::round(impact),
          monetized_impact_rounded = base::round(monetized_impact))


      # Calculate impact per 100K inhab.

      if("population" %in% base::colnames(impact_detailed)){
        impact_detailed <-
          impact_detailed |>
          dplyr::mutate(
            impact_per_100k_inhab = (impact / population) *1E5
          )
      }

      # Get the main and detailed output by aggregating and/or filtering cases (rows)
      output_monetization <-
        get_output(results_raw = impact_detailed) |>
        # Rename the list elements (not anymore health but health including monetization)
        stats::setNames(c("monetization_main", "monetization_detailed"))

      # Keep only the main detailed data frame (raw) for monetization
      output_monetization[["monetization_detailed"]] <-
        output_monetization[["monetization_detailed"]][["results_raw"]]

      # Add the list elements health_main and health_detailed
      output_monetization <-
        c(output_health,
          output_monetization)

    }else if (is_not_lifetable){

      ##** IF WITHOUT LIFE TABLE #######

      # Monetize impacts using health main
      output_monetization_health_main <-
        add_monetized_impact(df = output_attribute[["health_main"]],
                             valuation = valuation,
                             discount_rate = discount_rate,
                             n_years = n_years,
                             discount_shape = discount_shape,
                             inflation_rate = inflation_rate)

      # Put together health and monetization output
      output_monetization <-
        c(output_attribute, output_monetization_health_main)


      #Detailed results showing all the details of the health results
      output_monetization[["monetization_detailed"]][["health_raw"]]<-
        add_monetized_impact(df = output_attribute[["health_detailed"]][["results_raw"]],
                             valuation = valuation,
                             discount_rate = discount_rate,
                             n_years = n_years,
                             discount_shape = discount_shape,
                             inflation_rate = inflation_rate)[["monetization_main"]]
    }


    # For both with and without life table
    # Identify the relevant columns for monetization that are in the output
    relevant_columns <-
      c("info", "geo_id_micro", "geo_id_macro",
        "impact",
        "discount_rate", "discount_shape", "inflation_rate", "n_years",
        "valuation",
        base::paste0("monetized_impact", c("", "_without_discount_and_inflation", "_rounded")))

    # Keep only relevant columns for monetization
    output_monetization[["monetization_main"]] <-
      output_monetization[["monetization_main"]] |>
      dplyr::select(
        # The columns containing "_ci" are the uncertainties that define the rows
        dplyr::contains("_ci"),
        # Use any_of() instead of all_of() because depending on the calculation pathway
        # there might not be any of the relevant_columns
        dplyr::any_of(relevant_columns))


    #* IF USER INPUT ####

    # If the user only provide a number of the impact (not based on output of attribute)
    # No life table approach when user is entering the health impacts
    # because we cannot access the life table calculation to discount by year
    }else if(using_impact_from_user){

      output_monetization <-
        add_monetized_impact(
          df = tibble::tibble(impact = impact),
          valuation = valuation,
          discount_rate = discount_rate,
          n_years = n_years,
          discount_shape = discount_shape,
          inflation_rate = inflation_rate,
          info = info)

  }


  return(output_monetization)

}
