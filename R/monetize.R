#' Monetize health impacts

# DESCRIPTION ##################################################################
#' @description
#' This function monetizes health impacts

# ARGUMENTS ####################################################################
#' @param output_attribute \code{List} produced by \code{healthiar::attribute()} or \code{healthiar::compare()} as results.
#' @param impact \code{Numberic value} referring to the health impacts to be monetized (without attribute function). If a \code{Numberic vector} is entered multiple assessments (by year) will be carried out. Be aware that the value for year 0 (current) must be entered, while n_years does not include the year 0. Thus, length of impact = n_years + 1.
#' @param valuation \code{Numberic value} referring to unit value of a health impact.
#' @param discount_rate \code{Numeric value} showing the discount rate for future years. If it is a nominal discount rate, no inflation is to be entered. If it is a real discount rate, the result can be adjusted by entering inflation in this function.
#' @param discount_shape \code{String} referring to the assumed equation for the discount factor. By default: "exponential". Otherwise: "hyperbolic_harvey_1986" or "hyperbolic_mazur_1987".
#' @param n_years \code{Numeric value} referring to number of years in the future to be considered in the discounting and/or inflation. Be aware that the year 0 (without discounting/inflation, i.e. the present) is not be counted here. If a vector is entered in the argument impact, n_years does not need to be entered (length of impact = n_years + 1)
#' @param inflation_rate \code{Numeric value} between 0 and 1 referring to the annual inflation (increase of prices). Only to be entered if nominal (not real) discount rate is entered in the function. Default value = NULL (assuming no nominal discount rate)
#' @param info \code{String}, \code{data frame} or \code{tibble} providing \strong{information about the assessment}. Only attached if \code{impact} is entered by the users. If \code{output_attribute} is entered, use \code{info} in that function or add the column manually. \emph{Optional argument.}

# DETAILS ######################################################################

#' @details
#' \strong{Equation inflation factor (without discounting)}
#' @details
#' \deqn{inflation\_factor = (1 + inflation\_rate)^{n\_years}}

#' @details
#' \strong{Equations discount factors (without inflation)}
#' @details
#' \emph{Exponential discounting (no inflation)}
#' \deqn{discount\_factor = \frac{1}{(1 + discount\_rate) ^{n\_years}}}
#' @details
#' \emph{Hyperbolic discounting Harvey (no inflation)}
#' \deqn{discount\_factor = \frac{1}{(1 + n\_years)^{discount\_rate}}}
#' \emph{Hyperbolic discounting Mazure (no inflation)}
#' \deqn{discount\_factor = \frac{1}{(1 + (discount\_rate \times n\_years)}}

#' @details
#' \strong{Equations discount factors with inflation}
#' @details
#' \emph{Exponential discounting (with inflation)}
#' \deqn{discount\_and\_inflation\_factor = \frac{1}{((1 + discount\_rate) \times (1 + inflation\_rate)) ^{n\_years}}}
#' @details
#' \emph{Hyperbolic discounting Harvey (with inflation)}
#' \deqn{discount\_and\_inflation\_factor = \frac{1}{(1 + n\_years)^{discount\_rate} \times (1 + inflation\_rate)^{n\_years}}}
#' \emph{Hyperbolic discounting Mazure (with inflation)}
#' \deqn{discount\_and\_inflation\_factor = \frac{1}{(1 + (discount\_rate \times n\_years) \times (1 + inflation\_rate)^{n\_years}}}

# VALUE ########################################################################
#' @returns
#' This function returns two lists:
#' \itemize{
#'  \item \code{monetization_main} contains the central monetization estimate and the corresponding 95% confidence intervals
#'  \item \code{monetization_detailed} contains the monetized results for each unique combination of the input variable estimates that were provided to the initial \code{attribute_health()} call
#'  }
#' If the argument \code{output_attribute} was specified, then the two lists are added next to the existing attribute output.

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

#' @author Alberto Castro & Axel Luyten

#' @export



monetize <- function(output_attribute = NULL,
                     impact = NULL,
                     valuation,
                     discount_rate = NULL,
                     discount_shape = "exponential",
                     n_years = 0,
                     inflation_rate = NULL,
                     info = NULL) {

  # Define variables ####

  # Store variables to increase readability of conditions
  using_impact_from_healthiar <-
    !base::is.null(output_attribute) & base::is.null(impact)
  using_impact_from_user <-
    !using_impact_from_healthiar

  using_impact_vector_from_user <- base::length(impact)>1

  # is_lifetable only can exist if output_attribute is provided
  # and then it has to be checked of is_lifetable is TRUE or FALSE
  if(! base::is.null(output_attribute)){
    is_lifetable <-
      base::unique(output_attribute[["health_detailed"]][["input_table"]]$is_lifetable)
    # Witout output_attribute, no life table
  } else { is_lifetable <- FALSE}

  is_not_lifetable <- ! is_lifetable



  # If a vector is entered in impact
  # The discount years are already defined by the lenght of the vector
  # Users do not need to enter it.
  if(using_impact_vector_from_user){
    n_years <- base::length(impact)-1
  }

  # Validate input data ####

  ## Error if value lower than 0 ####
  for(var_name in c("valuation", "n_years")){

    if(!is.null(base::get(var_name)) &&
       base::get(var_name) < 0){

      stop(paste0(var_name, " must be higher than 0."),
           call. = FALSE)
    }

  }

  ## Error if value higher than 1 and lower than 0 ####
  for(var_name in c("discount_rate", "inflation_rate")){

    if(!is.null(base::get(var_name)) &&
       (base::get(var_name) < 0 | base::get(var_name) > 1)){

      stop(base::paste0(var_name, " must be higher than 0 and lower than 1."),
           call. = FALSE)
    }

  }



  ## Error if values for both impact and output_attribute are passed ####

  if(!base::is.null(impact) && !is.null(output_attribute)){
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


  ## Warning if no value for n_years, but discount_rate####

  # Then discount values are ignored because no discount is happening (by default `n_years = 0`)
  # discount_shape has a default value, so it is never NULL
  if(n_years == 0 &&
     base::any(!base::is.null(discount_rate))&&
     # Exclude life table because the n_years are calculated based on life table
     !is_lifetable){
    warning(
      base::paste0("You entered some value in discount_rate,",
                   " but n_years is 0 (default value).",
                   " Therefore no discount is applied."),
      call. = FALSE)
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

  if("n_years" %in% base::names(base::match.call()) &&
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

  if("n_years" %in% base::names(base::match.call()) &&
     is_lifetable){
    warning(
      base::paste0("n_years is aimed for any output_attribute",
                   " and for impact with single value (no vector).",
                   " Therefore n_years is ignored here and the length life table is used instead."),
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

      # Calculation preparation ####
      # If df has only one column (impact)
      # it means that this is the direct input from user
      # no previous health assessment with healthiar
      using_impact_from_user <- base::ncol(df) == 1
      using_impact_vector <- base::length(df$impact)>1
      using_impact_vector_from_user <- using_impact_from_user & using_impact_vector
      using_impact_value_from_user <- using_impact_from_user & !using_impact_vector
      using_impact_from_healthiar <- !using_impact_from_user
      using_lifetable <- "year" %in% base::names(df)
      using_impact_from_healthiar_with_lifetable <- using_impact_from_healthiar & using_lifetable
      using_impact_from_healthiar_without_lifetable <- using_impact_from_healthiar & !using_lifetable

      # Definition of calculation pathways
      taking_last_discounted_year <- using_impact_from_healthiar_without_lifetable | using_impact_value_from_user
      summing_across_discounted_years <- using_impact_vector_from_user | using_impact_from_healthiar_with_lifetable

      # Define discount years
      n_years_vector <- 0 : n_years

      df_with_input <-
        df |>
        # Add columns for input data in the table
        # Use {{}} to clarify the it refers to the argument and not to the column
        # with the same name
        dplyr::mutate(valuation = {{valuation}},
                      discount_rate = {{discount_rate}},
                      n_years = {{n_years}},
                      discount_shape = {{discount_shape}},
                      inflation_rate = inflation_rate) |>
        # Add info
        healthiar:::add_info(
          info = info
        )

      # If impact is inserted as vector to refer to different monetized impacts by year
      # (case of real costs, not applicable for nominal costs)

      if(summing_across_discounted_years){

        df_by_year <-  df_with_input
        df_by_year$year <-
          base::rep(n_years_vector, len = base::nrow(df_with_input))

      } else if(taking_last_discounted_year){
        df_by_year <-
          # Split by discount year
          dplyr::cross_join(x = tibble::tibble(year = n_years_vector),
                            y = df_with_input)
      }

      df_by_year <-
        df_by_year |>
        # Add inflation factor ####
      dplyr::mutate(
        inflation_factor =
          healthiar::get_inflation_factor(
            n_years = year,
            inflation_rate = inflation_rate),
        # Add discount factor ####
        discount_factor =
          healthiar::get_discount_factor(
            discount_rate = discount_rate,
            n_years = year,
            discount_shape = discount_shape,
            inflation_rate = inflation_rate),
        # Add monetized impact ####
        monetized_impact = impact * valuation * inflation_factor * discount_factor,
        monetized_impact_without_discount_and_inflation = impact * valuation,
        .after = impact)



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
      }else if(summing_across_discounted_years){

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
          monetization_detailed = df_by_year
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

      n_years <- base::max(impact_detailed$year) - base::unique(impact_detailed$year_of_analysis)


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
          impact_rounded = round(impact),
          monetized_impact_rounded = round(monetized_impact))


      # Calculate impact per 100K inhab.

      if("population" %in% base::colnames(impact_detailed)){
        impact_detailed <-
          impact_detailed |>
          dplyr::mutate(
            impact_per_100k_inhab = (impact / population) *1E5
          )
      }

      # Remove nested tibbles to prevent that rows are not summed correctly in
      # get_output() below

      impact_detailed <- impact_detailed |>
        dplyr::select(-dplyr::contains("_by_"))



      # Get the main and detailed output by aggregating and/or filtering cases (rows)
      output_monetization <-
        healthiar:::get_output(results_raw = impact_detailed) |>
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

      # If bhd_central is not NULL, then we are not using life table method

      # Duplicate output to work with monetization
      output_monetization <-
        output_attribute

      # Apply the function in main and detailed results
      output_monetization[["monetization_main"]] <-
        add_monetized_impact(df = output_attribute[["health_main"]],
                             valuation = valuation,
                             discount_rate = discount_rate,
                             n_years = {{n_years}},
                             discount_shape = discount_shape,
                             inflation_rate = inflation_rate)[["monetization_main"]]

      #Detailed results showing the by-year results of monetization
      output_monetization[["monetization_detailed"]][["by_year"]] <-
        add_monetized_impact(df = output_attribute[["health_main"]],
                             valuation = valuation,
                             discount_rate = discount_rate,
                             n_years = {{n_years}},
                             discount_shape = discount_shape,
                             inflation_rate = inflation_rate)[["monetization_detailed"]]

      #Detailed results showing all the details of the health results
      output_monetization[["monetization_detailed"]][["health_raw"]]<-
        add_monetized_impact(df = output_attribute[["health_detailed"]][["results_raw"]],
                             valuation = valuation,
                             discount_rate = discount_rate,
                             n_years = {{n_years}},
                             discount_shape = discount_shape,
                             inflation_rate = inflation_rate)[["monetization_main"]]
    }


    # For both with and without life table
    # Identify the relevant columns for monetization that are in the output
    relevant_columns <-
      c("info", "geo_id_micro", "geo_id_macro",
        base::paste0("impact", c("", "_before_inflation_and_discount", "_after_inflation_and_discount")),
        "discount_rate", "discount_shape",
        "valuation",
        base::paste0("monetized_impact", c("", "_without_inflation_and_discount", "_rounded")))

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
