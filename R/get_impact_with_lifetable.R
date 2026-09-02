#' Get population impact over time

# DESCRIPTION ##################################################################
#' @description Get population impact over time

# ARGUMENTS ####################################################################
#' @param input_with_risk_and_pop_fraction \code{Data frame} with the input data (including risk and population fraction)

# VALUE ########################################################################
#' @returns
#' This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central estimate, lower and upper bound confidence interval).
#' Moreover, the data frame include columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }

#' @author Alberto Castro & Axel Luyten

#' @keywords internal



get_impact_with_lifetable <-
  function(input_with_risk_and_pop_fraction){

    # GET POP IMPACT ######

    # USEFUL VARIABLES ##########
    # yoa means Year Of Analysis
    yoa <- input_with_risk_and_pop_fraction |>  dplyr::pull(year_of_analysis) |> dplyr::first()
    yoa_plus_1 <- base::as.numeric(yoa) + 1

    health_outcome <- base::unique(input_with_risk_and_pop_fraction$health_outcome)

    is_deaths <- health_outcome == "deaths"
    is_yll <- health_outcome == "yll"

    is_single_year_exposure <- base::unique(input_with_risk_and_pop_fraction$approach_exposure) == "single_year"
    is_constant_exposure <- base::unique(input_with_risk_and_pop_fraction$approach_exposure) == "constant"

    is_with_newborns <- base::unique(input_with_risk_and_pop_fraction$approach_newborns) == "with_newborns"


    # Store the value of time horizon (defined in compile_input())
    time_horizon <- base::unique(input_with_risk_and_pop_fraction$time_horizon)
    # Define the last year of the projection
    last_year_projection <- yoa + time_horizon - 1
    # Define the years to be projected, i.e. the years after the year of
    # analysis that are still within the time horizon.
    # e.g. 2020 to 2118
    # seq_len() (and not yoa_plus_1:last_year_projection) because with a
    # time horizon of 1 the sequence with ":" would count backwards
    # (e.g. 2020, 2019) instead of covering no year at all
    years_projection <- yoa + base::seq_len(time_horizon - 1)
    # n_years_projection defines for how many years the population should be projected;
    n_years_projection <- base::length(years_projection)

    # Precompute column names to be use below
    entry_names <- base::paste0("entry_population_", years_projection)
    midyear_names   <- base::paste0("midyear_population_", years_projection)
    death_names <- base::paste0("deaths_", years_projection)


    # LIFETABLE SETUP ##############################################################################

    lifetable_calculation <- input_with_risk_and_pop_fraction |>
      dplyr::mutate(
        # Duplicate bhd  and year_of_analysis
        # for more handy column names for life table calculations
        deaths = bhd,
        yoa = year_of_analysis,
        # Create midyear_population_yoa
        # yoa means Year Of Analysis
        # It is better to do it  now (before nesting tables)
        midyear_population_yoa = population)


    lifetable_calculation <- lifetable_calculation |>
 
      dplyr::mutate(

        # Hazard rate for calculating survival probabilities
        hazard_rate = deaths / midyear_population_yoa,

        # Get modification factor
        # it works with both single exposure and exposure distribution
        modification_factor = 1 - pop_fraction,
        .after = rr) |>     

      # CALCULATE PROBABILITY OF SURVIVAL FROM START YEAR TO END YEAR & START YEAR TO MID YEAR
      dplyr::mutate(
        # probability of survival from start of year i to start of year i+1 (entry to entry)
        prob_survival =
          (midyear_population_yoa - (fraction_lived * deaths)) /
          (midyear_population_yoa + ((1 - fraction_lived) * deaths)),

        # Probability of survival from start to midyear
        # For example entry_pop = 100, prob_survival = 0.8 then end_of_year_pop = 100 * 0.8 = 80.
        # midyear_pop = 100 - (20/2) = 90.
        prob_survival_until_midyear = 1 - ((1 - fraction_lived) * (1 - prob_survival)),   
        .after = deaths) |>
      
       # CALCULATE ENTRY POPULATION OF YEAR OF ANALYSIS (YOA)
      dplyr::mutate(
        entry_population_yoa = midyear_population_yoa + ((1 - fraction_lived) * deaths),
        .before = midyear_population_yoa) 


    # CALCULATE MODIFIED SURVIVAL PROBABILITIES
    lifetable_calculation <- lifetable_calculation |>
      dplyr::mutate(
        # For age intervals between min_age and max_age, calculate modified
        # survival probabilities.
        # min_age and max_age are inclusive, i.e. the exposure affects
        # the age groups from min_age to max_age (both included).
        # If the user did not enter them, compile_input() sets them to the
        # first and the last age group, so that all age groups are affected
        # Calculate first the boolean/logic column to speed up calculations below
        is_exposed_age = age_end > min_age & age_start <= max_age,

        # Calculate modified hazard rate = modification factor * hazard rate = mod factor * (deaths / mid-year pop)
        hazard_rate_mod =
          dplyr::if_else(is_exposed_age,
                         modification_factor * hazard_rate,
                         hazard_rate),

        # Calculate modified survival probability
        prob_survival_mod =
          dplyr::if_else(is_exposed_age,
                         (1 - (fraction_lived * hazard_rate_mod)) / (1 + ((1 - fraction_lived) * hazard_rate_mod)),
                         prob_survival),

        prob_survival_until_midyear_mod =
          dplyr::if_else(is_exposed_age,
                         1 - ((1 - fraction_lived) * (1 - prob_survival_mod)),
                         prob_survival_until_midyear),

        .after = deaths)


    # Nest life tables
    lifetable_calculation <- lifetable_calculation |>
      tidyr::nest(
        data_by_age =
        c(yoa, age_group, age_start, age_end, bhd, deaths,
          population, fraction_lived, 
          modification_factor,
          prob_survival, prob_survival_until_midyear, hazard_rate,
          is_exposed_age, prob_survival_mod, prob_survival_until_midyear_mod, hazard_rate_mod,
          # These columns at the end to link with projections
          midyear_population_yoa, entry_population_yoa))


    ## PROJECTION OF THE YEAR OF ANALYSIS (YOA) #####################################################
    # The exposed projection is the scenario of "business as usual"
    # i.e. the scenario with the exposure to the environmental stressor as (currently) measured.
    # The unexposed projection is the scenario without any exposure to the environmental stressor,
    # i.e. it uses the modified survival probabilities and
    # therefore the mid-year population of the YOA has to be re-calculated.

    # Both scenarios share the same calculation steps,
    # they only differ in the survival probabilities used
    project_yoa <- function(df, prob_survival, prob_survival_until_midyear = NULL){

      # All multiplications with a probability of surviving are rounded
      # to avoid floating-point precision issues
      # (i.e. a result that should be zero ends up with e.g. 0.0000000000003).
      # The number of decimals is random, just large enough
      # to avoid changes in the final results

      # MID-YEAR POP = (entry population YOA) * (survival probability until mid year)
      # Only in the unexposed scenario (modified survival probabilities)
      if (!base::is.null(prob_survival_until_midyear)) {
        df$midyear_population_yoa <-
          base::round(df$entry_population_yoa * prob_survival_until_midyear, 10)
      }

      # End-of-year population YOA = (entry population YOA) * (survival probability)
      df$end_population_yoa <-
        base::round(df$entry_population_yoa * prob_survival, 10)

      # Deaths YOA = Entry pop YOA - End pop YOA
      df$deaths_yoa <- df$entry_population_yoa - df$end_population_yoa

      # Entry population YOA+1 = lag ( End-of-year population YOA )
      df$entry_population_yoa_plus_1 <- dplyr::lag(df$end_population_yoa)

      return(df)
    }

    lifetable_calculation <- lifetable_calculation |>
      dplyr::mutate(
        projection_if_exposed_by_age_and_year =
          purrr::map(
            .x = data_by_age,
            .f = ~ project_yoa(
              df = .x,
              prob_survival = .x$prob_survival)),

        projection_if_unexposed_by_age_and_year =
          purrr::map(
            .x = data_by_age,
            .f = ~ project_yoa(
              df = .x,
              prob_survival = .x$prob_survival_mod,
              prob_survival_until_midyear = .x$prob_survival_until_midyear_mod)))

    # PREMATURE DEATHS (SINGLE YEAR EXPOSURE) ######################################################
    # YOA = YEAR OF ANALYSIS
    if (is_deaths && is_single_year_exposure) {

      lifetable_calculation <- lifetable_calculation |>
        # Premature deaths = YOA end-of-year population of unexposed minus exposed
        dplyr::mutate(
          impact_by_age_and_year =
            purrr::map2(
              .x = projection_if_unexposed_by_age_and_year,
              .y = projection_if_exposed_by_age_and_year,
              .f = ~ {
                tibble::tibble(
                  age_start = .x$age_start,
                  age_end = .x$age_end,
                  # The population entered by the user (and not the mid-year
                  # population of the unexposed scenario) so that the column
                  # means the same as in the other pathways, where
                  # calculate_impact() also takes the inputted population
                  population = .x$population,
                  # Change of sign in the difference unexposed minus exposed
                  # because if no exposure
                  # there are less deaths in unexposed
                  # For population unexposed minus exposed (without change of sign)
                  # because there are more population in unexposed
                  impact_yoa = -(.x$deaths_yoa - .y$deaths_yoa)) |>

                  dplyr::rename_with(.cols = dplyr::everything(),
                                     .fn = ~ base::gsub("yoa", yoa, .x))

                }
              )
          )

    }

    # YLL & PREMATURE DEATHS (CONSTANT EXPOSURE) ####################################################

    if (is_yll || #And  ("yld", "daly") if yld for life table ever implemented
         is_constant_exposure) {


      ## PROJECT POPULATIONS #########################################################################

      ### DEFINE FUNCTION FOR POPULATION PROJECTION ##################################################

      project_pop <- function(df, prob_survival, prob_survival_until_midyear) {


        # Rename yoa columns
        base::names(df) <- base::names(df) |>
          # Important to repace first yoa_plus_1,
          # otherwise the replacement of _yoa also affects yoa_plus_1
          base::gsub("_yoa_plus_1", base::paste0("_", yoa_plus_1), x = _) |>
          base::gsub("_yoa", base::paste0("_", yoa), x = _)


        # If the time horizon covers only the year of analysis there is
        # nothing to project. The columns of the year of analysis, which were
        # just renamed above, are the only ones needed to obtain the impacts
        if (n_years_projection == 0) { return(df) }

        # Precompute complements
        death_prob <- 1 - prob_survival
        n_ages <- base::nrow(df)      

        # Initialise matrices
        entry_pop <- base::matrix(NA, nrow = n_ages, ncol = n_years_projection,
                                  # Row and column names
                                  # NULL because no row names
                                  dimnames = base::list(NULL, entry_names))
        midyear_pop <- base::matrix(NA, nrow = n_ages, ncol = n_years_projection,
                                  dimnames = base::list(NULL, midyear_names))
        deaths <- base::matrix(NA, nrow = n_ages, ncol = n_years_projection,
                                  dimnames = base::list(NULL, death_names))      
        

        # Set initial year
        entry_pop[, 1] <- df[[entry_names[1]]]
        midyear_pop[, 1] <- base::round(entry_pop[, 1] * prob_survival_until_midyear, 10)
        deaths[, 1] <- base::round(entry_pop[, 1] * death_prob, 10)

        # Loop across years
        # E.g. starts with 1 and ends with 98;
        # i (index in the number of years) is used to select both the rows and the columns
        # seq_len() (and not 1:) to get no iteration at all
        # if the projection covers only one year

        for (i in base::seq_len(n_years_projection - 1)) {

          # Each year the survivors of the previous year get one year older.
          # The upper limit is the number of age groups (and NOT the number of
          # projection years) because nobody gets older than the last age group.
          # If the cohort is already older than the last age group,
          # then there is nothing left to project
          if (i + 2 > n_ages) { break }

          rows <- (i + 2):n_ages
          # ENTRY POP YOA+1 <- ( ENTRY POP YOA ) * ( SURVIVAL PROBABILITY YOA )
          entry_pop[rows, i + 1] <- base::round(entry_pop[rows - 1, i] * prob_survival[rows - 1], 10)
          # MID-YEAR POP YOA+1 <- ( ENTRY POP YOA+1) * ( SURVIVAL PROBABILITY FROM START OF YOA+1 TO MID YEAR YOA+1)
          midyear_pop[rows, i + 1]   <- base::round(entry_pop[rows, i + 1] * prob_survival_until_midyear[rows], 10)
          # DEATHS IN YOA+1 <- ( ENTRY POP YOA+1 ) * (1 - SURVIVAL PROBABILITY YOA+1 )
          deaths[rows, i + 1]    <- base::round(entry_pop[rows, i + 1] * death_prob[rows], 10)
        }

        # Column bin matrices to input data frame
        # Remove first column of entry_pop, because it exists already in input data frame
        # drop = FALSE to keep a matrix (with column names) also if only one
        # column is left, otherwise the column would lose its name
        df <-
          dplyr::bind_cols(df, midyear_pop, entry_pop[, -1, drop = FALSE], deaths)


        return(df)
      }

      ### SINGLE YEAR EXPOSURE #######################################################################
      # Determine YLLs for baseline and impacted scenario's in the single year exposure case

      if (is_single_year_exposure){

        # PROJECT POPULATIONS IN BOTH IMPACTED AND BASELINE SCENARIO FROM YOA+1 UNTIL THE END
        # USING MODIFIED SURVIVAL PROBABILITIES (BECAUSE AFTER YOA THERE IS NO MORE AIR POLLUTION)
        lifetable_calculation <- lifetable_calculation |>
          dplyr::mutate(
            projection_if_exposed_by_age_and_year =
              purrr::map(
                .x = projection_if_exposed_by_age_and_year,
                .f = ~ project_pop(
                  df = .x,
                  prob_survival = .x$prob_survival_mod,
                  prob_survival_until_midyear = .x$prob_survival_until_midyear_mod)),

            projection_if_unexposed_by_age_and_year =
              purrr::map(
                .x = projection_if_unexposed_by_age_and_year,
                .f = ~ project_pop(
                  df = .x,
                  prob_survival = .x$prob_survival_mod,
                  prob_survival_until_midyear = .x$prob_survival_until_midyear_mod))
            )

        ### CONSTANT EXPOSURE ########################################################################
        # Determine YLLs for baseline and impacted scenario's in the constant exposure case

        # IF CONSTANT EXPOSURE
      } else {

        # PROJECT POPULATION IN EXPOSED SCENARIO
        lifetable_calculation <- lifetable_calculation |>
          dplyr::mutate(
            projection_if_exposed_by_age_and_year =
              purrr::map(
                .x = projection_if_exposed_by_age_and_year,
                .f = ~ project_pop(
                  df = .x,
                  prob_survival = .x$prob_survival,
                  prob_survival_until_midyear = .x$prob_survival_until_midyear)),
            # PROJECT POPULATION IN UNEXPOSED SCENARIO
            projection_if_unexposed_by_age_and_year =
              purrr::map(
                .x = projection_if_unexposed_by_age_and_year,
                .f = ~ project_pop(
                  df = .x,
                  prob_survival = .x$prob_survival_mod,
                  prob_survival_until_midyear = .x$prob_survival_until_midyear_mod)

              )
          )
      }


      ###  DETERMINE IMPACT (YLL, PREMATURE DEATHS (CONSTANT EXPOSURE))  ###########################
      # YLL and premature deaths attributable to exposure are calculated

      # Helper function to be used below
      calculate_impact <- function(df_unexposed, df_exposed, var_prefix) {


        ages_and_pop <- df_unexposed |>
          dplyr::select(age_start, age_end, population)

        df_unexposed_vars <- df_unexposed |>
          dplyr::select(dplyr::starts_with(var_prefix))

        df_exposed_vars <- df_exposed|>
          dplyr::select(dplyr::starts_with(var_prefix))

        if(var_prefix == "midyear_population_"){
          diff <- df_unexposed_vars - df_exposed_vars
          # IF DEATHS
          } else {
            # The way round because otherwise negative numbers
            # Reason: unexposed means more population but less deaths
            diff <- - (df_unexposed_vars - df_exposed_vars)
          }

        impact <- dplyr::bind_cols(ages_and_pop, diff) |>
          dplyr::rename_with(
            .cols = dplyr::starts_with(var_prefix),
            .fn = ~ base::gsub(var_prefix, "impact_", .x)
          )

        return(impact)
      }

      # Apply the helper function above to calculate impacts (deaths or yll)
      # from exposed and unexposed projections

      var_prefix_for_function <-
        base::ifelse(is_deaths, "deaths_", "midyear_population_")

      lifetable_calculation <- lifetable_calculation |>
        dplyr::mutate(
          impact_by_age_and_year =
            purrr::map2(
              # Attention first argument unexposed and second exposed (see function above)
              .x = projection_if_unexposed_by_age_and_year,
              .y = projection_if_exposed_by_age_and_year,
              .f = calculate_impact,
              var_prefix = var_prefix_for_function))


      ## NEWBORNS #################################################################

      if (is_with_newborns && is_constant_exposure) {

        fill_right_of_diag <- function(tbl) {

          # Select only the numeric matrix portion, ignoring age columns
          cols <- base::setdiff(base::names(tbl), c("age_start", "age_end", "population"))
          data_selection <- tbl[, cols, drop = FALSE]

          n_years <- base::ncol(data_selection)

          # Once an age group is not in the population anymore
          # (i.e. right of the diagonal) it is assumed that the newborns
          # replacing it have the same impact as in the last year with
          # population (i.e. the value in the diagonal).
          # Only the rows that have a diagonal value AND at least one year
          # to the right of it have to be filled.
          # min() with the number of years because there can be
          # more age groups than projection years (and the other way round)
          for (i in base::seq_len(base::min(base::nrow(data_selection),
                                            n_years - 1))) {
            # Extract the diagonal value
            diag_value <- data_selection[i, i, drop = TRUE]
            # Replace NAs to the right of the diagonal with the diagonal value
            data_selection[i, (i + 1):n_years] <- diag_value
          }

          # Assign back into the same positions
          tbl[, cols] <- data_selection

          return(tbl)
        }


        lifetable_calculation <- lifetable_calculation |>
          dplyr::mutate(
            impact_by_age_and_year = purrr::map(
              .x = impact_by_age_and_year,
              .f = fill_right_of_diag))

      }
      # If without newborns nothing has to be done.
      # The same applies to a single year exposure: the newborns of the years
      # after the year of analysis were never exposed, so no impact can be
      # attributed to them (validate_input_attribute() warns about it)
    }


    # COMPILE OUTPUT ##############################################################################

    # Data wrangling to get the results in the needed format

    # GET DEATHS AND YLL FROM LIFETABLE

    # Store total impacts by age #########
    ## Sum impacts
    lifetable_calculation <- lifetable_calculation |>
      dplyr::mutate(

        impact_by_age_and_year_long = purrr::map(
          .x = impact_by_age_and_year,
          function(.x){

            # Reshape year to long format
            .x <-
              tidyr::pivot_longer(data = .x,
                                  cols = dplyr::starts_with("impact_"),
                                  names_to = "year",
                                  values_to = "impact",
                                  names_prefix = "impact_") |>
              # Keep only first value of population for each year
              # Otherwise the population is repeated for all years
              # and the sum of population, calculated in get_output(),
              # will be wrong (much higher)
              dplyr::mutate(.by = c(age_start, age_end),
                            population = ifelse(year == yoa, population, NA))

            if(is_deaths && is_single_year_exposure){
              .x <- .x |>
                ## Select first year of projection
                dplyr::filter(year == yoa)

            } else {
              .x <- .x |>
                ## Select all years within time horizon
                dplyr::filter(year <= last_year_projection )
            }
          }

        ))

    # Unnest column #####

    # Unnest the obtained impacts to integrate them the main tibble
     results_raw <- lifetable_calculation |>
      # Remove all nested tibbles except impact_by_age_and_year_long
      # which have to be nested
      dplyr::select(-data_by_age,
                    -projection_if_exposed_by_age_and_year,
                    -projection_if_unexposed_by_age_and_year,
                    -impact_by_age_and_year) |>
      # Unnest
      tidyr::unnest(impact_by_age_and_year_long) |>
      # Rename age_start to age_group (consistent with input and other pathways)
      dplyr::rename("age_group" = "age_start") |>
      # Remove age_end not needed anymore
      dplyr::select(-age_end)

    out <- base::list(
      intermediate_calculations = lifetable_calculation,
      results_raw = results_raw
    )

    return(out)

  }
