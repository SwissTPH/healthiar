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

# EXAMPLES #####################################################################
#' @examples
#' # TBD

#' @author Alberto Castro & Axel Luyten

#' @keywords internal



get_impact_with_lifetable <-
  function(input_with_risk_and_pop_fraction){

    # GET POP IMPACT ######

    user_options <- options()
    options(digits = 15)


    # USEFUL VARIABLES ##########
    # yoa means Year Of Analysis
    yoa <- input_with_risk_and_pop_fraction |>  dplyr::pull(year_of_analysis) |> dplyr::first()
    yoa_plus_1 <- base::as.numeric(yoa) + 1

    health_outcome <- base::unique(input_with_risk_and_pop_fraction$health_outcome)

    is_single_year_exposure <- base::unique(input_with_risk_and_pop_fraction$approach_exposure) == "single_year"
    is_constant_exposure <- base::unique(input_with_risk_and_pop_fraction$approach_exposure) == "constant"

    is_with_newborns <- base::unique(input_with_risk_and_pop_fraction$approach_newborns) == "with_newborns"

    # The number_years defines for how many years the population should be projected;
    # might be easier to have two arguments "start year" and "end year"
    number_years <-
      base::length(base::unique(input_with_risk_and_pop_fraction$age_start)) - 1

    # Define the years based on number_years
    # e.g. 2020 to 2118
    years_projection <- yoa_plus_1 : (yoa + number_years)


    # LIFETABLE SETUP ##############################################################################
    data_for_projection <- input_with_risk_and_pop_fraction |>
      dplyr::mutate(
      # Duplicate bhd  and year_of_analysis
      # for more handy column names for life table calculations
      deaths = bhd,
      yoa = year_of_analysis,
      # Rename population adding suffix yoa
      # yoa means Year Of Analysis
      # It is better to do it  now (before nesting tables)
      population_yoa = population)


    data_for_projection <- data_for_projection |>
      # Get modification factor
      # it works with both single exposure and exposure distribution
      dplyr::mutate(
        modification_factor = 1 - pop_fraction,
        .after = rr) |>

      # CALCULATE ENTRY POPULATION OF YEAR OF ANALYSIS (YOA)
      dplyr::mutate(
        population_yoa_entry = population_yoa + (deaths / 2),
        .before = population_yoa) |>

      # CALCULATE PROBABILITY OF SURVIVAL FROM START YEAR TO END YEAR & START YEAR TO MID YEAR
      dplyr::mutate(
        # probability of survival from start of year i to start of year i+1 (entry to entry)
        prob_survival =
          (population_yoa - (deaths / 2)) /
          (population_yoa + (deaths / 2) ),

        # Probability of survival from start to midyear
        # For example entry_pop = 100, prob_survival = 0.8 then end_of_year_pop = 100 * 0.8 = 80.
        # midyear_pop = 100 - (20/2) = 90.
        prob_survival_until_midyear = 1 - ((1 - prob_survival) / 2),

        # Hazard rate for calculating survival probabilities
        hazard_rate = deaths / population_yoa,

        .after = deaths)


    # CALCULATE MODIFIED SURVIVAL PROBABILITIES
    data_for_projection <- data_for_projection |>
      dplyr::mutate(
        # For all ages min_age and higher calculate modified survival probabilities
        # Calculate first the boolean/logic column to speed up calculations below
        age_end_over_min_age = age_end > min_age,

        # Calculate modified hazard rate = modification factor * hazard rate = mod factor * (deaths / mid-year pop)
        hazard_rate_mod =
          dplyr::if_else(age_end_over_min_age,
                         modification_factor * hazard_rate,
                         hazard_rate),

        # Calculate modified survival probability =
        # ( 2 - modified hazard rate ) / ( 2 + modified hazard rate )
        prob_survival_mod =
          dplyr::if_else(age_end_over_min_age,
                         (2 - hazard_rate_mod) / (2 + hazard_rate_mod),
                         prob_survival),

        prob_survival_until_midyear_mod =
          dplyr::if_else(age_end_over_min_age,
                         1 - ((1 - prob_survival_mod) / 2),
                         prob_survival_until_midyear),

        .after = deaths)


    # Nest life tables
    data_for_projection <- data_for_projection |>
      tidyr::nest(
        data_by_age_nested =
        c(yoa, age_group, age_start, age_end, bhd, deaths, population,
          modification_factor,
          prob_survival, prob_survival_until_midyear, hazard_rate,
          age_end_over_min_age, prob_survival_mod, prob_survival_until_midyear_mod, hazard_rate_mod,
          # These columns at the end to link with projections
          population_yoa, population_yoa_entry))


    ## EXPOSED PROJECTION ###########################################################################
    # The exposed projection is the scenario of "business as usual"
    # i.e. the scenario with the exposure to the environmental stressor as (currently) measured

    # DETERMINE ENTRY POPULATION OF YOA+1 IN BASELINE SCENARIO
    data_with_projection <- data_for_projection |>
      dplyr::mutate(
        projection_if_exposed_nested =
          purrr::map(
            .x = data_by_age_nested,
            function(.x){
              .x <- .x |>
                dplyr::mutate(
                  # End-of-year population YOA = (entry population YOA) * ( survival probability )
                  population_yoa_end = population_yoa_entry * prob_survival,

                  # Deaths YOA = End pop YOA - Entry pop YOA
                  deaths_yoa = population_yoa_entry - population_yoa_end,

                  # Entry population YOA+1 = lag ( End-of-year population YOA )
                  population_yoa_plus_1_entry = dplyr::lag(population_yoa_end))
            }
          )
        )

    ## UNEXPOSED PROJECTION ###########################################################################
    # The exposed projection is the scenario without any exposure to the environmental stressor

    # CALCULATE YOA MID-YEAR POPOULATION,
    # YOA END-OF-YEAR POPULATION, YOA DEATHS AND
    # YOA+1 ENTRY POPULATION USING MODIFIED SURVIVAL PROBABILITIES
    data_with_projection <- data_with_projection |>
      dplyr::mutate(
        projection_if_unexposed_nested =
          purrr::map(
            .x = data_by_age_nested ,
            function(.x){
              .x <- .x |>
                dplyr::mutate(
                  # Re-calculate population_yoa
                  # MID-YEAR POP = (entry population YOA) * ( survival probability until mid year )
                  population_yoa = population_yoa_entry * prob_survival_until_midyear_mod,

                  # Calculate end-of-year population in YOA to later determine premature deaths
                  population_yoa_end = population_yoa_entry * prob_survival_mod,

                  # Deaths YOA = End pop YOA - Entry pop YOA
                  deaths_yoa = population_yoa_entry - population_yoa_end,

                  # Entry population YOA+1 = lag ( End-of-year population YOA )
                  population_yoa_plus_1_entry = dplyr::lag(population_yoa_end)
                  )
            }
          )
      )

    # PREMATURE DEATHS (SINGLE YEAR EXPOSURE) ######################################################
    # YOA = YEAR OF ANALYSIS
    if (health_outcome == "deaths" &
        is_single_year_exposure) {

      data_with_projection <- data_with_projection |>
        # Premature deaths = ( impacted scenario YOA end-of-year population ) - ( baseline scenario YOA end-of-year pop )
        dplyr::mutate(
          deaths_by_age_and_year_nested =
            purrr::map2(
              .x = projection_if_unexposed_nested,
              .y = projection_if_exposed_nested,
              .f = ~ {
                tibble::tibble(
                  age_start = .x$age_start,
                  age_end = .x$age_end,
                  impact_yoa = .x$population_yoa_end - .y$population_yoa_end) |>

                  dplyr::rename_with(.cols = dplyr::everything(),
                                     .fn = ~ base::gsub("yoa", yoa, .x))

                }
              )
          )

    }

    # YLL & PREMATURE DEATHS (CONSTANT EXPOSURE) ####################################################

    if (health_outcome == "yll"| #And  ("yld", "daly") if yld for life table ever implemented
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

        # Precompute column names to increase speed code
        entry_names <- base::paste0("population_", years_projection, "_entry")
        mid_names   <- base::paste0("population_", years_projection)
        death_names <- base::paste0("deaths_", years_projection)

        # Precompute complements
        death_prob <- 1 - prob_survival

        # Initialise matrices
        pop_entry <- base::matrix(NA, nrow = 100, ncol = number_years,
                                  # Row and column names
                                  # NULL because no row names
                                  dimnames = base::list(NULL, entry_names))
        pop_mid   <- base::matrix(NA, nrow = 100, ncol = number_years,
                                  dimnames = base::list(NULL, mid_names))
        deaths    <- base::matrix(NA, nrow = 100, ncol = number_years,
                                  dimnames = base::list(NULL, death_names))

        # Set initial year
        pop_entry[, 1] <- df[[entry_names[1]]]
        pop_mid[, 1] <- pop_entry[, 1] * prob_survival_until_midyear
        deaths[, 1] <- pop_entry[, 1] * (1 - prob_survival)

        # Loop across years
        # E.g. starts with 1 and ends with 98;
        # i (index in the number of years) is used to select both the rows and the columns

        for (i in 1: (number_years - 1)) {
          rows <- (i + 2):(number_years + 1)
          # ENTRY POP YOA+1 <- ( ENTRY POP YOA ) * ( SURVIVAL PROBABILITY YOA )
          pop_entry[rows, i + 1] <- pop_entry[rows - 1, i] * prob_survival[rows - 1]
          # MID-YEAR POP YOA+1 <- ( ENTRY POP YOA+1) * ( SURVIVAL PROBABILITY FROM START OF YOA+1 TO MID YEAR YOA+1)
          pop_mid[rows, i + 1]   <- pop_entry[rows, i + 1] * prob_survival_until_midyear[rows]
          # DEATHS IN YOA+1 <- ( ENTRY POP YOA+1 ) * (1 - SURVIVAL PROBABILITY YOA+1 )
          deaths[rows, i + 1]    <- pop_entry[rows, i + 1] * death_prob[rows]
        }

        # Column bin matrices to input data frame
        # Remove first column of pop_entry, because it exists already in input data frame
        df <-
          dplyr::bind_cols(df, pop_mid, pop_entry[, -1], deaths)


        return(df)
      }

      ### SINGLE YEAR EXPOSURE #######################################################################
      # Determine YLLs for baseline and impacted scenario's in the single year exposure case

      if (is_single_year_exposure){

        # PROJECT POPULATIONS IN BOTH IMPACTED AND BASELINE SCENARIO FROM YOA+1 UNTIL THE END
        # USING MODIFIED SURVIVAL PROBABILITIES (BECAUSE AFTER YOA THERE IS NO MORE AIR POLLUTION)
        data_with_projection <- data_with_projection |>
          dplyr::mutate(
            projection_if_exposed_nested =
              purrr::map(
                .x = projection_if_exposed_nested,
                .f = ~ project_pop(
                  df = .x,
                  prob_survival = .x$prob_survival_mod,
                  prob_survival_until_midyear = .x$prob_survival_until_midyear_mod)),

            projection_if_unexposed_nested =
              purrr::map(
                .x = projection_if_unexposed_nested,
                .f = ~ project_pop(
                  df = .x,
                  prob_survival = .x$prob_survival_mod,
                  prob_survival_until_midyear = .x$prob_survival_until_midyear_mod))
            )

        ### CONSTANT EXPOSURE ########################################################################
        # Determine YLLs for baseline and impacted scenario's in the constant exposure case

      } else {

        # PROJECT POPULATION IN BASELINE SCENARIO
        data_with_projection <- data_with_projection |>
          dplyr::mutate(
            projection_if_exposed_nested =
              purrr::map(
                .x = projection_if_exposed_nested,
                .f = ~ project_pop(
                  df = .x,
                  prob_survival = .x$prob_survival,
                  prob_survival_until_midyear = .x$prob_survival_until_midyear)),
            # PROJECT POPULATION IN IMPACTED SCENARIO
            projection_if_unexposed_nested =
              purrr::map(
                .x = projection_if_unexposed_nested,
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
      calculate_impact <- function(df_a, df_b, var_prefix) {
        ages <- df_a |>
          dplyr::select(age_start, age_end)

        impact_df_a <- df_a |>
          dplyr::select(dplyr::starts_with(var_prefix),
                        -dplyr::contains("_end"),
                        -dplyr::contains("_entry"))

        impact_df_b <- df_b|>
          dplyr::select(dplyr::starts_with(var_prefix),
                        -dplyr::contains("_end"),
                        -dplyr::contains("_entry"))

        diff <- impact_df_a - impact_df_b

        impact <- dplyr::bind_cols(ages, diff) |>
          dplyr::rename_with(
            .cols = dplyr::starts_with(var_prefix),
            .fn   = ~ base::gsub(var_prefix, "impact_", .x)
          )

        return(impact)
      }

      # Apply the helper function above to calculate deaths and yll
      # from exposed and unexposed projections

      data_with_projection <- data_with_projection |>
        dplyr::mutate(
          yll_by_age_and_year_nested =
            purrr::map2(
              .x = projection_if_unexposed_nested,
              .y = projection_if_exposed_nested,
              .f = calculate_impact,
              var_prefix = "population_"
            ),
          deaths_by_age_and_year_nested =
            purrr::map2(
              projection_if_exposed_nested,
              projection_if_unexposed_nested,
              calculate_impact,
              var_prefix = "deaths_"
            )
        )

      ## NEWBORNS #################################################################

      if (is_with_newborns) {



        fill_right_of_diag <- function(tbl) {
          for (i in seq_len(nrow(tbl))) {
            # Extract the diagonal value
            diag_value <- tbl[i, i, drop = TRUE]
            # Replace NAs to the right of the diagonal with the diagonal value
            tbl[i, (i+1):ncol(tbl)] <- diag_value
          }
          tbl <- tbl |>
            dplyr::select(-ncol(tbl))
          return(tbl)
        }

        data_with_projection <- data_with_projection |>
          dplyr::mutate(
            yll_by_age_and_year_nested = purrr::map(
              .x = yll_by_age_and_year_nested,
              function(.x){

                .x[, dplyr::setdiff(names(.x), c("age_start", "age_end"))] <- fill_right_of_diag(.x[, dplyr::setdiff(names(.x), c("age_start", "age_end"))])
                return(.x)
              }
            )
            , .before = 1)

        data_with_projection <- data_with_projection |>
          dplyr::mutate(deaths_by_age_and_year_nested = purrr::map(
            .x = deaths_by_age_and_year_nested,
            function(.x){

              .x[, dplyr::setdiff(names(.x), c("age_start", "age_end"))] <- fill_right_of_diag(.x[, dplyr::setdiff(names(.x), c("age_start", "age_end"))])
              return(.x)
            }
          )
          , .before = 1)

      }

    }

    # COMPILE OUTPUT ##############################################################################
    # Data wrangling to get the results in the needed format

    if (health_outcome == "deaths"){
      data_with_projection <- data_with_projection |>
        dplyr::mutate(
          pop_impact_nested = deaths_by_age_and_year_nested) }
    else if (health_outcome == "yll"){
      data_with_projection <- data_with_projection |>
        dplyr::mutate(
          pop_impact_nested = yll_by_age_and_year_nested)
        }

    # Remove from pop, as already present in input_with_risk_...
    data_with_projection <- data_with_projection |>
      dplyr::select(-data_by_age_nested)

    if (health_outcome %in% c("deaths", "yll")){

      joining_columns_pop_impact <-
        healthiar:::find_joining_columns(data_for_projection,
                                         data_with_projection,
                                         except = "data_by_age_nested")

      pop_impact <-
        data_for_projection |>
        dplyr::right_join(data_with_projection,
                          by = joining_columns_pop_impact) |>
        dplyr::relocate(dplyr::contains("_nested"), .before = 1)}


    on.exit(options(user_options))

    # GET DEATHS AND YLL FROM LIFETABLE

    ## Define health_outcome variable
    health_outcome <- base::unique(data_for_projection$health_outcome)

    # Determine default time horizon for YLL/YLD if not specified ##############
    if ( health_outcome %in% c("yll") & # And ("yld")  if ever implemented
         !"time_horizon" %in% base::names(data_for_projection ) ) {

      time_horizon <- data_for_projection |>
        dplyr::slice(1) |>                      # Select the first row
        dplyr::pull(data_by_age_nested) |> # Extract the nested tibble column
        purrr::pluck(1) |>                      # Get the tibble stored in the first element
        base::nrow()

      ## Add time_horizon to tibble
      data_for_projection <- data_for_projection |>
        dplyr::mutate(time_horizon = time_horizon)

    }

    ## ALTERNATIVE CODE
    ## Filter for relevant ages
    impact_detailed <- pop_impact |>
      dplyr::mutate(
        health_outcome = health_outcome,
        impact_by_year_nested =
          purrr::pmap(
            base::list(.x =  pop_impact_nested,
                       max_age = base::unique(max_age),
                       min_age = base::unique(min_age),
                       health_outcome = base::unique(health_outcome)),
            function(.x, max_age, min_age, health_outcome){

              # Set values in upper triangle to NA ###############################
              ## NOTE: also removes newborns values

              if ( health_outcome == "deaths" ) {

                .x <- .x |>
                  dplyr::mutate(
                    dplyr::across(dplyr::contains("deaths"), ~ {
                      mat <- base::as.matrix(.x)
                      mat[base::upper.tri(mat, diag = FALSE)] <- NA
                      return(mat)}))

              } else {

                .x <- .x |>
                  dplyr::mutate(
                    dplyr::across(dplyr::contains("impact_"),
                                  ~ {mat <- base::as.matrix(.x)
                                  mat[base::upper.tri(mat, diag = FALSE)] <- NA
                                  return(mat)}))
              }


              # Filter for relevant ages #########################################
              .x <- .x |>
                dplyr::filter(age_start <= max_age,
                              age_start >= min_age)


              # Calculate YLL/YLD impact per year ################################

              if ( health_outcome %in% c("yll") ) { # And ("yld")  if ever implemented

                .x <- .x |>
                  dplyr::select(dplyr::contains("impact_")) |>

                  ## Sum over ages (i.e. vertically)
                  ## only ages between "max_age" and "data_for_projection |>  pull(min_age) |> first()" filtered for above
                  dplyr::summarize_all(sum, na.rm = TRUE) |>

                  ## Reshape to long format
                  ## (output is data frame with 2 columns "year" & "impact")
                  tidyr::pivot_longer(cols = dplyr::starts_with("impact_"),
                                      names_to = "year",
                                      values_to = "impact",
                                      names_prefix = "impact_") |>

                  ## Convert year to numeric
                  dplyr::mutate(year = base::as.numeric(year))
              } else
                .x<-.x}), .before = 1)


    # Deaths ###################################################################

    ## Store total deaths in YOA in column impact_nested
    if ( health_outcome == "deaths" ) {

      impact_detailed <- impact_detailed |>
        # Store in new column "impact_nested"
        dplyr::mutate(
          impact = purrr::map(
            .x = impact_by_year_nested,
            function(.x){

              .x <- .x |>
                dplyr::select(dplyr::all_of(base::paste0("impact_", yoa))) |>
                base::sum(na.rm = TRUE)
              return(.x)
            }
          )
        ) |>
        dplyr::mutate(impact = base::as.numeric(impact))


    }

    # Store total, YLL/YLD in YOA in column impact_nested #########
    ## Single number

    if ( health_outcome == "yll"){ # And ("yld")  if ever implemented

      impact_detailed <- impact_detailed |>

        ## Add column for year of analysis
        dplyr::mutate(year_of_analysis = year_of_analysis) |>
        ## Add column for time horizon
        dplyr::mutate(time_horizon = data_for_projection |>  dplyr::pull(time_horizon) |> dplyr::first()) |>
        ## Add column for last year of analysis
        dplyr::mutate(last_year = year_of_analysis + time_horizon - 1)

      ## Sum impacts
      impact_detailed <- impact_detailed |>

        dplyr::mutate(
          impact_nested = purrr::pmap(
            base::list(.x = impact_by_year_nested, .y = last_year, health_outcome = base::unique(health_outcome)),
            function(.x, .y, health_outcome){

              ## If yll or yld
              if( health_outcome == "yll"){ # And ("yld")  if ever implemented

                .x <-
                  .x |>

                  ## Select all years within time horizon
                  dplyr::filter(.data = _, year < .y+1) |>

                  ## Sum impact
                  dplyr::summarise(impact = base::sum(impact, na.rm = TRUE))

                return(.x)
              }
            }
          )
        ) |>
        dplyr::mutate(
          impact_for_discounting_nested = impact_nested
        ) |>
        ## Unnest the obtained impacts to integrate them the main tibble
        ## Impact saved in column impact
        tidyr::unnest(impact_nested)

    }


    # Assign ID ###################################

    ## Create ID for the rows (will be used below)
    id_columns <-
      c("sex",
        "geo_id_disaggregated",
        "erf_ci", "bhd_ci", "exp_ci", "dw_ci", "cutoff_ci", "duration_ci")

    id_columns_in_df <-
      id_columns[id_columns %in% base::names(impact_detailed)]

    id <-
      purrr::pmap_chr(impact_detailed[id_columns_in_df],
                      ~base::paste(..., sep = "_"))

    ## Name rows with the ids for better overview in Environment
    impact_detailed <-
      impact_detailed  |>
      dplyr::mutate(dplyr::across(dplyr::contains("_nested"),
                                  ~purrr::set_names(.x,
                                                    id)),
                    age_group = "total")

    return(impact_detailed)

  }
