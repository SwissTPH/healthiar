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
    population_yoa <- base::paste0("population_", yoa)
    population_yoa_entry <- base::paste0(population_yoa,"_entry")
    population_yoa_plus_1_entry <- base::paste0("population_", yoa_plus_1,"_entry")
    population_yoa_end <- base::paste0(population_yoa,"_end")
    deaths_yoa <- base::paste0("deaths_", yoa)
    impact_yoa <- base::paste0("impact_", yoa)

    health_outcome <- base::unique(input_with_risk_and_pop_fraction$health_outcome)

    # LIFETABLE SETUP ##############################################################################

    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
      dplyr::mutate(modification_factor = 1 - pop_fraction, .after = rr) |> # WORKS WITH BOTH SINGLE EXPOSURE VALUE AND EXPOSURE DISTRIBUTION AS INPUTS
      # ADD THE MODIFICATION FACTOR TO THE NESTED TIBBLE "LIFETABLE_WITH_POP_nested" USING FCT PMAP()
      dplyr::mutate(lifetable_with_pop_nested =
                      purrr::pmap(
                        list(lifetable_with_pop_nested, modification_factor),
                        function(lifetable_with_pop_nested, modification_factor){
                          lifetable_with_pop_nested <- lifetable_with_pop_nested |>
                            dplyr::mutate(modification_factor = modification_factor)
                        }
                      )
      )



    # ADD ENTRY POPULATION OF YOA & SURVIVAL PROBABILITIES
    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
      dplyr::relocate(lifetable_with_pop_nested, .before = 1) |>
      dplyr::mutate(
        lifetable_with_pop_nested =
          purrr::map(
            .x = lifetable_with_pop_nested,
            function(.x){

              .x <- .x |>
                dplyr::rename(!!population_yoa := population) |>

                # CALCULATE ENTRY POPULATION OF YEAR OF ANALYSIS (YOA)
                dplyr::mutate(
                  !!population_yoa_entry := !!dplyr::sym(population_yoa) + (deaths / 2),
                  .before = !!population_yoa) |>

                # CALCULATE PROBABILITY OF SURVIVAL FROM START YEAR TO END YEAR & START YEAR TO MID YEAR
                # probability of survival from start of year i to start of year i+1 (entry to entry)
                dplyr::mutate(
                  prob_survival =
                    (!!dplyr::sym(population_yoa) - (deaths / 2)) /
                    (!!dplyr::sym(population_yoa) + (deaths / 2) ),
                  .after = deaths) |>

                # Probability of survival from start to midyear
                # For example entry_pop = 100, prob_survival = 0.8 then end_of_year_pop = 100 * 0.8 = 80.
                # mid_year_pop = 100 - (20/2) = 90.
                dplyr::mutate(
                  prob_survival_until_mid_year = 1 - ((1 - prob_survival) / 2),
                  .after = deaths) |>

                # Hazard rate for calculating survival probabilities
                dplyr::mutate(
                  hazard_rate = deaths / !!dplyr::sym(population_yoa),
                  .after = deaths)
            }
          )
      )

    # CALCULATE MODIFIED SURVIVAL PROBABILITIES
    input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
      dplyr::mutate(
        lifetable_with_pop_nested =
          purrr::map(
            .x = lifetable_with_pop_nested,
            function(.x){
              .x <- .x |>
                # For all ages min_age and higher calculate modified survival probabilities
                # Calculate modified hazard rate = modification factor * hazard rate = mod factor * (deaths / mid-year pop)
                dplyr::mutate(
                  hazard_rate_mod =
                    dplyr::if_else(age_end > c(rep_len(input_with_risk_and_pop_fraction |>  dplyr::pull(min_age) |> dplyr::first(), length.out = length(age_end))), # This makes sure comparators are of same length
                                   modification_factor * hazard_rate,
                                   hazard_rate),
                  .after = deaths) |>

                # Calculate modified survival probability =
                # ( 2 - modified hazard rate ) / ( 2 + modified hazard rate )
                dplyr::mutate(
                  prob_survival_mod =
                    dplyr::if_else(age_end > c(rep_len(input_with_risk_and_pop_fraction |>  dplyr::pull(min_age) |> dplyr::first(), length.out = length(age_end))), # This makes sure comparators are of same length
                                   (2 - hazard_rate_mod) / (2 + hazard_rate_mod),
                                   prob_survival),
                  .after = deaths) |>

                dplyr::mutate(
                  prob_survival_until_mid_year_mod =
                    dplyr::if_else(age_end > c(rep_len(input_with_risk_and_pop_fraction |>  dplyr::pull(min_age) |> dplyr::first(), length.out = length(age_end))), # This makes sure comparators are of same length
                                   1 - ((1 - prob_survival_mod) / 2),
                                   prob_survival_until_mid_year),
                  .after = deaths)
            }

          )
      )

    ## BASELINE SCENARIO ###########################################################################
    # The baseline scenario is the scenario of "business as usual"
    # i.e. the scenario with the exposure to the environmental stressor as (currently) measured

    # DETERMINE ENTRY POPULATION OF YOA+1 IN BASELINE SCENARIO
    pop <- input_with_risk_and_pop_fraction |>
      dplyr::mutate(
        projection_if_exposed_nested =
          purrr::map(
            .x = lifetable_with_pop_nested,
            function(.x){
              .x <- .x |>
                # End-of-year population YOA = entry pop YOA * ( survival probability )
                dplyr::mutate(
                  !!population_yoa_end :=
                    !!dplyr::sym(population_yoa_entry) * prob_survival) |>

                # Deaths YOA = End pop YOA - Entry pop YOA
                dplyr::mutate(
                  !!deaths_yoa :=
                    !!dplyr::sym(population_yoa_entry) - !!dplyr::sym(population_yoa_end),
                  .after =  !!dplyr::sym(population_yoa)) |>

                # Entry population YOA+1 = lag ( End-of-year population YOA )
                dplyr::mutate(
                  !!population_yoa_plus_1_entry :=
                    dplyr::lag(!!dplyr::sym(population_yoa_end)))
            }
          )
        , .after = lifetable_with_pop_nested)

    ## IMPACTED SCENARIO ###########################################################################
    # The impacted scenario is the scenario without any exposure to the environmental stressor


    # CALCULATE YOA MID-YEAR POPOULATION,
    # YOA END-OF-YEAR POPULATION, YOA DEATHS AND
    # YOA+1 ENTRY POPULATION USING MODIFIED SURVIVAL PROBABILITIES
    pop <- pop |>
      dplyr::mutate(
        projection_if_unexposed_nested =
          purrr::map(
            .x = lifetable_with_pop_nested ,
            function(.x){
              .x <- .x |>

                # MID-YEAR POP = (ENTRY POP) * ( survival probability until mid year )
                dplyr::mutate(!!population_yoa :=
                                !!dplyr::sym(population_yoa_entry) * prob_survival_until_mid_year_mod) |>

                # Calculate end-of-year population in YOA to later determine premature deaths
                dplyr::mutate(!!population_yoa_end :=
                                !!dplyr::sym(population_yoa_entry) * prob_survival_mod) |>

                # Deaths YOA = End pop YOA - Entry pop YOA
                dplyr::mutate(!!deaths_yoa :=
                                !!dplyr::sym(population_yoa_entry) - !!dplyr::sym(population_yoa_end),
                              .after =  !!dplyr::sym(population_yoa)) |>

                # Entry population YOA+1 = lag ( End-of-year population YOA )
                dplyr::mutate(!!population_yoa_plus_1_entry :=
                                dplyr::lag(!!dplyr::sym(population_yoa_end)))

            }
          )
        , .after = projection_if_exposed_nested
      )

    # PREMATURE DEATHS (SINGLE YEAR EXPOSURE) ######################################################
    # YOA = YEAR OF ANALYSIS
    if (health_outcome == "deaths" &
        base::unique(input_with_risk_and_pop_fraction |> dplyr::select(dplyr::contains("approach_exposure"))== "single_year")[1]) {

      pop <- pop |>
        # Premature deaths = ( impacted scenario YOA end-of-year population ) - ( baseline scenario YOA end-of-year pop )
        dplyr::mutate(
          deaths_by_age_and_year_nested =
            purrr::map2(
              .x = projection_if_unexposed_nested,
              .y = projection_if_exposed_nested,
              .f = ~ {
                tibble::tibble(
                  age_start = .x$age_start,
                  age_end = .x$age_end) |>
                  dplyr::bind_cols(
                    tibble::as_tibble(
                      stats::setNames(
                        base::list(.x[[population_yoa_end]] - .y[[population_yoa_end]]),
                        impact_yoa)))
                }),
          .after = projection_if_unexposed_nested)

    }

    # YLL & PREMATURE DEATHS (CONSTANT EXPOSURE) ####################################################

    if ((health_outcome %in% c("yll")| #And  ("yld", "daly") if yld from lifetable ever implemented
         (base::unique(input_with_risk_and_pop_fraction |> dplyr::select(dplyr::contains("approach_exposure")) == "constant")[1] & health_outcome == "deaths"))) {

      ## PROJECT POPULATIONS #########################################################################

      ### DEFINE FUNCTION FOR POPULATION PROJECTION ##################################################

      project_pop <- function(df, prob_survival, prob_survival_until_mid_year) {
        # Store useful variables such number_years
        # The number_years argument defines for how many years the population should be projected;
        # might be easier to have two arguments "start year" and "end year"
        number_years <-
          nrow(df) - 1

        # Define the years based on number_years
        # e.g. 2020 to 2118
        years_projection <-
          (input_with_risk_and_pop_fraction |>  dplyr::pull(year_of_analysis) |> dplyr::first() + 1) : (input_with_risk_and_pop_fraction |>  dplyr::pull(year_of_analysis) |> dplyr::first() + number_years)

        # Initialize matrices for entry population, mid-year population, and deaths
        pop_entry <- matrix(NA, nrow = 100, ncol = number_years)
        # Provide column names (population_year)
        # e.g. population_2020 to population_2118
        colnames(pop_entry) <-
          base::paste0("population_", years_projection , "_entry")

        # Same for mid-year population
        pop_mid <- matrix(NA, nrow = 100, ncol = number_years)
        colnames(pop_mid) <-
          base::paste0("population_", years_projection)

        # Same for deaths
        deaths <- matrix(NA, nrow = 100, ncol = number_years)
        colnames(deaths) <-
          base::paste0("deaths_", years_projection)

        # Set initial population for the first year (2020)
        pop_entry[, 1] <- df[[base::paste0("population_", input_with_risk_and_pop_fraction |>  dplyr::pull(year_of_analysis) |> dplyr::first() + 1, "_entry")]]
        pop_mid[, 1] <- pop_entry[, 1] * prob_survival_until_mid_year
        deaths[, 1] <- pop_entry[, 1] * (1 - prob_survival)

        # Loop across years
        # E.g. starts with 1 and ends with 98;
        # i (index in the number of years) is used to select both the rows and the columns

        for (i in 1:(number_years-1)) {

          # ENTRY POP YOA+1 <- ( ENTRY POP YOA ) * ( SURVIVAL PROBABILITY YOA )
          pop_entry[(i + 2):(number_years + 1), i + 1] <-
            pop_entry[(i + 1):(number_years), i] * prob_survival[(i + 1):(number_years)]

          # MID-YEAR POP YOA+1 <- ( ENTRY POP YOA+1) * ( SURVIVAL PROBABILITY FROM START OF YOA+1 TO MID YEAR YOA+1)
          pop_mid[(i + 2):(number_years+1), i + 1] <-
            pop_entry[(i + 2):(number_years + 1), i + 1] * prob_survival_until_mid_year[(i + 2):(number_years + 1)]

          # DEATHS IN YOA+1 <- ( ENTRY POP YOA+1 ) * (1 - SURVIVAL PROBABILITY YOA+1 )
          deaths[(i + 2):(number_years+1), i + 1] <-
            pop_entry[(i + 2):(number_years + 1), i + 1] * ( 1 - prob_survival[(i + 2):(number_years + 1)] )

        }

        # Column bin matrices to input data frame
        # Remove first column of pop_entry, because it exists already in input data frame
        df <- df |>
          dplyr::bind_cols(pop_mid) |>
          dplyr::bind_cols(pop_entry[, -1]) |>
          dplyr::bind_cols(deaths)

        return(df)
      }

      ### SINGLE YEAR EXPOSURE #######################################################################
      # Determine YLLs for baseline and impacted scenario's in the single year exposure case

      if (base::unique(input_with_risk_and_pop_fraction |> dplyr::select(dplyr::contains("approach_exposure")) == "single_year")[1]){

        # PROJECT POPULATIONS IN BOTH IMPACTED AND BASELINE SCENARIO FROM YOA+1 UNTIL THE END
        # USING MODIFIED SURVIVAL PROBABILITIES (BECAUSE AFTER YOA THERE IS NO MORE AIR POLLUTION)
        pop <- pop |>
          dplyr::mutate(
            projection_if_exposed_nested =
              purrr::map(
                .x = projection_if_exposed_nested,
                function(.x){
                  project_pop(df = .x,
                              prob_survival = .x$prob_survival_mod,
                              prob_survival_until_mid_year = .x$prob_survival_until_mid_year_mod)
                }
              )
          )

        pop <- pop |>
          dplyr::mutate(
            projection_if_unexposed_nested =
              purrr::map(
                .x = projection_if_unexposed_nested,
                function(.x){
                  project_pop(df = .x,
                              prob_survival = .x$prob_survival_mod,
                              prob_survival_until_mid_year = .x$prob_survival_until_mid_year_mod)
                }
              )
          )

        ### CONSTANT EXPOSURE ########################################################################
        # Determine YLLs for baseline and impacted scenario's in the constant exposure case

      } else {

        # PROJECT POPULATION IN BASELINE SCENARIO
        pop <- pop |>
          dplyr::mutate(
            projection_if_exposed_nested =
              purrr::map(
                .x = projection_if_exposed_nested,
                function(.x){
                  project_pop(df = .x,
                              prob_survival = .x$prob_survival,
                              prob_survival_until_mid_year = .x$prob_survival_until_mid_year)
                }
              )
          )

        # PROJECT POPULATION IN IMPACTED SCENARIO
        pop <- pop |>
          dplyr::mutate(
            projection_if_unexposed_nested =
              purrr::map(
                .x = projection_if_unexposed_nested,
                function(.x){
                  project_pop(df = .x,
                              prob_survival = .x$prob_survival_mod,
                              prob_survival_until_mid_year = .x$prob_survival_until_mid_year_mod)
                }
              )
          )
      }

      ###  DETERMINE IMPACT (YLL, PREMATURE DEATHS (CONSTANT EXPOSURE))  ###########################
      # YLL and premature deaths attributable to exposure are calculated

      pop <- pop |>
        dplyr::mutate(
          yll_by_age_and_year_nested = purrr::map2(
            .x = projection_if_unexposed_nested,
            .y = projection_if_exposed_nested,

            function(.x, .y){

              ages <- .x |>
                dplyr::select(age_start, age_end)

              unexposed <- .x |>
                dplyr::select(dplyr::contains("population"),
                              -dplyr::all_of(population_yoa_end),
                              -dplyr::contains("entry"))
              exposed <- .y |>
                dplyr::select(dplyr::contains("population"),
                              -dplyr::all_of(population_yoa_end),
                              -dplyr::contains("entry"))

              # Difference in mid-year populations of baseline and impacted scenario equals attributable YLL
              pop_diff <-
                unexposed - exposed

              # Add ages (in other pipeline because it does not work in one)
              pop_diff <-
                dplyr::bind_cols(ages, pop_diff) |>
                # Rename to impact
                dplyr::rename_with(
                  .cols = dplyr::starts_with("population_"),
                  .fn = ~ base::gsub("population_", "impact_", .x)
                )

              return(pop_diff)
            }

          ),
          .before = 1)




      pop <- pop |>
        dplyr::mutate(
          deaths_by_age_and_year_nested = purrr::map2(
            .x = projection_if_exposed_nested,
            .y = projection_if_unexposed_nested,
            function(.x, .y){

              exposed <- .x |>
                dplyr::select(dplyr::contains("deaths"),
                              -deaths)
              unexposed <- .y |>
                dplyr::select(dplyr::contains("deaths"),
                              -deaths)

              ages <- .x |>
                dplyr::select(age_start, age_end)

              # Calculate difference in deaths
              # Baseline scenario minus impacted scenario
              pop_diff <- exposed - unexposed

              # Add ages (in other pipeline because it does not work in one)
              pop_diff <-
                dplyr::bind_cols(ages, pop_diff) |>
              # Rename to impact
              dplyr::rename_with(
                .cols = dplyr::starts_with("deaths_"),
                .fn = ~ base::gsub("deaths_", "impact_", .x)
              )



              return(pop_diff)
            }
          )
          , .before = 1)


      ## NEWBORNS #################################################################

      if (base::unique(input_with_risk_and_pop_fraction |> dplyr::select(dplyr::contains("approach_newborns")) == "with_newborns")[1]) {

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

        pop <- pop |>
          dplyr::mutate(
            yll_by_age_and_year_nested = purrr::map(
              .x = yll_by_age_and_year_nested,
              function(.x){
                .x[, dplyr::setdiff(names(.x), c("age_start", "age_end"))] <- fill_right_of_diag(.x[, dplyr::setdiff(names(.x), c("age_start", "age_end"))])
                return(.x)
              }
            )
            , .before = 1)

        pop <- pop |>
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

     pop <- pop |>
      dplyr::mutate(
        pop_impact_nested =
          if({{health_outcome}} == "deaths") deaths_by_age_and_year_nested else yll_by_age_and_year_nested)

    # Remove from pop, as already present in input_with_risk_...
    pop <- pop |>
      dplyr::select(-lifetable_with_pop_nested)

    if (health_outcome %in% c("deaths", "yll")){

      joining_columns_pop_impact <-
        healthiar:::find_joining_columns(input_with_risk_and_pop_fraction,
                                         pop,
                                         except = "lifetable_with_pop_nested")

      pop_impact <-
        input_with_risk_and_pop_fraction |>
        dplyr::right_join(pop, by = joining_columns_pop_impact) |>
        dplyr::relocate(dplyr::contains("nest"), .before = 1)}


    on.exit(options(user_options))

    # GET DEATHS AND YLL FROM LIFETABLE

    ## Define health_outcome variable
    health_outcome <- base::unique(input_with_risk_and_pop_fraction$health_outcome)

    # Determine default time horizon for YLL/YLD if not specified ##############
    if ( health_outcome %in% c("yll") & # And ("yld")  if ever implemented
         !"time_horizon" %in% base::names(input_with_risk_and_pop_fraction ) ) {

      time_horizon <- input_with_risk_and_pop_fraction |>
        dplyr::slice(1) |>                      # Select the first row
        dplyr::pull(lifetable_with_pop_nested) |> # Extract the nested tibble column
        purrr::pluck(1) |>                      # Get the tibble stored in the first element
        base::nrow()

      ## Add time_horizon to tibble
      input_with_risk_and_pop_fraction <- input_with_risk_and_pop_fraction |>
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
                  ## only ages between "max_age" and "input_with_risk_and_pop_fraction |>  pull(min_age) |> first()" filtered for above
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
                dplyr::select(.data = _, dplyr::all_of(base::paste0("impact_", year_of_analysis))) |>
                base::sum(na.rm = TRUE)
              return(.x)
            }
          )
        ) |>
        dplyr::mutate(impact = base::as.numeric(impact))


    }

    # Store total, YLL/YLD in YOA in column impact_nested #########
    ## Single number

    if ( health_outcome %in% c("yll")){ # And ("yld")  if ever implemented

      impact_detailed <- impact_detailed |>

        ## Add column for year of analysis
        dplyr::mutate(year_of_analysis = year_of_analysis) |>
        ## Add column for time horizon
        dplyr::mutate(time_horizon = input_with_risk_and_pop_fraction |>  dplyr::pull(time_horizon) |> dplyr::first()) |>
        ## Add column for last year of analysis
        dplyr::mutate(last_year = year_of_analysis + time_horizon - 1)

      ## Sum impacts
      impact_detailed <- impact_detailed |>

        dplyr::mutate(
          impact_nested = purrr::pmap(
            base::list(.x = impact_by_year_nested, .y = last_year, health_outcome = base::unique(health_outcome)),
            function(.x, .y, health_outcome){

              ## If yll or yld
              if( health_outcome %in% c("yll")){ # And ("yld")  if ever implemented

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
