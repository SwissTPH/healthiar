#' Get deaths, YLL or YLD
#'
#' @description Get attributable deaths, years of life lost or years lived with disability from lifetable
#' @inheritParams attribute_master
#' @returns
#' This function returns a \code{List}
#' @examples
#' TBD
#' @author Alberto Castro & Axel Luyten
#' @note Experimental function
#' @keywords internal
#'
#'
#'
get_deaths_yll_from_lifetable <-
  function(pop_impact,
           input_with_risk_and_pop_fraction) {

    ## Define health_outcome variable
    health_outcome <- base::unique(input_with_risk_and_pop_fraction$health_outcome)

    # Determine default time horizon for YLL/YLD if not specified ##############
    if ( health_outcome %in% c("yll") & # And ("yld")  if ever implemented
         !"time_horizon" %in% base::names(input_with_risk_and_pop_fraction ) ) {

        time_horizon <- input_with_risk_and_pop_fraction |>
          dplyr::slice(1) |>                      # Select the first row
          dplyr::pull(lifetable_with_pop_nest) |> # Extract the nested tibble column
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
        lifeyears_nest =
          purrr::pmap(
            base::list(.x =  pop_impact_nest,
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
                    dplyr::across(dplyr::contains("population"),
                           ~ {mat <- base::as.matrix(.x)
                           mat[base::upper.tri(mat, diag = FALSE)] <- NA
                           return(mat)}))
              }


              # Filter for relevant ages #########################################
              # use {{}} to refer to the argument and avoid warnings

              if ( !base::is.null( max_age) ) {

                .x <-
                  dplyr::filter(.x, age <= max_age)
              }

              if ( !base::is.null( min_age ) ) {
                .x <-
                  dplyr::filter(.x, age >= min_age)
              }

              # Calculate YLL/YLD impact per year ################################

              if ( health_outcome %in% c("yll") ) { # And ("yld")  if ever implemented

                .x <- .x |>
                  dplyr::select(dplyr::contains("population_")) |>

                  ## Sum over ages (i.e. vertically)
                  ## only ages between "max_age" and "input_with_risk_and_pop_fraction |>  pull(min_age) |> first()" filtered for above
                  dplyr::summarize_all(sum, na.rm = TRUE) |>

                  ## Reshape to long format
                  ## (output is data frame with 2 columns "year" & "impact")
                  tidyr::pivot_longer(cols = dplyr::starts_with("population_"),
                                      names_to = "year",
                                      values_to = "impact",
                                      names_prefix = "population_") |>

                  ## Convert year to numeric
                  dplyr::mutate(year = base::as.numeric(year))
              } else
                .x<-.x}), .before = 1)

    # YLD if ever implemented###################################################

    # ## Determine year- and age specific YLD
    # if ( health_outcome %in% "yld" ) {
    #
    #   impact_detailed <- impact_detailed |>
    #     dplyr::mutate(yll_nest =
    #                     purrr::map2(
    #       .x = yll_nest, .y = dw,
    #       function(yll_nest, dw){
    #         # YLL * DW = YLD
    #         yll_nest <- yll_nest * dw
    #         return(yll_nest)
    #       }
    #     )
    #     )
    #
    #   ## Determine total YLD per year
    #   impact_detailed <- impact_detailed |>
    #     dplyr::mutate(lifeyears_nest =
    #                     purrr::map2(
    #                       .x = lifeyears_nest, .y = dw,
    #                       function(lifeyears_nest, dw){
    #                         lifeyears_nest <- lifeyears_nest |>
    #                           mutate(impact = impact * dw)
    #                         return(lifeyears_nest)
    #                       }
    #                     ))
    #
    # }


    # Deaths ###################################################################

    ## Store total deaths in YOA in column impact_nest
    if ( health_outcome == "deaths" ) {

      impact_detailed <- impact_detailed |>
        # Store in new column "impact_nest"
        dplyr::mutate(
          impact = purrr::map(
            .x = lifeyears_nest,
            function(.x){

              .x <- .x |>
                dplyr::select(.data = _, dplyr::all_of(base::paste0("deaths_", year_of_analysis))) |>
                base::sum(na.rm = TRUE)
              return(.x)
            }
          )
        ) |>
        dplyr::mutate(impact = base::as.numeric(impact))


    }

    # Store total, YLL/YLD in YOA in column impact_nest #########
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
          impact_nest = purrr::pmap(
            base::list(.x = lifeyears_nest, .y = last_year, health_outcome = base::unique(health_outcome)),
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
          impact_for_discounting_nest = impact_nest
        ) |>
        ## Unnest the obtained impacts to integrate them the main tibble
        ## Impact saved in column impact
        tidyr::unnest(impact_nest)

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
      dplyr::mutate(dplyr::across(dplyr::contains("_nest"),
                           ~rlang::set_names(.x,
                                      id)))

    return(impact_detailed)

  }
