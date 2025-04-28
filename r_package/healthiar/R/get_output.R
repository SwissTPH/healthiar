#' Attributable health cases based on relative risk

#' @description Distributes and store outputs by level of detail by aggregating or filtering impacts.
#' @param input_args \code{List} containingall arguments and values entered in attribute().
#' @param input_table \code{List} containing the input_table data compiled and packed in a data frame.
#' @param impact_raw \code{List} containing all the calculation of health impacts.
#' @returns
#' TBD. E.g. This function returns a \code{data.frame} with one row for each value of the
#' concentration-response function (i.e. central, lower and upper bound confidence interval.
#' Moreover, the data frame includes columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }
#' @examples
#' TBD
#' @author Alberto Castro
#' @keywords internal
get_output <-
  function(input_args = NULL,
           input_table = NULL,
           impact_raw){

    # Get main results from detailed results ###################################

    health_detailed_from_impact  <-
      list(input_args = input_args,
           input_table = input_table,
           impact_raw = impact_raw)

    if(any(grepl("nest", names(impact_raw)))){
      impact_main <-
        impact_raw |>
        dplyr::select(-dplyr::contains("nest"))|>
        dplyr::filter(sex %in% "total")

      if ("duration_ci" %in% names(impact_main)){impact_main <- impact_main |> dplyr::filter(duration_ci %in% "central")}
      if ("dw_ci" %in% names(impact_main)){impact_main <- impact_main |> dplyr::filter(dw_ci %in% "central")}


      ## Classify results in main and detailed
      output <-
        list(health_main = impact_main,
             health_detailed = health_detailed_from_impact)


    } else {

      # Store output so far
      # The main will change below that we give a first value
      output <-
        list(health_main = impact_raw,
             health_detailed = health_detailed_from_impact)}

    # Keep the last version
      output_last <- output[["health_main"]]



    # Absolute risk ############

    if(unique(impact_raw$approach_risk) == "absolute_risk") {

      output[["health_detailed"]][["impact_agg_exp_cat"]] <-
        output_last |>
        # Remove all impact rounded because
        # we have to round final results
        # not summing rounded results ("too rounded")
        dplyr::select(-dplyr::any_of(paste0("impact_rounded", c("", "_1", "_2")))) |>
        dplyr::group_by(geo_id_disaggregated) |>
        # Collapse the exposure categories to have only a vector
        dplyr::mutate(dplyr::across(dplyr::any_of(
          c(paste0("exp", c("", "_1", "_2")),
            paste0("pop_exp", c("", "_1", "_2")),
            paste0("prop_pop_exp", c("", "_1", "_2")),
            "exposure_dimension")),
          ~ paste(., collapse = ", ")))

      output[["health_detailed"]][["impact_agg_exp_cat"]] <-
        output[["health_detailed"]][["impact_agg_exp_cat"]] |>
        # Sum columns to summarize
        dplyr::group_by(
          dplyr::across(-dplyr::any_of(
            c("geo_id_disaggregated",
              paste0("exp", c("", "_1", "_2")),
              paste0("population", c("", "_1", "_2")),
              paste0("prop_pop_exp", c("", "_1", "_2")),
              paste0("pop_exp", c("", "_1", "_2")),
              paste0("rr_conc", c("", "_1", "_2")),
              paste0("pop_fraction", c("", "_1", "_2")),
              paste0("absolute_risk_as_percent", c("", "_1", "_2")),
              paste0("impact", c("", "_1", "_2")),
              paste0("impact_per_100k_inhab", c("", "_1", "_2")))))) |>
        dplyr::summarize(
          dplyr::across(dplyr::any_of(
            c(paste0("absolute_risk_as_percent", c("", "_1", "_2")),
              paste0("impact", c("", "_1", "_2")),
              "impact_social")),
                 ~sum(.x, na.rm = TRUE)),
          .groups = "drop") |>
        # Round impact
        dplyr::mutate(impact_rounded = round(impact, 0))


      output_last <- output[["health_detailed"]][["impact_agg_exp_cat"]]

    }

    # Aggregate results by higher geo_level
    # only if geo_id_aggregated is defined
    if("geo_id_aggregated" %in% names(output_last)){

      output[["health_detailed"]][["impact_agg_geo"]]  <-
        output_last |>
        # Group by higher geo level
        dplyr::group_by(dplyr::across(dplyr::any_of(
                          c("exposure_name",
                            "geo_id_aggregated",
                            "erf_ci", "exp_ci", "bhd_ci", "dw_ci"))))

        if (!"population" %in% names(output_last)) {
          output[["health_detailed"]][["impact_agg_geo"]]  <- output[["health_detailed"]][["impact_agg_geo"]] |>
          dplyr::summarise(impact = sum(impact),
                           impact_rounded = round(impact),
                           .groups = "drop")

        } else {
          output[["health_detailed"]][["impact_agg_geo"]]  <- output[["health_detailed"]][["impact_agg_geo"]] |>
          dplyr::summarise(impact = sum(impact),
                           impact_rounded = round(impact),
                           population = sum(population),
                           impact_per_100k_inhab = (impact / population) * 1E5,
                           .groups = "drop")
        }


      output_last <- output[["health_detailed"]][["impact_agg_geo"]]

    }

    # Aggregate results across pollutants (exposures)
    if("approach_multiexposure" %in% names(impact_raw)){
      if(unique(impact_raw$approach_multiexposure) %in% "additive"){

      output[["health_detailed"]][["impact_agg_exp_names"]]  <-
        output_last |>
        dplyr::mutate(
          exposure_name = paste(unique(exposure_name), collapse = ", ")) |>
        # Group by higher geo level
        dplyr::group_by(dplyr::across(dplyr::any_of(c("geo_id_aggregated", "exp_ci",
                                        "bhd_ci", "erf_ci","dw_ci", "cutoff_ci", "duration_ci"))))|>
        dplyr::summarise(impact = sum(impact),
                         impact_rounded = round(impact),
                         .groups = "drop")

      output_last <- output[["health_detailed"]][["impact_agg_exp_names"]]
      }
    }

    # Keep only exp_ci = central and bhd_ci = central in main output ###########
    output[["health_main"]] <-
      output_last |>
      dplyr::filter(exp_ci %in% c("central"))

    if("bhd_ci" %in% names(output[["health_main"]])) {

      output[["health_main"]] <- output[["health_main"]] |>
        dplyr::filter(bhd_ci %in% c("central"))}

    if("cutoff_ci" %in% names(output[["health_main"]])) {

      output[["health_main"]] <- output[["health_main"]] |>
        dplyr::filter(cutoff_ci %in% c("central"))}

    if(any(grepl("dw_", names(output[["health_main"]])))) {

      output[["health_main"]] <- output[["health_main"]] |>
        dplyr::filter(dw_ci %in% "central")}

    if("duration_ci" %in% names(output[["health_main"]])) {

      output[["health_main"]] <- output[["health_main"]] |>
        dplyr::filter(duration_ci %in% c("central"))}

    # Order columns ############################################################
    # putting first (on the left) those that determine different results across rows

    id_columns <- c("geo_id_aggregated", "geo_id_disaggregated",
                    "sex",
                    "erf_ci","exp_ci", "bhd_ci", "cutoff_ci", "dw_ci", "duration_ci")

    impact_columns <-paste0(c("impact", "impact_rounded", "impact_per_100k_inhab",
                              "monetized_impact", "monetized_impact_rounded"),
                            rep(c("", "_1", "_2"), each = 3))

    first_columns <- c(id_columns, impact_columns)

    put_first_cols <-
      function(x, cols){
        dplyr::select(x,
                      dplyr::any_of(cols),
                      dplyr::everything())
      }

    put_first_cols_recursive <-
      function(x, cols){

        # If x is a data.frame
        if(is.data.frame(x)){
          put_first_cols(x, cols)

        # If x is list and all list elements are data frames (and not lists)
        }else if (is.list(x) & all(purrr::map_lgl(x, is.data.frame))){
          purrr::map(
            .x = x,
            .f = ~ put_first_cols(.x, cols))

        }else{x}

      }

    output <-
      purrr:::map(
        .x = output,
        .f = ~ put_first_cols_recursive(x = .x,
                                        cols = first_columns))




    return(output)
  }
