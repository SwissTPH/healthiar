#' Prepare exposure data

# DESCRIPTION ##################################################################
#' @description
#' This function prepares tabular population exposure data compatible with the \code{attribute()} and \code{compare()} functions, based on gridded pollution concentration data and vector data representing geographic units. The function calculates an average concentration value in each geographic unit, weighted by the fraction of the population in each sub-unit.

# ARGUMENTS ####################################################################
#' @param poll_grid \code{SpatRaster} of the pollution concentration data.
#' @param geo_units \code{sf} of the geographic sub-units.
#' @param population \code{Vector} containing the total population number in each geographic sub-unit.
#' @param geo_id_aggregated \code{Vector} containing the id code of the geographic unit the sub-unit belongs to.

# VALUE ########################################################################
#' @return
#' This function returns a vector of population exposure values.

# EXAMPLES #####################################################################
#' @examples
#' # TODO

#' @export

#' @author Arno Pauwels

prepare_exposure <-
  function(
    poll_grid,
    geo_units,
    population,
    geo_id_aggregated
  ){
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("The 'terra' package is required for this function. Please install it if you want to use this function.", call. = FALSE)}
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("The 'sf' package is required for this function. Please install it if you want to use this function.", call. = FALSE)}

    ## extract mean concentration in each raw geo unit
    poll_mean <- terra::extract(
      poll_grid, geo_units,
      fun = function(x) {base::mean(x, na.rm = T)})[, 2]
    ## create table of non-aggregated exposure for raw output
    non_aggregated_exposure <- base::as.data.frame(
      base::cbind(
        geo_id_aggregated,
        population,
        poll_mean
      )
    )

    ## create table to calculate pop-weighted exposure
    exposure <- base::data.frame(
      geo_id_aggregated,
      population,
      poll_mean
    )

    ## calculate population-weighted mean concentration in each aggregated geo unit
    exposure <- exposure |>
      # group_by() instead of .by= because we need to keep the group
      # for mutate() and summarize()
      dplyr::group_by(geo_id_aggregated) |>
      dplyr::mutate(poll_weighted = population / sum(population) * poll_mean) |>
      dplyr::summarise(exp_value = base::sum(poll_weighted)) |>
      dplyr::ungroup() |>
      dplyr::mutate(exp_type = 'Population-weighted mean concentration')

    ## build output list
    main <- base::as.list(exposure)
    raw <- base::as.list(non_aggregated_exposure)

    detailed <- list(raw)
    base::names(detailed) <- 'raw'

    output <- list(main, detailed)
    base::names(output) <- c('main', 'detailed')

    return(output)
  }

