#' Prepare exposure data

# DESCRIPTION ##################################################################
#' @description
#' This function prepares tabular population exposure data compatible with the \code{attribute()} and \code{compare()} functions, based on gridded pollution concentration data and vector data representing geographic units. The function calculates an average concentration value in each geographic unit, weighted by the fraction of the population in each sub-unit.

# ARGUMENTS ####################################################################
#' @param poll_grid \code{SpatRaster} of the pollution concentration data.
#' @param geo_units \code{sf} of the geographic sub-units.
#' @param population \code{Numeric vector} containing the total population number in each geographic sub-unit.
#' @param geo_id_macro \code{Numeric or string vector} containing the higher-level IDs of the geographic units the sub-unit belong to and will be aggregated at.

# VALUE ########################################################################
#' @return
#' This function returns a \code{list} containing:
#' @returns
#' 1) \code{main} (\code{tibble}) containing the main results as vectors;
#' \itemize{
#'  \item \code{geo_id_macro} (\code{string} column) containing the (higher-level) geographic IDs of the assessment
#'  \item \code{exp_value} (\code{numeric} column) containing the (population-weighted) mean exposure
#'  \item \code{exp_type} (\code{string} column) specifying the exposure type
#' }
#' @returns
#' 2) \code{detailed} (\code{list}) containing detailed (and interim) results.

# EXAMPLES #####################################################################
#' @examples
#' # Goal: determine population-weighted mean PM2.5 exposure for several
#' neighborhoods of Brussels (Belgium)
#'
#' exdat_pwm_1 <- terra::rast(system.file("extdata", "exdat_pwm_1.tif", package = "healthiar"))
#' exdat_pwm_2 <- sf::st_read(system.file("extdata", "exdat_pwm_2.gpkg", package = "healthiar"), quiet = TRUE)
#'
#' pwm <- prepare_exposure(
#'   poll_grid = exdat_pwm_1, # Formal class SpatRaster
#'   geo_units = exdat_pwm_2, # sf of the geographic sub-units
#'   population = sf::st_drop_geometry(exdat_pwm_2$population), # population per geographic sub-unit
#'   geo_id_macro = sf::st_drop_geometry(exdat_pwm_2$region) # higher-level IDs to aggregate at
#' )
#'
#' pwm$main # population-weighted mean exposures for the (higher-level) geographic units

#' @export

#' @author Arno Pauwels

prepare_exposure <-
  function(
    poll_grid,
    geo_units,
    population,
    geo_id_macro
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
        geo_id_macro,
        population,
        poll_mean
      )
    )

    ## create table to calculate pop-weighted exposure
    exposure <- base::data.frame(
      geo_id_macro,
      population,
      poll_mean
    )

    ## calculate population-weighted mean concentration in each aggregated geo unit
    exposure <- exposure |>
      # group_by() instead of .by= because we need to keep the group
      # for mutate() and summarize()
      dplyr::group_by(geo_id_macro) |>
      dplyr::mutate(poll_weighted = population / sum(population) * poll_mean) |>
      dplyr::summarise(exp_value = base::sum(poll_weighted)) |>
      dplyr::ungroup() |>
      dplyr::mutate(exp_type = 'Population-weighted mean concentration')

    ## build output list
    main <- base::as.list(exposure)
    raw <- base::as.list(non_aggregated_exposure)

    detailed <- base::list(raw)
    base::names(detailed) <- 'raw'

    output <- base::list(main, detailed)
    base::names(output) <- c('main', 'detailed')

    return(output)
  }

