#' Prepare exposure data

# DESCRIPTION ##################################################################
#' @description
#' This function prepares tabular population exposure data compatible with the \code{attribute()} and \code{compare()} functions, 
#' based on gridded pollution concentration data and polygon data representing geographic units. 
#' If population data is provided, the function calculates an average concentration value in each geographic unit
#' that is weighted with the population number at each location.
#' If no population data is provided, the function calculates the simple spatial average concentration in each geographic unit.

# ARGUMENTS ####################################################################
#' @param poll_grid \code{SpatRaster} of the pollution concentration data.
#' @param geo_units \code{sf} of the geographic units or sub-units.
#' @param population \code{Integer vector} of the total population number in each geographic sub-unit.
#' @param pop_grid \code{SpatRaster} of the gridded population data.
#' @param geo_id_micro \code{Numeric or string vector} of the IDs of the geographic units. Required if \code{pop_grid} is given or if no population data is provided.
#' @param geo_id_macro \code{Numeric or string vector} of the higher-level IDs of the geographic units the sub-unit belong to and will be aggregated at. Required if \code{population} is provided.
#' @param bin_width \code{Numeric} specifying the width of the population exposure bins.

# VALUE ########################################################################
#' @return
#' This function returns a \code{list} containing:
#' @returns
#' 1) \code{main} (\code{list}) containing the main results as vectors;
#' \itemize{
#'  \item \code{geo_id_micro} of \code{geo_id_macro} (\code{string} column) containing the (higher-level) geographic IDs of the assessment
#'  \item \code{exposure_mean} (\code{numeric} column) containing the (population-weighted) mean exposure
#'  \item \code{population_total} (\code{integer} column) containing the total population in each geographic unit, if population data was provided
#' }
#' @returns
#' 2) \code{detailed} (\code{list}) containing detailed (and interim) results.

# EXAMPLES #####################################################################
#' @examples
#' # Goal: determine population-weighted mean PM2.5 exposure for several
#' # neighborhoods of Brussels (Belgium)
#'
#' exdat_pwm_1 <- terra::rast(system.file("extdata", "exdat_pwm_1.tif", package = "healthiar"))
#' exdat_pwm_2 <- sf::st_read(
#'     system.file("extdata", "exdat_pwm_2.gpkg", package = "healthiar"),
#'     quiet = TRUE
#' )
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

#' @author Arno Pauwels & Liliana Vazquez Fernandez

prepare_exposure <-
  function(
    poll_grid,
    geo_units,
    population = NULL,
    pop_grid = NULL,
    geo_id_micro = NULL,
    geo_id_macro = NULL,
    bin_width = 0.1
  ) {
    ## check required packages
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("The 'terra' package is required for this function. Please install it if you want to use this function.", call. = FALSE)}
    if (!requireNamespace("sf", quietly = TRUE)) {
      stop("The 'sf' package is required for this function. Please install it if you want to use this function.", call. = FALSE)}
    if (!requireNamespace("exactextractr", quietly = TRUE)) {
      stop("The 'exactextractr' package is required for this function. Please install it if you want to use this function.", call. = FALSE)}
    
    if (base::is.null(population) & base::is.null(pop_grid)) {
      out <- get_exposure_simple(
        poll_grid = poll_grid,
        geo_units = geo_units,
        geo_id_micro = geo_id_micro
      )
    }
    
    if (!base::is.null(pop_grid)) {
      out <- get_exposure_grid(
        poll_grid = poll_grid,
        geo_units = geo_units,
        pop_grid = pop_grid,
        geo_id_micro = geo_id_micro,
        bin_width = bin_width
      )
    }
    
    if (!base::is.null(population)) { 
      out <- get_exposure_unit(
        poll_grid = poll_grid,
        geo_units = geo_units,
        population = population,
        geo_id_macro = geo_id_macro,
        bin_width = bin_width
      )
    }
    
    return(out)
}
