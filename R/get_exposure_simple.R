#' Get exposure as a simple average concentration

# DESCRIPTION ##################################################################
#' @description
#' This function calculates the simple spatial average pollution concentration in each geographic unit.

# ARGUMENTS ####################################################################
#' @param poll_grid \code{SpatRaster} of the pollution concentration data.
#' @param geo_units \code{sf} of the geographic units.
#' @param geo_id_micro \code{Numeric or string vector} of the IDs of the geographic units.

# VALUE ########################################################################
#' @return
#' This function returns a \code{list} containing:
#' @returns
#' 1) \code{main} (\code{list}) containing the main results as vectors;
#' \itemize{
#'  \item \code{geo_id_micro} (\code{string} column) containing the geographic IDs of the assessment
#'  \item \code{exposure_mean} (\code{numeric} column) containing the mean exposure
#' }
#' @returns
#' 2) \code{detailed} (\code{list}) containing detailed (and interim) results.

#' @export

#' @author Arno Pauwels & Liliana Vazquez Fernandez

get_exposure_simple <-
  function(
    poll_grid,
    geo_units,
    geo_id_micro
  ) {
    ## check for matching CRS
    if (sf::st_crs(geo_units) != sf::st_crs(poll_grid)) {
      geo_units <- sf::st_transform(geo_units, st_crs(poll_grid))
      warning("'geo_units' was reprojected to match the CRS of 'poll_grid'.")}
    
    ## crop & mask pollution grid
    poll_grid <- terra::mask(terra::crop(poll_grid, terra::vect(geo_units)), terra::vect(geo_units))
    
    ## rename pollution grid
    base::names(poll_grid) <- "poll"
    
    ## calculate mean concentration value and other stats by geographical unit
    exp_mean <- base::data.frame(
      geo_id_micro = geo_id_micro,
      mean = exactextractr::exact_extract(
        poll_grid, 
        geo_units,
        fun = "mean",
        progress = FALSE
      ),
      median = exactextractr::exact_extract(
        poll_grid, 
        geo_units,
        fun = "median",
        progress = FALSE
      ),
      min = exactextractr::exact_extract(
        poll_grid, 
        geo_units,
        fun = "min",
        progress = FALSE
      ),
      lower = exactextractr::exact_extract(
        poll_grid, 
        geo_units,
        fun = "quantile",
        quantiles = 0.025,
        progress = FALSE
      ),
      upper = exactextractr::exact_extract(
        poll_grid, 
        geo_units,
        fun = "quantile",
        quantiles = 0.975,
        progress = FALSE
      ),
      max = exactextractr::exact_extract(
        poll_grid, 
        geo_units,
        fun = "max",
        progress = FALSE
      ),
      stdev = exactextractr::exact_extract(
        poll_grid, 
        geo_units,
        fun = "stdev",
        progress = FALSE
      )
    )
    
    ## build output lists
    exposure_main <- base::list(
      geo_id_micro = exp_mean$geo_id_micro,
      exposure_mean = exp_mean$mean
    )
    
    exposure_detailed <- base::list(
      geo_id_micro = exp_mean$geo_id_micro,
      exposure_mean = exp_mean$mean,
      exposure_median = exp_mean$median,
      exposure_min = exp_mean$min,
      exposure_lower = exp_mean$lower,
      exposure_upper = exp_mean$upper,
      exposure_max = exp_mean$max,
      exposure_stdev = exp_mean$stdev
    )
    
    out <- base::list(
      exposure_main = exposure_main,
      exposure_detailed = exposure_detailed
    )
    
    return(out)
}
