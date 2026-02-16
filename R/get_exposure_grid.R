#' Get exposure as a population-weighted average concentration based on gridded population

# DESCRIPTION ##################################################################
#' @description
#' This function calculates an average concentration value in each geographic unit,
#' where the concentration values are weighted with the values of a population grid.

# ARGUMENTS ####################################################################
#' @param poll_grid \code{SpatRaster} of the pollution concentration data.
#' @param geo_units \code{sf} of the geographic units.
#' @param pop_grid \code{SpatRaster} of the gridded population data.
#' @param geo_id_micro \code{Numeric or string vector} of the IDs of the geographic units. Required if \code{pop_grid} is given or if no population data is provided.
#' @param bin_width \code{Numeric} specifying the width of the population exposure bins.

# VALUE ########################################################################
#' @return
#' This function returns a \code{list} containing:
#' @returns
#' 1) \code{main} (\code{list}) containing the main results as vectors;
#' \itemize{
#'  \item \code{geo_id_micro} (\code{string} column) containing the geographic IDs of the assessment
#'  \item \code{exposure_mean} (\code{numeric} column) containing the population-weighted mean exposure
#'  \item \code{population_total} (\code{integer} column) containing the total population in each geographic unit
#' }
#' @returns
#' 2) \code{detailed} (\code{list}) containing detailed (and interim) results.

#' @export

#' @author Arno Pauwels & Liliana Vazquez Fernandez

get_exposure_grid <-
  function(
    poll_grid,
    geo_units,
    pop_grid,
    geo_id_micro,
    bin_width
  ) {
    ## check for matching CRS
    if (terra::ext(pop_grid) != terra::ext(poll_grid)) {
      poll_grid <- terra::project(poll_grid, pop_grid, method = "near")
      warning("'poll_grid' was reprojected to match the extent and resolution of 'pop_grid'.")}
    if (sf::st_crs(geo_units) != sf::st_crs(poll_grid)) {
      geo_units <- sf::st_transform(geo_units, st_crs(poll_grid))
      warning("'geo_units' was reprojected to match the CRS of 'poll_grid' and 'pop_grid'.")}
    
    ## other checks !!!
    
    ## crop & mask pollution & population grid
    poll_grid <- terra::mask(terra::crop(poll_grid, terra::vect(geo_units)), terra::vect(geo_units))
    pop_grid <- terra::mask(terra::crop(pop_grid, terra::vect(geo_units)), terra::vect(geo_units))
    
    ## extract min and max value
    poll_min <- base::min(values(poll_grid), na.rm = TRUE)
    poll_max <- base::max(values(poll_grid), na.rm = TRUE)
    
    ## define bins
    decimals = base::round(-base::log10(bin_width))
    bin_min <- base::round(poll_min, decimals)
    bin_max <- base::round(poll_max, decimals)
    bins <- base::data.frame(
      bin = base::cut(
        x = base::seq(bin_min, bin_max-bin_width, by = bin_width), 
        breaks = base::seq(bin_min, bin_max, by = bin_width),
        right = FALSE
      ),
      mid = base::seq(bin_min, bin_max-bin_width, by = bin_width) + (bin_width/2)
    )
    
    ## bind pollution and population grids
    grid <- base::c(poll_grid, pop_grid)
    base::names(grid) <- base::c("poll", "pop")
    
    ## extract grid values by geographical unit
    geo_units$geo_id_micro <- geo_id_micro
    exp_vals <- exactextractr::exact_extract(
      grid, 
      geo_units,
      include_cols = "geo_id_micro",
      progress = FALSE
    )
    
    ## get population by exposure bin
    exp_bins <- base::lapply(exp_vals, function(df) {
      df$pop <- df$coverage_fraction*df$pop
      df$bin <- base::cut(df$poll, base::seq(bin_min, bin_max, by = bin_width), right = FALSE)
      geo_id_micro <- unique(df$geo_id_micro)
      df <- stats::aggregate(pop~bin, df, sum)
      df <- dplyr::left_join(bins, df, by = "bin")
      df$geo_id_micro <- geo_id_micro
      df[base::is.na(df$pop), "pop"] <- 0
      return(df)
    }) |> data.table::rbindlist()
    
    ## get population-weighted average
    exp_mean <- base::lapply(exp_vals, function(df) {
      df$pop <- df$coverage_fraction*df$pop
      mean <- stats::weighted.mean(df$poll, df$pop, na.rm = TRUE)
      pop <- base::round(base::sum(df$pop))
      df <- base::data.frame(
        geo_id_micro = base::unique(df$geo_id_micro),
        mean = mean,
        pop = pop
      )
      return(df)
    }) |> data.table::rbindlist()
    
    ## build output lists
    exposure_main <- base::list(
      geo_id_micro = exp_mean$geo_id_micro,
      exposure_mean = exp_mean$mean,
      population_total = exp_mean$pop
    )
    
    exposure_detailed <- base::list(
      geo_id_micro = exp_bins$geo_id_micro,
      exposure_bin = exp_bins$bin,
      exposure_mid = exp_bins$mid,
      population = exp_bins$pop
    )
    
    out <- base::list(
      exposure_main = exposure_main,
      exposure_detailed = exposure_detailed
    )
    
    return(out)
}
