#' Get exposure as a population-weighted average concentration based on population in sub-units

# DESCRIPTION ##################################################################
#' @description
#' This function calculates an average concentration value in each geographic unit,
#' where the mean concentration values in sub-units are weighted with the corresponding population numbers.

# ARGUMENTS ####################################################################
#' @param poll_grid \code{SpatRaster} of the pollution concentration data.
#' @param geo_units \code{sf} of the geographic sub-units.
#' @param population \code{Integer vector} containing the total population number in each geographic sub-unit.
#' @param geo_id_macro \code{Numeric or string vector} of the higher-level IDs of the geographic units the sub-unit belong to and will be aggregated at. Required if \code{population} is provided.
#' @param bin_width \code{Numeric} specifying the width of the population exposure bins.

# VALUE ########################################################################
#' @return
#' This function returns a \code{list} containing:
#' @returns
#' 1) \code{main} (\code{list}) containing the main results as vectors;
#' \itemize{
#'  \item \code{geo_id_macro} (\code{string} column) containing the higher-level geographic IDs of the assessment
#'  \item \code{exposure_mean} (\code{numeric} column) containing the population-weighted mean exposure
#'  \item \code{population_total} (\code{integer} column) containing the total population in each geographic unit
#' }
#' @returns
#' 2) \code{detailed} (\code{list}) containing detailed (and interim) results.

#' @export

#' @author Arno Pauwels & Liliana Vazquez Fernandez


get_exposure_unit <-
  function(
    poll_grid,
    geo_units,
    population,
    geo_id_macro,
    bin_width
  ) {
    ## check for matching CRS
    if (sf::st_crs(geo_units) != sf::st_crs(poll_grid)) {
      geo_units <- sf::st_transform(geo_units, st_crs(poll_grid))
      warning("'geo_units' was reprojected to match the CRS of 'poll_grid' and 'population'.")}
    
    ## crop & mask pollution grid
    poll_grid <- terra::mask(terra::crop(poll_grid, terra::vect(geo_units)), terra::vect(geo_units))
    
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
    
    ## extract pollution mean by geographical sub-unit
    exp_vals <- base::data.frame(
      geo_id_macro = geo_id_macro,
      pop = population,
      poll = exactextractr::exact_extract(
        poll_grid, 
        geo_units,
        fun = "mean",
        progress = FALSE
      )
    )
    
    ## get population by exposure bin
    exp_bins <- base::by(
      exp_vals,
      base::list(exp_vals$geo_id_macro),
      function(df) {
        # df$pop <- df$coverage_fraction*df$pop
        df$bin <- base::cut(df$poll, base::seq(bin_min, bin_max, by = bin_width), right = FALSE)
        geo_id_macro <- unique(df$geo_id_macro)
        df <- stats::aggregate(pop~bin, df, sum)
        df <- dplyr::left_join(bins, df, by = "bin")
        df$geo_id_macro <- geo_id_macro
        df[base::is.na(df$pop), "pop"] <- 0
        return(df)
      },
      simplify = FALSE
    ) |> data.table::rbindlist()
    
    ## get population-weighted average
    exp_mean <- exp_vals |>
      dplyr::group_by(geo_id_macro) |>
      dplyr::summarise(
        mean = stats::weighted.mean(poll, pop),
        pop = base::sum(pop)
      )
    
    ## build output lists
    exposure_main <- base::list(
      geo_id_macro = exp_mean$geo_id_macro,
      exposure_mean = exp_mean$mean,
      population_total = exp_mean$pop
    )
    
    exposure_detailed <- base::list(
      geo_id_macro = exp_bins$geo_id_macro,
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
