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
#'exdat_pwm_1 <- terra::rast(system.file("extdata", "exdat_pwm_1.tif", package = "healthiar"))
#'exdat_pwm_2 <- sf::st_read(system.file("extdata", "exdat_pwm_2.gpkg", package = "healthiar"), quiet = TRUE)
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

    ## calculate exposure as a simple average concentration
    if (base::is.null(population) & base::is.null(pop_grid)) {

      ## check for matching CRS
      if (sf::st_crs(geo_units) != sf::st_crs(poll_grid)) {
        geo_units <- sf::st_transform(geo_units, sf::st_crs(poll_grid))
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

    ## calculate exposure as a population-weighted average concentration based on gridded population
    if (!base::is.null(pop_grid)) {

      ## check for matching CRS
      if (terra::ext(pop_grid) != terra::ext(poll_grid)) {
        poll_grid <- terra::project(poll_grid, pop_grid, method = "near")
        warning("'poll_grid' was reprojected to match the extent and resolution of 'pop_grid'.")}
      if (sf::st_crs(geo_units) != sf::st_crs(poll_grid)) {
        geo_units <- sf::st_transform(geo_units, st_crs(poll_grid))
        warning("'geo_units' was reprojected to match the CRS of 'poll_grid' and 'pop_grid'.")}

      ## crop & mask pollution & population grid
      poll_grid <- terra::mask(terra::crop(poll_grid, terra::vect(geo_units)), terra::vect(geo_units))
      pop_grid <- terra::mask(terra::crop(pop_grid, terra::vect(geo_units)), terra::vect(geo_units))

      ## extract min and max value
      poll_min <- base::min(terra::values(poll_grid), na.rm = TRUE)
      poll_max <- base::max(terra::values(poll_grid), na.rm = TRUE)

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

    ## calculate exposure as a population-weighted average concentration based on population in sub-units
    if (!base::is.null(population)) {

      ## check for matching CRS
      if (sf::st_crs(geo_units) != sf::st_crs(poll_grid)) {
        geo_units <- sf::st_transform(geo_units, st_crs(poll_grid))
        warning("'geo_units' was reprojected to match the CRS of 'poll_grid' and 'population'.")}

      ## crop & mask pollution grid
      poll_grid <- terra::mask(terra::crop(poll_grid, terra::vect(geo_units)), terra::vect(geo_units))

      ## extract min and max value
      poll_min <- base::min(terra::values(poll_grid), na.rm = TRUE)
      poll_max <- base::max(terra::values(poll_grid), na.rm = TRUE)

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
}
