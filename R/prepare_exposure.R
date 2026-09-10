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
#' path <- system.file("extdata", "exdat_pwm_1.tif", package = "healthiar")
#' exdat_pwm_1 <- terra::rast(path)
#'
#' pwm <- prepare_exposure(
#'   poll_grid = exdat_pwm_1, # Formal class SpatRaster
#'   geo_units = exdat_pwm_2, # sf of the geographic sub-units
#'   population = sf::st_drop_geometry(exdat_pwm_2$population), # population per geographic sub-unit
#'   geo_id_macro = sf::st_drop_geometry(exdat_pwm_2$region) # higher-level IDs to aggregate at
#' )
#'
#' pwm$exposure_main # population-weighted mean exposures for the (higher-level) geographic units

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

    ## Create helper function that defines the exposure bins.
    ## It is used in both pathways below (gridded and tabular population) so
    ## that the bins cannot drift apart between them.
    ## floor() and ceiling() and not round(): rounding could place the lowest
    ## break above the minimum and the highest break below the maximum of the
    ## pollutant, and the cells outside the breaks were then assigned to no bin
    ## at all (bin NA). Depending on the pathway they either ended up in the
    ## results with an exposure of NA or were dropped together with their
    ## population.
    ## The breaks are returned as well, so that the same ones are used for the
    ## master table of bins and for the binning of the values
    get_exposure_bins <- function(poll_min, poll_max, bin_width) {

      bin_min <- floor(poll_min / bin_width) * bin_width
      bin_max <- ceiling(poll_max / bin_width) * bin_width

      ## cut(right = FALSE) uses the intervals [a, b), so the maximum has to
      ## lie strictly below the last break. This also guarantees at least one
      ## bin when all cells have the same value
      if (bin_max <= poll_max) {
        bin_max <- bin_max + bin_width
      }

      breaks <- seq(bin_min, bin_max, by = bin_width)
      ## The lower edge of each bin, i.e. all breaks but the last one
      lower_edges <- breaks[-length(breaks)]

      list(
        breaks = breaks,
        bins = data.frame(
          bin = cut(lower_edges, breaks = breaks, right = FALSE),
          mid = lower_edges + (bin_width / 2)
        )
      )
    }

    ## Three mutually exclusive pathways follow, chosen by the population data
    ## that the user entered: none (simple average), a population grid, or a
    ## population per sub-unit. Each one returns its own output, so the later
    ## pathways are only reached if the earlier conditions did not apply.

    ## calculate exposure as a simple average concentration
    if (is.null(population) & is.null(pop_grid)) {

      ## check for matching CRS
      if (sf::st_crs(geo_units) != sf::st_crs(poll_grid)) {
        geo_units <- sf::st_transform(geo_units, sf::st_crs(poll_grid))
        warning("'geo_units' was reprojected to match the CRS of 'poll_grid'.")}

      ## crop & mask pollution grid
      poll_grid <- terra::mask(terra::crop(poll_grid, terra::vect(geo_units)), terra::vect(geo_units))

      ## rename pollution grid
      names(poll_grid) <- "poll"

      ## calculate mean concentration value and other stats by geographical unit
      exp_mean <- data.frame(
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
      exposure_main <- list(
        geo_id_micro = exp_mean$geo_id_micro,
        exposure_mean = exp_mean$mean
      )

      exposure_detailed <- list(
        geo_id_micro = exp_mean$geo_id_micro,
        exposure_mean = exp_mean$mean,
        exposure_median = exp_mean$median,
        exposure_min = exp_mean$min,
        exposure_lower = exp_mean$lower,
        exposure_upper = exp_mean$upper,
        exposure_max = exp_mean$max,
        exposure_stdev = exp_mean$stdev
      )

      out <- list(
        exposure_main = exposure_main,
        exposure_detailed = exposure_detailed
      )

      return(out)
    }

    ## calculate exposure as a population-weighted average concentration based on gridded population
    if (!is.null(pop_grid)) {

      ## check for matching CRS
      ## method = "near", i.e. nearest neighbour, so that the reprojected grid
      ## keeps the pollutant values that were measured instead of interpolating
      ## new ones between them
      if (terra::ext(pop_grid) != terra::ext(poll_grid)) {
        poll_grid <- terra::project(poll_grid, pop_grid, method = "near")
        warning("'poll_grid' was reprojected to match the extent and resolution of 'pop_grid'.")}
      if (sf::st_crs(geo_units) != sf::st_crs(poll_grid)) {
        geo_units <- sf::st_transform(geo_units, sf::st_crs(poll_grid))
        warning("'geo_units' was reprojected to match the CRS of 'poll_grid' and 'pop_grid'.")}

      ## crop & mask pollution & population grid
      poll_grid <- terra::mask(terra::crop(poll_grid, terra::vect(geo_units)), terra::vect(geo_units))
      pop_grid <- terra::mask(terra::crop(pop_grid, terra::vect(geo_units)), terra::vect(geo_units))

      ## extract min and max value
      poll_min <- min(terra::values(poll_grid), na.rm = TRUE)
      poll_max <- max(terra::values(poll_grid), na.rm = TRUE)

      ## define bins
      exposure_bins <- get_exposure_bins(poll_min, poll_max, bin_width)
      bin_breaks <- exposure_bins$breaks
      bins <- exposure_bins$bins

      ## bind pollution and population grids
      grid <- c(poll_grid, pop_grid)
      names(grid) <- c("poll", "pop")

      ## extract grid values by geographical unit
      geo_units$geo_id_micro <- geo_id_micro
      exp_vals <- exactextractr::exact_extract(
        grid,
        geo_units,
        include_cols = "geo_id_micro",
        progress = FALSE
      )

      ## get population by exposure bin
      exp_bins <- purrr::map_dfr(exp_vals, function(df) {
        df |>
          # 1. Calculate weighted population
          # Cells on the border lie only partly inside the geographic unit.
          # coverage_fraction is that share, so it scales the population of
          # the cell down to the part that belongs to the unit
          dplyr::mutate(pop = coverage_fraction * pop) |>
          # 2. Create bins for pollutant levels
          # right = FALSE, i.e. bins are closed on the left, so that a value
          # falling exactly on a break belongs to the bin above it,
          # as in the master table of bins created by get_exposure_bins()
          dplyr::mutate(bin = cut(
            poll,
            bin_breaks,
            right = FALSE
          )) |>
          # 3. Aggregate population by bin
          dplyr::group_by(bin) |>
          dplyr::summarise(
            pop = sum(pop, na.rm = TRUE),
            .groups = "drop"
          ) |>
          # 4. Join with master 'bins' table to ensure all bins are represented.
          # right_join() and not left_join(): the left hand side is the table
          # already summarised for this geo unit, so a left join could only add
          # columns and never the bins without population. The bins missing
          # there stayed absent instead of being filled with 0 below
          dplyr::right_join(bins, by = "bin") |>
          # 5. Add back the geo_id and fill empty bins with 0
          dplyr::mutate(
            geo_id_micro = unique(df$geo_id_micro),
            pop = dplyr::coalesce(pop, 0)
          )
      })


      ## get population-weighted average
      exp_mean <- purrr::map_dfr(exp_vals, function(df) {
        df |>
          # 1. Update population by coverage fraction
          dplyr::mutate(pop = coverage_fraction * pop) |>
          # 2. Calculate weighted mean and total population
          dplyr::summarise(
            geo_id_micro = unique(geo_id_micro),
            mean = stats::weighted.mean(poll, pop, na.rm = TRUE),
            pop = round(sum(pop, na.rm = TRUE)),
            .groups = "drop"
          )
      })

      ## build output lists
      exposure_main <- list(
        geo_id_micro = exp_mean$geo_id_micro,
        exposure_mean = exp_mean$mean,
        population_total = exp_mean$pop
      )

      exposure_detailed <- list(
        geo_id_micro = exp_bins$geo_id_micro,
        exposure_bin = exp_bins$bin,
        exposure_mid = exp_bins$mid,
        population = exp_bins$pop
      )

      out <- list(
        exposure_main = exposure_main,
        exposure_detailed = exposure_detailed
      )

      return(out)
    }

    ## calculate exposure as a population-weighted average concentration based on population in sub-units
    if (!is.null(population)) {

      ## check for matching CRS
      if (sf::st_crs(geo_units) != sf::st_crs(poll_grid)) {
        geo_units <- sf::st_transform(geo_units, sf::st_crs(poll_grid))
        warning("'geo_units' was reprojected to match the CRS of 'poll_grid' and 'population'.")}

      ## crop & mask pollution grid
      poll_grid <- terra::mask(terra::crop(poll_grid, terra::vect(geo_units)), terra::vect(geo_units))

      ## extract min and max value
      poll_min <- min(terra::values(poll_grid), na.rm = TRUE)
      poll_max <- max(terra::values(poll_grid), na.rm = TRUE)

      ## define bins
      exposure_bins <- get_exposure_bins(poll_min, poll_max, bin_width)
      bin_breaks <- exposure_bins$breaks
      bins <- exposure_bins$bins

      ## extract pollution mean by geographical sub-unit
      exp_vals <- data.frame(
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
      exp_bins <- exp_vals |>
        # 1. Create bins for the whole dataset at once (Fastest)
        dplyr::mutate(
          bin = cut(
            poll,
            bin_breaks,
            right = FALSE
          )
        ) |>
        # 2. Aggregate by ID and Bin
        dplyr::group_by(geo_id_macro, bin) |>
        dplyr::summarise(
          pop = sum(pop, na.rm = TRUE),
          .groups = "drop"
        ) |>
        # 3. Join with a grid of ALL IDs and ALL Bins (from your master 'bins' table)
        # This ensures 'mid' and any other bin metadata are included
        dplyr::right_join(
          tidyr::expand_grid(
            geo_id_macro = unique(exp_vals$geo_id_macro),
            bin = bins$bin
          ) |>
            dplyr::left_join(bins, by = "bin"), # This brings 'mid' back in
          by = c("geo_id_macro", "bin")
        ) |>
        # 4. Cleanup NAs
        dplyr::mutate(pop = dplyr::coalesce(pop, 0))

      ## get population-weighted average
      exp_mean <- exp_vals |>
        dplyr::group_by(geo_id_macro) |>
        dplyr::summarise(
          # na.rm = TRUE as in the gridded pathway above: without it a single
          # sub-unit without raster coverage turned the mean of the whole
          # geographical unit into NA
          mean = stats::weighted.mean(poll, pop, na.rm = TRUE),
          pop = sum(pop)
        )

      ## build output lists
      exposure_main <- list(
        geo_id_macro = exp_mean$geo_id_macro,
        exposure_mean = exp_mean$mean,
        population_total = exp_mean$pop
      )

      exposure_detailed <- list(
        geo_id_macro = exp_bins$geo_id_macro,
        exposure_bin = exp_bins$bin,
        exposure_mid = exp_bins$mid,
        population = exp_bins$pop
      )

      out <- list(
        exposure_main = exposure_main,
        exposure_detailed = exposure_detailed
      )

      return(out)
    }
}
