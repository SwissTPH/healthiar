# Prepare exposure data

This function prepares tabular population exposure data compatible with
the `attribute()` and
[`compare()`](https://swisstph.github.io/healthiar/reference/compare.md)
functions, based on gridded pollution concentration data and polygon
data representing geographic units. If population data is provided, the
function calculates an average concentration value in each geographic
unit that is weighted with the population number at each location. If no
population data is provided, the function calculates the simple spatial
average concentration in each geographic unit.

## Usage

``` r
prepare_exposure(
  poll_grid,
  geo_units,
  population = NULL,
  pop_grid = NULL,
  geo_id_micro = NULL,
  geo_id_macro = NULL,
  bin_width = 0.1
)
```

## Arguments

- poll_grid:

  `SpatRaster` of the pollution concentration data.

- geo_units:

  `sf` of the geographic units or sub-units.

- population:

  `Integer vector` of the total population number in each geographic
  sub-unit.

- pop_grid:

  `SpatRaster` of the gridded population data.

- geo_id_micro:

  `Numeric or string vector` of the IDs of the geographic units.
  Required if `pop_grid` is given or if no population data is provided.

- geo_id_macro:

  `Numeric or string vector` of the higher-level IDs of the geographic
  units the sub-unit belong to and will be aggregated at. Required if
  `population` is provided.

- bin_width:

  `Numeric` specifying the width of the population exposure bins.

## Value

This function returns a `list` containing:

1\) `main` (`list`) containing the main results as vectors;

- `geo_id_micro` of `geo_id_macro` (`string` column) containing the
  (higher-level) geographic IDs of the assessment

- `exposure_mean` (`numeric` column) containing the
  (population-weighted) mean exposure

- `population_total` (`integer` column) containing the total population
  in each geographic unit, if population data was provided

2\) `detailed` (`list`) containing detailed (and interim) results.

## Author

Arno Pauwels & Liliana Vazquez Fernandez

## Examples

``` r
# Goal: determine population-weighted mean PM2.5 exposure for several
# neighborhoods of Brussels (Belgium)

exdat_pwm_1 <- terra::rast(system.file("extdata", "exdat_pwm_1.tif", package = "healthiar"))
exdat_pwm_2 <- sf::st_read(system.file("extdata", "exdat_pwm_2.gpkg", package = "healthiar"),
                          quiet = TRUE)

pwm <- prepare_exposure(
  poll_grid = exdat_pwm_1, # Formal class SpatRaster
  geo_units = exdat_pwm_2, # sf of the geographic sub-units
  population = sf::st_drop_geometry(exdat_pwm_2$population), # population per geographic sub-unit
  geo_id_macro = sf::st_drop_geometry(exdat_pwm_2$region) # higher-level IDs to aggregate at
)

pwm$main # population-weighted mean exposures for the (higher-level) geographic units
#> $geo_id_macro
#> [1] "Center" "East"   "North"  "South"  "West"  
#> 
#> $exp_value
#> [1] 11.48074 11.06808 11.47630 11.05403 11.37794
#> 
#> $exp_type
#> [1] "Population-weighted mean concentration"
#> [2] "Population-weighted mean concentration"
#> [3] "Population-weighted mean concentration"
#> [4] "Population-weighted mean concentration"
#> [5] "Population-weighted mean concentration"
#> 
```
