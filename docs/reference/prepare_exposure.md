# Prepare exposure data

This function prepares tabular population exposure data compatible with
the `attribute()` and
[`compare()`](https://swisstph.github.io/healthiar/reference/compare.md)
functions, based on gridded pollution concentration data and vector data
representing geographic units. The function calculates an average
concentration value in each geographic unit, weighted by the fraction of
the population in each sub-unit.

## Usage

``` r
prepare_exposure(poll_grid, geo_units, population, geo_id_macro)
```

## Arguments

- poll_grid:

  `SpatRaster` of the pollution concentration data.

- geo_units:

  `sf` of the geographic sub-units.

- population:

  `Numeric vector` containing the total population number in each
  geographic sub-unit.

- geo_id_macro:

  `Numeric or string vector` containing the higher-level IDs of the
  geographic units the sub-unit belong to and will be aggregated at.

## Value

This function returns a `list` containing:

1\) `main` (`tibble`) containing the main results as vectors;

- `geo_id_macro` (`string` column) containing the (higher-level)
  geographic IDs of the assessment

- `exp_value` (`numeric` column) containing the (population-weighted)
  mean exposure

- `exp_type` (`string` column) specifying the exposure type

2\) `detailed` (`list`) containing detailed (and interim) results.

## Author

Arno Pauwels

## Examples

``` r
# Goal: determine population-weighted mean PM2.5 exposure for several
# neighborhoods of Brussels (Belgium)

exdat_pwm_1 <- terra::rast(system.file("extdata", "exdat_pwm_1.tif", package = "healthiar"))
exdat_pwm_2 <- sf::st_read(
    system.file("extdata", "exdat_pwm_2.gpkg", package = "healthiar"),
    quiet = TRUE
)

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
