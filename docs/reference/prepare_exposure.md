# Prepare exposure data

This function prepares tabular population exposure data that can be
entered in the argument `exp_...` of the `healthiar` functions, e.g.
[`attribute_health()`](https://swisstph.github.io/healthiar/reference/attribute_health.md)
using gridded pollution concentration and population data.

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

## Details

**Methodology**

The population-weighted exposure is calculated by intersecting gridded
concentration values with population grids, following the methodology
described in Shaddick et al. (2018) .

Detailed information about the methodology (including equations) is
available in the package vignette. More specifically, see chapters:

- [Preparation of exposure
  data](https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#preparation-of-exposure-data)

## References

Shaddick G, Thomas J, Jobling A, Brauer M, Van Donkelaar A, Martin R,
Burnett R, Casadei B, others (2018). “Data integration for the
assessment of population exposure to ambient air pollution.” *Journal of
the Royal Statistical Society Series C: Applied Statistics*, **67**(1),
231–248. [doi:10.1111/rssc.12227](https://doi.org/10.1111/rssc.12227) .

## Author

Arno Pauwels, Axel Luyten and Alberto Castro

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
  geo_id_macro = sf::st_drop_geometry(exdat_pwm_2$region) # higher-level IDs to aggregate
)
```
