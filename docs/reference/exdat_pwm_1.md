# Air pollution expsoure data of the Brussels-Capital region (Belgium)

The data can be loaded using

`exdat_pwm_1 <- terra::rast(`

`system.file("extdata", "exdat_pwm_1.tif", package = "healthiar")`

`)`

(see Examples section below).

When loaded it is a variable of class `SpatRaster`, which contains air
pollution exposure levels of municipalities in the Brussels-Capital
region (Belgium).

Because it is a `.tif` file it is stored in the package's `inst/extdata`
directory.

## Format

GeoTIFF raster

## Source

Real-world data

## Author

Arno Pauwels

## Examples

``` r
path <- system.file("extdata", "exdat_pwm_1.tif", package = "healthiar")
exdat_pwm_1 <- terra::rast(path)
```
