# Test data

Test-only inputs and expected results for the testthat suite.

## How to read one

Always with the namespaced reader plus `testthat::test_path()`, two arguments,
so that every call site looks the same:

```r
data   <- base::readRDS(testthat::test_path("testdata", "airqplus_pm_copd.rds"))
expect <- utils::read.csv(testthat::test_path("testdata", "expected_exposure_pwm.csv"))
pop    <- terra::rast(testthat::test_path("testdata", "population.tif"))
```

## Conventions

* Names are lowercase `snake_case`, prefixed by topic
  (`noise_`, `ozone_`, `erf_`, `pop_`, `lifetable_`, `expected_`).
* Inputs are `.rds` (xz-compressed); expected results stay `.csv` so that
  changes to reference values are visible in a diff.
* Plain `data.frame` / `tibble` only, so that the test suite needs no package
  beyond the ones in DESCRIPTION.
* No `exdat_` prefix here: that prefix is reserved for the package datasets
  in `data/`.

## Do not duplicate package data

Data that is part of the package interface is used directly, never copied
into this directory:

| Data                      | Where it lives  | How to use it in a test                                                        |
| ------------------------- | --------------- | ------------------------------------------------------------------------------ |
| `exdat_*` (tibbles, `sf`) | `data/`         | `exdat_pwm_2` (lazy-loaded, no prefix needed)                                    |
| `exdat_pwm_1` (GeoTIFF)   | `inst/extdata/` | `terra::rast(system.file("extdata", "exdat_pwm_1.tif", package = "healthiar"))` |

The raw sources and the scripts that build both are in `data-raw/`, which is
excluded from the built package via `.Rbuildignore`.
