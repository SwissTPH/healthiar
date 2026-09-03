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

## Provenance of the AirQ+ data

Two data sets hold an AirQ+ life table assessment of the mortality attributable
to PM2.5 in Switzerland in 2019, each with the input data as entered in AirQ+
(`input`, `pop`) and the results of AirQ+ (`output`), so that both tools can be
compared on exactly the same data:

| Data set                          | Exposure         | AirQ+ export in `data-raw/`                              |
| --------------------------------- | ---------------- | -------------------------------------------------------- |
| `airqplus_pm_deaths_yll.rds`      | constant         | `exdat_lifetable_airqplus_deaths_yll_lifetable_adults.csv` |
| `airqplus_pm_yll_single_year.rds` | one single year  | `airqplus_pm_yll_single_year.csv`                          |

Both were parsed from those exports, which are kept as the reference.
`airqplus_pm_yll_single_year.rds` is built by
`data-raw/prepare_airqplus_export.R`, whose function reads either export. The
0 deaths of the females aged 8 were replaced by 1, because a survival
probability of 100% has no conceptual logic and `healthiar` warns about it;
this does not change the results of AirQ+ at the two decimals that it exports.

Compare with the male and the female results of AirQ+ and never with its
"All genders" ones: the latter are not the sum of the former but a life table
with both sexes pooled. See the chapter "YLL & deaths with life table" of the
vignette.
