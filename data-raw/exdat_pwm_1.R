# exdat_pwm_1 ##################################################################

## NOTE AL 2026-02-20: because this dataset (.tif) can't be saved as .rda
## without losing some info, it is saved in inst/extdata instead of data/

## NOTE AL 2026-02-20: the original data set was called
## pm25.tif; renamed to exdat_pwm_1_pm25.tif

# save data ####################################################################
base::dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
base::file.copy(
  from = "data-raw/exdat_pwm_1_pm25.tif",
  to = "inst/extdata/exdat_pwm_1.tif",
  overwrite = TRUE
)
