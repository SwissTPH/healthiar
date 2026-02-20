# exdat_pwm_2 ##################################################################

## NOTE AL 2026-02-20: the original data set was called
## municipalities_brussels.gpkg; renamed to
## exdat_pwm2_municipalities_brussels.gpkg

exdat_pwm_2 <- sf::st_read(
  "data-raw/exdat_pwm_2_municipalities_brussels.gpkg",
  quiet = TRUE
  )

# save data ####################################################################
usethis::use_data(exdat_pwm_2, overwrite = TRUE)
