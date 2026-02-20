# exdat_prepare_mdi ############################################################

## NOTE AL 2026-02-20: the original data set was called
## Belgium_MDI_2021.csv; renamed to
## exdat_prepare_mdi_Belgium_MDI_2021.csv

exdat_prepare_mdi <- read.csv("data-raw/exdat_prepare_mdi_Belgium_MDI_2021.csv")

# save data ####################################################################
usethis::use_data(exdat_prepare_mdi, overwrite = TRUE)
