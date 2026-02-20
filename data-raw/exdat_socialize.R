# exdat_socialize ############################################################

## NOTE AL 2026-02-20: the original data set was called
## mort_pm25_sector_2019.csv; renamed to
## exdat_socialize_mort_pm25_sector_2019.csv
social_mort_rawdata <-
  ## import burden
  read.csv("data-raw/exdat_socialize_mort_pm25_sector_2019.csv")


## import deprivation index
## NOTE AL 2026-02-20: the original data set was called
## BIMD_2011_WITHOUT_HEALTH_ELLIS_WIDE.csv; renamed to
## exdat_socialize_BIMD_2011_WITHOUT_HEALTH_ELLIS_WIDE.csv
social_bimd_rawdata <-
  read.csv("data-raw/exdat_socialize_BIMD_2011_WITHOUT_HEALTH_ELLIS_WIDE.csv")

## subset Flanders (NUTS1 = BE2)
social_mort_data <-
  dplyr::filter(
    social_mort_rawdata,
    NUTS1 == 'BE2' & POPULATION > 0)

# Join input data tables
social_data <-
  dplyr::left_join(
    x = social_mort_data,
    y= social_bimd_rawdata,
    by = c("CS01012020" = "CD_RES_SECTOR"))

# Data set to be used as input in the R package
social_data_input <-
  social_data |>
  # Remove the columns rank and deciles (to be done by the R package)
  dplyr::select(-rank, -deciles)

## exposure and attributable burden per deprivation decile
social_per_decile <-
  data.frame(
    exposure_mean = with(social_data, tapply(PM25_MEAN, deciles, mean)),
    mort_attr = 1e5 * with(social_data, tapply(MORTALITY_ATTR, deciles, sum))/
      with(social_data, tapply(POPULATION, deciles, sum)),
    mort_total = 1e5 * with(social_data, tapply(MORTALITY_TOTAL, deciles, sum))/
      with(social_data, tapply(POPULATION, deciles, sum)))


## inequalities

social_results <-
  data.frame(
    ## absolute diff
    exp_abs_diff =
      social_per_decile[["exposure_mean"]][1] -
      social_per_decile[["exposure_mean"]][10],
    ## relative diff
    exp_rel_diff = 100 *
      (
        (
          social_per_decile[["exposure_mean"]][1] /
           social_per_decile[["exposure_mean"]][10]
          ) - 1
        ),
    ## PAF
    exp_paf_di = 100 *
      (
        mean(social_data$PM25_MEAN) - social_per_decile[["exposure_mean"]][10]
        ) /
      mean(social_data$PM25_MEAN),
    ## absolute diff
    mort_abs_diff = social_per_decile[["mort_attr"]][1] -
      social_per_decile[["mort_attr"]][10],
    ## relative diff
    mort_rel_diff = 100 *
      (
        (
        social_per_decile[["mort_attr"]][1] /
          social_per_decile[["mort_attr"]][10]
        ) - 1
        ),
    mort_mean = (
      1e5 *
        sum(social_data$MORTALITY_ATTR) / sum(social_data$POPULATION)
      )
    ) |>
  ## PAF
  dplyr::mutate(
    mort_paf_di = 100 *
      (mort_mean - social_per_decile[["mort_attr"]][10]) /
      mort_mean
  )

exdat_socialize <- dplyr::bind_rows(
  # Add rows for different age groups
  social_data_input |> dplyr::mutate(age_group = "below_40"),
  social_data_input |> dplyr::mutate(
    age_group = "40_plus",
    # Fake numbers reducing the total ones in 40_plus
    MORTALITY_TOTAL = MORTALITY_TOTAL - 10,
    POPULATION = POPULATION - 20,
    PM25_MEAN = PM25_MEAN- 0.1,
  )) |>
  # Be sure that there is no negative numbers
  dplyr::mutate(
    dplyr::across(c(MORTALITY_TOTAL, POPULATION, PM25_MEAN),
                  ~ base::pmax(.x, 0))) |>
  # Keep only relevant columns
  dplyr::select(
    NUTS1,
    CS01012020,
    age_group,
    PM25_MEAN,
    RR,
    MORTALITY_TOTAL,
    POPULATION,
    score
    ) |>
  dplyr::rename(geo_unit = CS01012020,
                pm25_mean = PM25_MEAN,
                rr = RR,
                mortality = MORTALITY_TOTAL,
                population = POPULATION) |>
  # Create ref_prop_pop
  dplyr::mutate(ref_prop_pop = 0.5) |>
  # The data set contains the RR for the exposure but not per increment.
  # Calculable as e.g. exp(log(1.038017)/(4.848199)*10)
  # Replace the value of rr (originally corresponds to rr_at_exp)
  dplyr::mutate(rr = 1.08)

# save data ####################################################################
# usethis::use_data(exdat_socialize, overwrite = TRUE)
