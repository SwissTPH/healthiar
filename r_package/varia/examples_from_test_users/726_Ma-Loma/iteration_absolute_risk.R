# Ma-Loma^s code ###############################################################

library(healthiar)
library(tidyverse) #Create a reproducible exposure dataframe

exdat_noise_long <- exdat_noise_ha %>% select(-erf_percent,-number,-yld) %>%  pivot_longer( cols = starts_with("population_exposed_"), names_to = "region", values_to = "exposed" ) %>% mutate(region = str_split_i(region, "_", 3)) %>% mutate(regionID = region %>% as.factor() %>% as.numeric())

HA1 <- exdat_noise_long%>% { attribute_health( approach_risk = "absolute_risk", exp_central = .$exposure_mean, pop_exp = .$exposed, erf_eq_central = "78.9270-3.1162*c+0.0342*c^2" ) }

HA2 <- exdat_noise_long %>%
  { attribute_health(
    geo_id_disaggregated = .$regionID,
    approach_risk = "absolute_risk",
    exp_central = .$exposure_mean,
    pop_exp = .$exposed,
    erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
  )
  }

HA3 <- exdat_noise_long %>% { attribute_health( geo_id_disaggregated = .$regionID, approach_risk = "absolute_risk", exp_central = .$exposure_mean %>% as.list, pop_exp = .$exposed %>% as.list, erf_eq_central = "78.9270-3.1162*c+0.0342*c^2" ) }

# AL ###########################################################################
## Finding reason for error in HA2

View(exdat_noise_long)

length(exdat_noise_long$regionID)
length(exdat_noise_long$exposure_mean |> as.list())
length(exdat_noise_long$exposure_mean |> as.list())
length(exdat_noise_long$exposed |> as.list())
## Length is 15, but should be only 3 because there are only 3 geo units (urban, rural, total)

HA2 <-
  attribute_health(
    geo_id_disaggregated = c("rural", "urban", "total"),
    approach_risk = "absolute_risk",
    exp_central = rep(list(exdat_noise_ha$exposure_mean), 3), # List of 3 elements; each element is a vector containing the 5 exposure means
    pop_exp = list(
      exdat_noise_ha$population_exposed_rural, # Rural population exposed
      exdat_noise_ha$population_exposed_urban, # Urban population exposed
      exdat_noise_ha$population_exposed_total # Total population exposed
      ),
    erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
  )

## Check that results of HA1 & HA2 match
HA1$health_main$impact_rounded
HA2$health_main$impact_rounded
HA1$health_main$impact_rounded == HA2$health_main$impact_rounded

## Detailed results (per geo ID) for HA2
View(HA2[["health_detailed"]][["results_raw"]])

HA3 <- attribute_health(
  # geo_id_disaggregated = exdat_noise_long$regionID,
  # geo_id_disaggregated = c("c","a","b",
  #                          "c","a","b",
  #                          "c","a","b",
  #                          "c","a","b",
  #                          "c","a","b"),
  geo_id_disaggregated = rep(c("c","a","b"), times = 5),
  approach_risk = "absolute_risk",
  exp_central = exdat_noise_long$exposure_mean,
  pop_exp = exdat_noise_long$exposed,
  erf_eq_central = "78.9270-3.1162*c+0.0342*c^2"
)

HA3 <- exdat_noise_long %>% { attribute_health( geo_id_disaggregated = .$regionID, approach_risk = "absolute_risk", exp_central = .$exposure_mean %>% as.list, pop_exp = .$exposed %>% as.list, erf_eq_central = "78.9270-3.1162*c+0.0342*c^2" ) }

