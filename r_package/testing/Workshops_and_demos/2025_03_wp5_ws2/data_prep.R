## setup
setwd("C:/Users/luytax/switchdrive/BEST-COST/best-cost_WPs/r_package/testing/Workshops_and_demos/WS_WP5/ws_2/")
library(tidyr)
library(dplyr)


## PM country level data
data_pm_norway <- read.csv("data_ws2/exposure_population_pm25_norway.csv")
data_pm_norway <- data_pm_norway %>%
  separate(Bin, into = c("exp_lower", "exp_higher"), sep = "-", convert = TRUE) %>%
  mutate(exp_mean = (exp_lower + exp_higher) / 2) |>
  drop_na() |>
  mutate(pop_prop = Population_all / sum(Population_all)) |>
  mutate(product = pop_prop * exp_mean) |>
  mutate(pwm = sum(product)) |>
  select(exp = exp_mean, population = Population_all)

## PM Oslo
data_pm_oslo <- read.csv("data_ws2/exposure_population_pm25oslo.csv")
data_pm_oslo <- data_pm_oslo %>%
  separate(Bin, into = c("exp_lower", "exp_higher"), sep = "-", convert = TRUE) %>%
  mutate(exp_mean = (exp_lower + exp_higher) / 2) |>
  drop_na() |>
  mutate(pop_prop = Population_all / sum(Population_all)) |>
  mutate(product = pop_prop * exp_mean) |>
  mutate(pwm = sum(product)) |>
  select(exp = exp_mean, population = Population_all)

## PM MR-BRT point-pairs
data_pm_mr_brt <- read.csv("data_ws2/resp_copd_mr_brt.csv")

## Noise Oslo data
data_noise <- tibble(
  exp_type = rep("Lden (dB)", 5),
  exp_cat = c(57.5, 62.5, 67.5, 72.5, 77.5),
  pop_asker = c(10593, 4312, 823, 161, 18),
  pop_baerum = c(15041, 9425, 4003, 813, 288),
  pop_oslo = c(128356, 65603, 37762, 10250, 525),
  pop_lorenskog = c(6364, 3101, 1247, 313, 5),
  pop_lillestrom = c(9932, 4679, 991, 63, 0),
  pop_raelingen = c(2163, 1072, 554, 61, 0),
  pop_nordre_follo = c(4379, 1733, 164, 5, 0)
  )

## Save data
save.image(file = "data_clean.RData")



