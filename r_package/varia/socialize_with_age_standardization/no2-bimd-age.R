## AGE-STD AIR POLLUTION BURDEN INEQUALITIES

## required packages
library(dplyr)
library(tidyr)

## read data
data_raw <- readRDS("tests/testthat/data/no2_bimd_age.rds")

# Calculation of deciles
deciles <- unique(data_raw[, c("SECTOR", "SCORE")])
deciles$RANK <- as.numeric(rank(-deciles$SCORE, na.last = "keep", ties.method = "random"))
deciles$decile <- cut(
  deciles$RANK,
  breaks = quantile(deciles$RANK, probs = seq(0, 1, by = 0.1)),
  labels = FALSE, include.lowest = TRUE
)

# The deciles were already included in data
## aggregate impact & population by deprivation decile
data_by_age <- rbind(
  aggregate(cbind(POP,IMPACT)~AGE+DECILE+REF, data_raw, sum),
  cbind(
    aggregate(cbind(POP,IMPACT)~AGE+REF, data_raw, sum),
    DECILE = 'ALL'
  )
)


## calculate age-std rates
data_by_age$RATE <- 1e5*(data_by_age$IMPACT/data_by_age$POP)
data_by_age$RATE[is.nan(data_by_age$RATE)] <- 0
data_by_age$RATE <- data_by_age$REF*data_by_age$RATE
data_by_decile <- aggregate(RATE~DECILE, data_by_age, sum)

## calculate inequalities
abs <- tidyr::pivot_wider(data_by_decile, names_from = "DECILE", values_from = "RATE")
abs$OVERALL <- abs$ALL - abs$D10
abs$BOTTOM_QUANT <- abs$D1 - abs$D10
rel <- abs
rel$OVERALL <- rel$OVERALL/rel$ALL
rel$BOTTOM_QUANT <- rel$BOTTOM_QUANT/rel$D10
res <- rbind(
  cbind(DIFF_TYPE = "ABS", abs),
  cbind(DIFF_TYPE = "REL", rel)
)

## print results
res[, c('DIFF_TYPE', 'OVERALL', 'BOTTOM_QUANT')]
