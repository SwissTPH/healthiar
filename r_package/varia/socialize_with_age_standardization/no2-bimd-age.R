## AGE-STD AIR POLLUTION BURDEN INEQUALITIES

## required packages
library(dplyr)
library(tidyr)

## read data
data <- readRDS("./no2_bimd_age.rds")

## aggregate impact & population by deprivation decile
data <- rbind(
  aggregate(cbind(POP,IMPACT)~AGE+DECILE+REF, data, sum),
  cbind(
    aggregate(cbind(POP,IMPACT)~AGE+REF, data, sum),
    DECILE = 'ALL'
  )
)

## calculate age-std rates
data$RATE <- 1e5*(data$IMPACT/data$POP)
data$RATE[is.nan(data$RATE)] <- 0
data$RATE <- data$REF*data$RATE
data <- aggregate(RATE~DECILE, data, sum)

## calculate inequalities
abs <- pivot_wider(data, names_from = "DECILE", values_from = "RATE")
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
