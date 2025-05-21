## AIR POLLUTION BURDEN INEQUALITIES

## required packages
library(dplyr)
library(tidyr)

## read data
mrt <- base::readRDS(testthat::test_path("data", "no2_mrt_mdi.rds"))
ref <- base::readRDS(testthat::test_path("data", "pop_ref.rds"))

## aggregate burden & population by MDI
mrt_mdi <- aggregate(cbind(POP,ATT_MORT)~AGE+MDI, mrt, sum)

## calculate crude rates
rate_cr <- aggregate(cbind(POP,ATT_MORT)~MDI, mrt_mdi, sum)
rate_cr$CRUDE <- 1e5*(rate_cr$ATT_MORT/rate_cr$POP)

## calculate age-std rates
rate_std <- merge(mrt_mdi, ref)
rate_std$ATT_RT <- 1e5*(rate_std$ATT_MORT/rate_std$POP)
rate_std$ATT_RT[is.nan(rate_std$ATT_RT)] <- 0
rate_std$AGE_STD <- rate_std$REF*rate_std$ATT_RT
rate_std <- aggregate(AGE_STD~MDI, rate_std, sum)

## bind crude and age-std rates
rate <- merge(
  rate_cr[, c("MDI", "CRUDE")],
  rate_std
)

## calculate inequalities
ineq <- pivot_longer(rate, cols = CRUDE:AGE_STD, names_to = "RATE", values_to = "VALUE")
ineq$MDI <- paste0("D", ineq$MDI)
ineq <- pivot_wider(ineq, names_from = "MDI", values_from = "VALUE")
ineq$MEAN <- ineq$MEAN <- rowMeans(ineq[startsWith(names(ineq), 'D')])
ineq$OVERALL <- ineq$MEAN - ineq$D10
ineq$BOTTOM <- ineq$D1 - ineq$D10
ineq_rel <- ineq
ineq_rel$OVERALL <- ineq_rel$OVERALL/ineq_rel$MEAN
ineq_rel$BOTTOM <- ineq_rel$BOTTOM/ineq_rel$D10
ineq <- rbind(
  cbind(DIFF_TYPE = "ABS", ineq),
  cbind(DIFF_TYPE = "REL", ineq_rel)
)
ineq <- pivot_longer(
  ineq[, c("RATE", "DIFF_TYPE", "OVERALL", "BOTTOM")],
  cols = OVERALL:BOTTOM, names_to = "DIFF_COMP", values_to = "VALUE")

