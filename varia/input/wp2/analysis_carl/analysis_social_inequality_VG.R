##Carl's analysis
##Premature mortality - BEST-COST MDI
library(MASS)
library(lme4)

#data
dta_merge <-
  # read.csv("//sciensano.be/fs/1147_BESTCOST_Employee/03_Content/WP3/Task3.4_protocol/Analysis/best-cost-mdi-belgium-2021_prem_mortality.csv")
  read.csv("../varia/input/wp2/analysis_carl/best-cost-mdi-belgium-2021.csv") # Error on line 15 with this data
  # read.csv("../varia/input/wp2/analysis_carl/best-cost-mdi-belgium-2021_attr_prem_mortality.csv")
  # read.csv("../varia/input/wp2/analysis_carl/mort_pm25_nis_2021.csv")

str(dta_merge)

##prem_mort is the 10,000 rate of total mortality
with(dta_merge, (mortality_total/population)*1e4)
with(dta_merge, (MORTALITY_TOTAL/POPULATION)*1e4)

##merge with attributable premature mortality
#mortality attributable to PM2.5 in Belgium, in 2021 by LAU2
mort_pm25_nis_2021 <-
  read.csv("../varia/input/wp2/analysis_carl/mort_pm25_nis_2021.csv")
str(mort_pm25_nis_2021)

mort_pm25_nis_2021$PREM_MORTALITY_ATTR <-
  ifelse(mort_pm25_nis_2021$AGE %in% c("[70,75)", "[75,80)", "[80,85)", "[85,Inf)"),
         0, mort_pm25_nis_2021$MORTALITY_ATTR)

mort_agg <- aggregate(
  cbind(POPULATION, MORTALITY_ATTR, PREM_MORTALITY_ATTR)~ SEX + NIS,
  data = mort_pm25_nis_2021, sum)


colnames(dta_merge)[1] <- "NIS"
dta_merge <- merge(mort_agg, dta_merge, by = "NIS")
str(dta_merge)

#To be used the premature attributable mortality rate (10,000)
dta_merge$PREM_MORTALITY_ATTR_RT <- with(dta_merge, (PREM_MORTALITY_ATTR/population)*1e4)
dta_merge$sex_num <- as.factor(dta_merge$sex_num)

# Model 1: Crude
#Female
model1_f <- glm.nb(round(PREM_MORTALITY_ATTR_RT) ~ factor(rank),
                   data = dta_merge[dta_merge$sex_num == "F",])
summary(model1_f)

#Male
model1_m <- glm.nb(round(pre_mort) ~ factor(rank), data = dta_merge[dta_merge$sex_num == "M",])
summary(model1_m)

# Model 2: Crude, by sex, random intercept by region
#Female
model2_f <- glmer.nb(round(pre_mort) ~ factor(rank) +
                     (1 | region), data = dta_merge[dta_merge$sex_num == "F",])
summary(model2_f)

#Male
model2_m <- glmer.nb(round(pre_mort) ~ factor(rank) +
                       (1 | region), data = dta_merge[dta_merge$sex_num == "M",])
summary(model2_m)

# Model 3: Adjusted for age groups, by sex, random intercept by region
colnames(dta_merge)[10:19] <- c(
  "age_prop10_0y", "age_prop10_10y", "age_prop10_20y", "age_prop10_30y", "age_prop10_40y",
  "age_prop10_50y", "age_prop10_60y", "age_prop10_70y", "age_prop10_80y", "age_prop10_85y"
)

#Female
model3_f <- glmer.nb(round(pre_mort) ~ factor(rank) + age_prop10_0y +
                      age_prop10_10y + age_prop10_20y + age_prop10_30y + age_prop10_40y
                      + age_prop10_50y + age_prop10_60y + age_prop10_70y + age_prop10_80y
                     + (1 | region), data = dta_merge[dta_merge$sex_num == "F",])
summary(model3_f)

#Male
model3_m <- glmer.nb(round(pre_mort) ~ factor(rank) + age_prop10_0y +
                       age_prop10_10y + age_prop10_20y + age_prop10_30y + age_prop10_40y
                     + age_prop10_50y + age_prop10_60y + age_prop10_70y + age_prop10_80y
                     + (1 | region), data = dta_merge[dta_merge$sex_num == "M",])
summary(model3_m)


#RII
#Female
rii_f <- glm(round(pre_mort) ~ ridit_rank + age_prop10_0y +
               age_prop10_10y + age_prop10_20y + age_prop10_30y + age_prop10_40y
             + age_prop10_50y + age_prop10_60y + age_prop10_70y + age_prop10_80y,
             data = dta_merge[dta_merge$sex_num == "F",],
               family = poisson(link = "log"))
summary(rii_f)
exp(coef(rii_f))

#Male
rii_m <- glm(round(pre_mort) ~ ridit_rank + age_prop10_0y +
               age_prop10_10y + age_prop10_20y + age_prop10_30y + age_prop10_40y
             + age_prop10_50y + age_prop10_60y + age_prop10_70y + age_prop10_80y,
             data = dta_merge[dta_merge$sex_num == "M",],
             family = poisson(link = "log"))
summary(rii_m)
exp(coef(rii_m))


#SII
sii_f <- glm(round(pre_mort) ~ ridit_rank + age_prop10_0y +
               age_prop10_10y + age_prop10_20y + age_prop10_30y + age_prop10_40y
             + age_prop10_50y + age_prop10_60y + age_prop10_70y + age_prop10_80y,
             data = dta_merge[dta_merge$sex_num == "F",],
             family = poisson(link = "identity"))
#not working
#Error: no valid set of coefficients has been found: please supply starting values
# In addition: Warning message:
#   In log(y/mu) : NaNs produced
