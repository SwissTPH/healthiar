# Load necessary libraries
library(tidyverse)   # For data manipulation
library(lme4)        # For mixed-effects models
library(broom)       # For tidying model outputs
library(haven)       # For reading Stata files (if needed)
library(readr)       # For reading CSV files
library(glmmTMB)     # For negative binomial mixed-effects models

# Set working directory
setwd("C:/Users/CMBA/OneDrive - Folkehelseinstituttet/Projects/BEST-COST/InequalityIndicators")

# Load merged data
merged_data <- read_csv("best-cost-mdi-belgium-2021_prem_mortality.csv")

# Descriptive statistics
desc_stats <- merged_data %>%
  group_by(sex, rank) %>%
  summarise(
    mean_pre_mort = mean(pre_mort, na.rm = TRUE),
    sd_pre_mort = sd(pre_mort, na.rm = TRUE),
    min_pre_mort = min(pre_mort, na.rm = TRUE),
    max_pre_mort = max(pre_mort, na.rm = TRUE),
    q25 = quantile(pre_mort, 0.25, na.rm = TRUE),
    q75 = quantile(pre_mort, 0.75, na.rm = TRUE),
    .groups = 'drop'
  )

# Absolute range and ratio
mort_low <- desc_stats %>% filter(rank == 1) %>% pull(mean_pre_mort)
mort_high <- desc_stats %>% filter(rank == 10) %>% pull(mean_pre_mort)
difference <- mort_high - mort_low
ratio <- mort_high / mort_low

cat("Difference (10th - 1st rank):", difference, "\n")
cat("Ratio (10th / 1st rank):", ratio, "\n")

# Regression analysis
merged_data <- merged_data %>%
  mutate(sex_num = as.numeric(as.factor(sex)))

# Females
# Model 1: Crude
model1_f <- glm.nb(pre_mort ~ factor(rank), data = merged_data %>% filter(sex_num == 1))
summary(model1_f)

# Model 2: Crude, by sex, random intercept by region
model2_f <- glmer.nb(pre_mort ~ factor(rank) + (1 | region), data = merged_data %>% filter(sex_num == 1))
summary(model2_f)

# Model 3: Adjusted for age groups, by sex, random intercept by region
model3_f <- glmer.nb(pre_mort ~ factor(rank) + nis_age_prop10y_0 + nis_age_prop10y_10 +
                       nis_age_prop10y_20 + nis_age_prop10y_30 + nis_age_prop10y_50 +
                       nis_age_prop10y_60 + nis_age_prop10y_70 + nis_age_prop10y_80 +
                       nis_age_prop10y_85 + (1 | region), data = merged_data %>% filter(sex_num == 1))
summary(model3_f)

# Males
# Model 1: Crude
model1_m <- glm.nb(pre_mort ~ factor(rank), data = merged_data %>% filter(sex_num == 2))
summary(model1_m)

# Model 2: Crude, by sex, random intercept by region
model2_m <- glmer.nb(pre_mort ~ factor(rank) + (1 | region), data = merged_data %>% filter(sex_num == 2))
summary(model2_m)

# Model 3: Adjusted for age groups, by sex, random intercept by region
model3_m <- glmer.nb(pre_mort ~ factor(rank) + nis_age_prop10y_0 + nis_age_prop10y_10 +
                       nis_age_prop10y_20 + nis_age_prop10y_30 + nis_age_prop10y_50 +
                       nis_age_prop10y_60 + nis_age_prop10y_70 + nis_age_prop10y_80 +
                       nis_age_prop10y_85 + (1 | region), data = merged_data %>% filter(sex_num == 2))
summary(model3_m)

# RII and SII
# Females
merged_data <- merged_data %>%
  group_by(sex_num) %>%
  mutate(ridit_rank = (rank - min(rank)) / (max(rank) - min(rank))) %>%
  ungroup()

# RII
rii_f <- glm(pre_mort ~ ridit_rank + nis_age_prop10y_0 + nis_age_prop10y_10 +
               nis_age_prop10y_20 + nis_age_prop10y_30 + nis_age_prop10y_50 +
               nis_age_prop10y_60 + nis_age_prop10y_70 + nis_age_prop10y_80 +
               nis_age_prop10y_85, data = merged_data %>% filter(sex_num == 1),
             family = poisson(link = "log"))
summary(rii_f)

# SII
sii_f <- glm(pre_mort ~ ridit_rank + nis_age_prop10y_0 + nis_age_prop10y_10 +
               nis_age_prop10y_20 + nis_age_prop10y_30 + nis_age_prop10y_50 +
               nis_age_prop10y_60 + nis_age_prop10y_70 + nis_age_prop10y_80 +
               nis_age_prop10y_85, data = merged_data %>% filter(sex_num == 1),
             family = poisson(link = "identity"))
summary(sii_f)

# Males
# RII
rii_m <- glm(pre_mort ~ ridit_rank + nis_age_prop10y_0 + nis_age_prop10y_10 +
               nis_age_prop10y_20 + nis_age_prop10y_30 + nis_age_prop10y_50 +
               nis_age_prop10y_60 + nis_age_prop10y_70 + nis_age_prop10y_80 +
               nis_age_prop10y_85, data = merged_data %>% filter(sex_num == 2),
             family = poisson(link = "log"))
summary(rii_m)

# SII
sii_m <- glm(pre_mort ~ ridit_rank + nis_age_prop10y_0 + nis_age_prop10y_10 +
               nis_age_prop10y_20 + nis_age_prop10y_30 + nis_age_prop10y_50 +
               nis_age_prop10y_60 + nis_age_prop10y_70 + nis_age_prop10y_80 +
               nis_age_prop10y_85, data = merged_data %>% filter(sex_num == 2),
             family = poisson(link = "identity"))
summary(sii_m)

# Save the final dataset
final_data <- merged_data %>%
  select(-starts_with("popchange"))

save(final_data, file = "best-cost-mdi-belgium-2021_prem_mortality.RData")