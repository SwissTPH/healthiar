#' socialize

#' @description Create and check the BEST-COST Multidimensional Deprivation Index (MDI)
#' @param output_healthiar \code{List} produced by \code{healthiar::attribute()} or \code{healthiar::compare()} as results
#' @param impact \code{Numeric vector} containing the health impacts to be used for social analysis and matched with the argument \code{geo_id_disaggregated}.
#' @param population \code{Integer vector} containing the population per geographic unit and matched with the argument \code{geo_id_disaggregated}.
#' @param social_indicator \code{Vector} with numeric values showing the deprivation score (indicator of economic wealth) of the fine geographical area (it should match with those used in \code{attribute} or \code{compare})
#' @param n_quantile \code{Integer value} specifying to the number quantiles in the analysis
#' @param approach \code{String} referring the approach to include the social aspects. To choose between "quantile" and ?
#' @inheritParams attribute
#'
#' @return Returns the impact (absolute and relative) theoretically attributable to the difference in the social indicator (e.g. degree of deprivation) between the quantiles.
#'
#' @examples
#' # Example of how to use the function
#' function_name(param1 = value1, param2 = value2)
#' @export

# BEST-COST MDI - Example code for creating the Multidimensional Deprivation Index (MDI)
# Using Belgium 2021 Census Data

# REQUIRED INDICATORS:
# Each geographical unit must have data for the following indicators:
# 1. Educational attainment: % of individuals (≥18) without a high school diploma (ISCED 0-2).
# 2. Unemployment: % of unemployed individuals in the active population (18-65).
# 3. Single-parent households: % of total households headed by a single parent.
# 4. Population change: % change in population over the previous 5 years (e.g., 2017-2021).
# 5. Housing conditions: % of households without central heating.

# NOTE: Educational attainment = (Number of individuals ≥18 without a high school diploma in unit k) / (All individuals ≥18 in unit k) * 100

# REQUIRED VARIABLES:
# Dataset must include:
# - `id`: Unique identifier for each geographical unit (e.g., `31005` for Bruges).
# - `edu_attain`: % of individuals (≥18) without a high school diploma.
# - `unemployed`: % of unemployed individuals (18-65).
# - `single_parent`: % of households headed by a single parent.
# - `pop_change`: % change in population over the past 5 years.
# - `no_heating`: % of households without central heating.

# DATA COMPLETENESS AND IMPUTATION:
# Ensure the dataset is as complete as possible. For missing data:
# - Time-Based Imputation: Use linear regression based on historical trends if prior years' data is complete.
# - Indicator-Based Imputation: Use multiple linear regression if the missing indicator correlates strongly with others.
# Imputation models should have an R² ≥ 0.7. If R² < 0.7, consider alternative data sources or methods.


# Load necessary libraries
library(dplyr)
library(ggplot2)

# Load dataset
## Sciensano
# data <- read.csv("//sciensano.be/fs/1147_BESTCOST_Employee/03_Content/WP3/Task3.1/Analysis/Belgium_MDI_2021.csv")
## Swiss TPH
data <- read.csv("../testing/MDI_code_from_WP3/Belgium_MDI_2021.csv")
str(data)

# Normalize indicators using min-max scaling
normalize <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

data <- data %>%
  mutate(across(c(edu, unemployed, single_parent, pop_change, no_heating), normalize, .names = "norm_{.col}"))

# Compute Multidimensional Deprivation Index (MDI)
data$MDI <- with(data,
                 (norm_edu + norm_unemployed + norm_single_parent + norm_pop_change + norm_no_heating) / 5)

#create quantile ranks
#define N of quantile
n <- 10
data$MDI_index <- ntile(data$MDI, n)

# Save results
# write.csv(data, "Belgium_MDI_2021.csv", row.names = FALSE)


##Check internal consistency
indicators <- c("norm_edu", "norm_unemployed", "norm_single_parent", "norm_pop_change", "norm_no_heating")
#Descriptive analysis
sapply(data[c(indicators, "MDI")], function(x)
  data.frame(MEAN = round(mean(x), 3), SD = round(sd(x), 3), MIN = min(x), MAX = max(x)))

# Boxplot
ggplot(stack(data[ ,c(indicators, "MDI")]), aes(x = ind, y = values)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Boxplot of Normalized Indicators and MDI")

#ggsave("boxplot.png")

# Histogram
ggplot(data, aes(x = MDI)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  theme_minimal() +
  ggtitle("Histogram of MDI with Normal Curve")

#ggsave("MDI_hist.png")


#Total MDI Cronbach's
cronbach_alpha <- function(x) {
  N <- ncol(x)  # Number of items
  item_variances <- apply(x, 2, var)  # Variance of each item
  total_variance <- var(rowSums(x))   # Variance of the total score

  # Cronbach's alpha formula
  alpha <- (N / (N - 1)) * (1 - sum(item_variances) / total_variance)
  return(alpha)
}

alpha_value <- cronbach_alpha(
  data[, indicators])

print(paste("Cronbach's Alpha:", round(alpha_value, 3)))
# •	Alpha ≥0.9 → Excellent reliability
# •	0.8 – 0.89 → Good reliability
# •	0.7 – 0.79 → Acceptable reliability
# •	0.6 – 0.69 → Questionable reliability
# •	alpha < 0. → Poor reliability

#Pearson’s correlation coefficient for each indicator
cor(data[,indicators], use = "pairwise.complete.obs", method = "pearson")


#-----
#all in one function with the package "psych"
# Install and load the psych package (if not already installed)
#install.packages("psych")
#library(psych)
reliability <- psych::alpha(
  data %>% select(norm_edu, norm_unemployed, norm_single_parent, norm_pop_change, norm_no_heating))
print(reliability)
