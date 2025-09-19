#' Create the BEST-COST Multidimensional Deprivation Index (MDI)

# DESCRIPTION ##################################################################
#' @description
#' This function creates the BEST-COST Multidimensional Deprivation Index (MDI) and checks internal consistency of the single deprivation indicators using Cronbach's coefficient \eqn{\alpha} and other internal consistency checks

# ARGUMENTS ####################################################################
#' @inheritParams socialize
#' @param edu \code{Numeric vector} indicating educational attainment as \% of individuals (older than 18) without a high school diploma (ISCED 0-2) per geo unit
#' @param unemployed \code{Numeric vector} containing \% of unemployed individuals in the active population (18-65) per geo unit
#' @param single_parent \code{Numeric vector} containing single-parent households as \% of total households headed by a single parent per geo unit
#' @param pop_change \code{Numeric vector} containing population change as \% change in population over the previous 5 years (e.g., 2017-2021) per geo unit
#' @param no_heating \code{Numeric vector} containing \% of households without central heating per geo unit

# DETAILS ######################################################################
#' @details
#' The function prints Cronbach's \eqn{\alpha}.
#' \describe{
#'   \item{\eqn{\alpha \geq} 0.9}{Excellent reliability}
#'   \item{0.8 \eqn{\leq \alpha \lt} 0.9}{Good reliability}
#'   \item{0.7 \eqn{\leq \alpha \lt} 0.8}{Acceptable reliability}
#'   \item{0.6 \eqn{\leq \alpha \lt} 0.7}{Questionable reliability}
#'   \item{\eqn{\alpha} < 0.6}{Poor reliability}
#' }
#' @details
#' Data completeness and imputation: ensure the dataset is as complete as possible. You can try to impute missing data:
#' \itemize{
#'   \item Time-Based Imputation: Use linear regression based on historical trends if prior years' data is complete.
#'   \item Indicator-Based Imputation: Use multiple linear regression if the missing indicator correlates strongly with others.
#' }
#' Imputation models should have an R² ≥ 0.7. If R² < 0.7, consider alternative data sources or methods.

# VALUE ########################################################################
#' @return
#' Tibble with the columns
#' \itemize{
#'   \item geo_id_micro
#'   \item BEST-COST Multidimensional Deprivation Index
#' }
#' @return
#' For the internal consistency check the function provides
#' \itemize{
#'   \item Cronbach's \eqn{\alpha} (including the reliability rating this value indicates)
#'   \item Descriptive analysis of the input data
#'   \item Boxplots of the single indicators
#'   \item Histogram of the MDI's for the geo units with a normal distribution curve
#'   \item Person's correlation coefficient (pairwise-comparisons)
#' }

# EXAMPLES #####################################################################
#' @examples
#' # Goal: create the BEST-COST Multidimensional Deprivation Index for
#' # a selection of geographic units
#'
#' results <- prepare_mdi(
#'   geo_id_micro = exdat_prepare_mdi$id,
#'   edu = exdat_prepare_mdi$edu,
#'   unemployed = exdat_prepare_mdi$unemployed,
#'   single_parent = exdat_prepare_mdi$single_parent,
#'   pop_change = exdat_prepare_mdi$pop_change,
#'   no_heating = exdat_prepare_mdi$no_heating,
#'   n_quantile = 10
#' )
#'
#' results |>
#'   dplyr::select(geo_id_micro, MDI, MDI_index) |>
#'   dplyr::slice(1:15)

#' @author Alberto Castro & Axel Luyten

#' @export



prepare_mdi <- function(
    geo_id_micro,
    edu,
    unemployed,
    single_parent,
    pop_change,
    no_heating,
    n_quantile
) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("The 'ggplot2' package is required for this function. Please install it if you want to use this function.", call. = FALSE)}

  # Create helper functions ####################################################

  ## Create helper function that normalizes indicators using min-max scaling
  normalize <- function(x) {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }

  ## Create helper function that calculates total MDI Cronbach's
  cronbach_alpha <- function(x) {
    N <- base::ncol(x)  # Number of items
    item_variances <- base::apply(x, 2, stats::var)  # Variance of each item
    total_variance <- stats::var(base::rowSums(x))   # Variance of the total score

    ## Cronbach's alpha formula
    alpha <- (N / (N - 1)) * (1 - base::sum(item_variances) / total_variance)
    return(alpha)
  }

  # Compute MDI ################################################################
  data <- tibble::tibble(
    geo_id_micro,
    edu,
    unemployed,
    single_parent,
    pop_change,
    no_heating
  )

  data <- data |>
    dplyr::mutate(dplyr::across(c(edu, unemployed, single_parent, pop_change, no_heating), normalize, .names = "norm_{.col}"))

  data$MDI <- base::with(data,
                         (norm_edu + norm_unemployed + norm_single_parent + norm_pop_change + norm_no_heating) / 5)

  ## Create quantile ranks
  data$MDI_index <- dplyr::ntile(data$MDI, n_quantile)

  # Save results
  # write.csv(data, "Belgium_MDI_2021.csv", row.names = FALSE)

  # Check internal consistency ###################################################

  indicators <- c("norm_edu", "norm_unemployed", "norm_single_parent", "norm_pop_change", "norm_no_heating")

  # * Descriptive analysis #######################################################
  print("DESCRIPTIVE STATISTICS")
  print(
    base::sapply(data[c(indicators, "MDI")], function(x)
      tibble::tibble(MEAN = base::round(base::mean(x), 3), SD = base::round(stats::sd(x), 3), MIN = base::min(x), MAX = base::max(x)))
  )

  # * Boxplot ####################################################################

  print(
    ggplot2::ggplot(utils::stack(data[ , c(indicators, "MDI")]), ggplot2::aes(x = ind, y = values)) +
      ggplot2::geom_boxplot(na.rm = TRUE) +
      ggplot2::theme_minimal() +
      ggplot2::ggtitle("Boxplot of Normalized Indicators and MDI") +
      ggplot2::xlab("Indicator") +
      ggplot2::ylab("Value")
  )

  #ggsave("boxplot.png")

  # * Histogram ##################################################################
  print(
    ggplot2::ggplot(data, ggplot2::aes(x = MDI)) +
      ggplot2::geom_histogram(na.rm = TRUE, ggplot2::aes(y = ggplot2::after_stat(density)), bins = 30, alpha = 0.5) +
      ggplot2::geom_density(color = "red") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +  # x-axis from 0 to 1, with ticks every 0.1
      ggplot2::ggtitle("Histogram of MDI with Normal Curve") +
      ggplot2::ylab("Density")
  )
  #ggsave("MDI_hist.png")

  # * Pearson’s correlation coefficient for each indicator #######################
  print("PEARSON'S CORRELATION COEFFICIENTS")
  print(
    stats::cor(data[,indicators], use = "pairwise.complete.obs", method = "pearson")
  )

  # * Cronbach's alpha ###########################################################
  alpha_value <- cronbach_alpha(
    data[, indicators])


  # Store non-ASCII characters as unicode escape to avoid errors
  alpha <- "\u03B1"
  higher_or_equal <- "\u2265"
  lower_or_equal <- "\u2264"

  base::print(base::paste("CRONBACH'S", alpha, ":", base::round(alpha_value, 3)))


  if ( alpha_value >= 0.9 ) base::print(base::paste("Excellent reliability:", alpha, higher_or_equal, "0.9"))
  if ( alpha_value >= 0.8 & alpha_value < 0.9 ) base::print(base::paste("Good reliability: 0.8", lower_or_equal, alpha, "< 0.9"))
  if ( alpha_value >= 0.7 & alpha_value < 0.8 ) base::print(base::paste("Acceptable reliability: 0.7", lower_or_equal, alpha, "< 0.8"))
  if ( alpha_value >= 0.6 & alpha_value < 0.7 ) base::print(base::paste("Questionable reliability: 0.6", lower_or_equal, alpha, "< 0.7"))
  if ( alpha_value < 0.6 ) base::print(base::paste("Poor reliability:", alpha, "< 0.6"))

  return(
    data |>
      dplyr::relocate(MDI, .after = geo_id_micro) |>
      dplyr::relocate(MDI_index, .after = MDI)
  )
}
