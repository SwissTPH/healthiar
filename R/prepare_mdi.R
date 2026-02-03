#' Create the BEST-COST Multidimensional Deprivation Index (MDI)

# DESCRIPTION ##################################################################
#' @description
#' This function creates the BEST-COST Multidimensional Deprivation Index (MDI) and checks internal
#' consistency of the single deprivation indicators using Cronbach's coefficient \eqn{\alpha} and
#' other internal consistency checks

# ARGUMENTS ####################################################################
#' @inheritParams socialize
#' @param edu \code{Numeric vector} indicating educational attainment as \% of individuals
#' (at the age 18 or older) without a high school diploma (ISCED 0-2) per geo unit
#' @param unemployed \code{Numeric vector} containing \% of unemployed individuals in the active
#' population (18-65) per geo unit
#' @param single_parent \code{Numeric vector} containing single-parent households as \% of total
#' households headed by a single parent per geo unit
#' @param pop_change \code{Numeric vector} containing population change as \% change in population
#' over the previous 5 years (e.g., 2017-2021) per geo unit
#' @param no_heating \code{Numeric vector} containing \% of households without central heating per
#' geo unit
#' @param verbose \code{Boolean} indicating whether function output is printed to console.
#' Default: \code{TRUE}.

# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' Information about the methodology
#' (including corresponding equations and literature)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#multiple-deprivation-index}{Multiple deprivation index}}
#'
#'
#' \strong{The function outputs Cronbach's alpha}
#'
#' See below the considered reliability based on the alpha value:
#'
#' \itemize{
#'  \item{0.9 and higher: Excellent reliability}
#'  \item{between 0.8 (included) and 0.9: Good reliability}
#'  \item{between 0.7 (included) and 0.8: Acceptable reliability}
#'  \item{between 0.6 (included) and 0.7: Questionable reliability}
#'  \item{lower than 0.6: Poor reliability}}
#'
#'
#' \strong{Data completeness and imputation}
#'
#' Ensure the data set is as complete as possible. Otherwise, you can try to impute missing data,
#' but R^2 should be greater than or equal to 0.7.
#'
#' \strong{Plots}
#'
#' See the example below for how to reproduce the box plots and the histogram after the \code{prepare_mdi} function call.
#'
# VALUE ########################################################################
#' @return
#' This function returns a \code{list} containing
#'
#' 1) \code{mdi_main} (\code{tibble}) with the columns (selection);
#' \itemize{
#'   \item \code{geo_id_micro} containing the \code{numeric} geo id's
#'   \item \code{MDI} containing the \code{numeric} BEST-COST Multidimensional Deprivation Index values
#'   \item \code{MDI_index} \code{numeric} decile based on values in the column \code{MDI}
#'   \item additional columns containing the function input data
#' }
#'
#' 2) \code{mdi_detailed} (\code{list}) with several elements for the internal consistency check of the BEST-COST
#'   Multidimensional Deprivation Index.
#' \itemize{
#'   \item \code{boxplot} (\code{language}) containing the code to reproduce the boxplot of the single indicators
#'   \item \code{histogram} (\code{language}) containing the code to reproduce a histogram of the BEST-COST
#'   Multidimensional Deprivation Index (MDI) values with a normal distribution curve
#'   \item \code{descriptive_statistics} (\code{list} table of descriptive statistics (mean, SD, min, max) of the normalized input data and the MDI
#'   \item \code{cronbachs_alpha_value} (\code{numeric value} See the Details section for the reliability rating this value indicates
#'   \item \code{pearsons_corr_coeff} (\code{numeric vector}) Person's correlation coefficient (pairwise-comparisons)
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
#'   n_quantile = 10,
#'   verbose = TRUE
#' )
#'
#' results$mdi_main |>
#'   dplyr::select(geo_id_micro, MDI, MDI_index) |>
#'   dplyr::slice(1:15)
#'
#' # Reproduce plots after the function call
#' eval(results$mdi_detailed$boxplot)
#' eval(results$mdi_detailed$histogram)
#'
#' @references
#'
#' \insertRef{Cronbach1951_p}{healthiar}
#'
#' \insertRef{Gorasso2024_report}{healthiar}
#'
#' \insertRef{Mogin2025_ejph}{healthiar}
#'
#'
#'
#' @author Carl Baravelli, Vanessa Gorasso, Alberto Castro & Axel Luyten
#'
#' @export



prepare_mdi <- function(
    geo_id_micro,
    edu,
    unemployed,
    single_parent,
    pop_change,
    no_heating,
    n_quantile,
    verbose = TRUE
) {

  # Create helper functions ####################################################

  ## Create helper function that normalizes indicators using min-max scaling
  normalize <- function(x) {
    return(
      (x - base::min(x, na.rm = TRUE)) / (base::max(x, na.rm = TRUE) - base::min(x, na.rm = TRUE))
      )
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
    dplyr::mutate(
      dplyr::across(
        c(edu, unemployed, single_parent, pop_change, no_heating),
        normalize,
        .names = "norm_{.col}")
    )

  data$MDI <- base::with(
    data,
    (norm_edu + norm_unemployed + norm_single_parent + norm_pop_change + norm_no_heating) / 5
  )

  ## Create quantile ranks
  data$MDI_index <- dplyr::ntile(data$MDI, n_quantile)

  data |>
    dplyr::relocate(MDI, .after = geo_id_micro) |>
    dplyr::relocate(MDI_index, .after = MDI)

  # Check internal consistency ################################################

  indicators <- c(
    "norm_edu",
    "norm_unemployed",
    "norm_single_parent",
    "norm_pop_change",
    "norm_no_heating"
    )

  # * Cronbach's alpha ########################################################

  # Store non-ASCII characters as unicode escape to avoid errors

  cronbachs_alpha_value <- cronbach_alpha(
    data[, indicators])

  # * Descriptive analysis ####################################################

  descriptive_statistics <- base::sapply(data[c(indicators, "MDI")], function(x)
    tibble::tibble(
      MEAN = base::round(base::mean(x), 3),
      SD = base::round(stats::sd(x), 3),
      MIN = base::min(x),
      MAX = base::max(x)
      )
    )

  # * Pearson’s correlation coefficients for each indicator ####################

  pearsons_corr_coeff <- stats::cor(
    data[,indicators],
    use = "pairwise.complete.obs",
    method = "pearson"
    )

  # * Boxplot #################################################################

  cols <- c(indicators, "MDI")

  boxplot_code <- base::substitute({ # save code and data in a variable to plot it later)
    graphics::boxplot(
      data[ , cols],
      main = "Boxplot of Normalized Indicators and MDI",
      xlab = "Indicator",
      ylab = "Value",
      col = "lightgray",     # optional: add some color for clarity
      border = "darkgray",   # mimic ggplot's minimal theme
      outline = TRUE,
      axes = FALSE
    )
    graphics::box(bty = "l")  # remove top and right box borders (like theme_minimal from ggplot2)
    graphics::axis(2) # add y-axis
    at_pos <- base::seq_along(cols)
    graphics::axis(1, at = at_pos, labels = FALSE)  # Add custom x-axis tick marks
    ## Add rotated labels
    graphics::text(
      x = at_pos,
      ## position slightly below axis
      y = graphics::par("usr")[3] - 0.02 * base::diff(graphics::par("usr")[3:4]),
      labels = cols,
      srt = 20,           # rotate 45 degrees
      adj = 1,            # right-aligned
      xpd = TRUE,         # allow drawing outside plot area
      cex = 0.9
    )
  },
  list(
    cols = cols,
    data = data
    )
  )
  boxplot <- boxplot_code

  # * Histogram ###############################################################

  histogram_code <- base::substitute({
    graphics::hist(
      data$MDI,
      breaks = 30,
      freq = FALSE, # use density instead of counts
      col = grDevices::rgb(0.2, 0.4, 0.8, 0.5),  # semi-transparent fill (like ggplot alpha)
      main = "Histogram of MDI with Normal Curve",
      xlab = "MDI",
      ylab = "Density",
      xlim = c(0, 1),
      xaxt = "n" # suppress x-axis to add custom ticks
    )
    graphics::axis(1, at = seq(0, 1, by = 0.2)) # Add x-axis ticks every 0.2
    ## Add density line
    graphics::lines(
      stats::density(data$MDI, na.rm = TRUE),
      col = "red",
      lwd = 2
      )
    graphics::box(bty = "l") # Optional minimal styling
  },
  list(
    data = data[, "MDI"]
  ))
  histogram <- histogram_code

  # PRINT OUTPUTS #############################################################
  if (verbose == TRUE) { # only print if user has not specified verbose == FALSE

    # * Cronbach's alpha ######################################################

    ## with alpha and >= & <= sympbols
    alpha <- "\u03B1"
    higher_or_equal <- "\u2265"
    lower_or_equal <- "\u2264"

    base::print(base::paste("CRONBACH'S", alpha, ":", base::round(cronbachs_alpha_value, 3)))

    if ( cronbachs_alpha_value >= 0.9 ) {
      base::print(base::paste("Excellent reliability:", alpha, higher_or_equal, "0.9"))
    }
    if ( cronbachs_alpha_value >= 0.8 & cronbachs_alpha_value < 0.9 ) {
      base::print(base::paste("Good reliability: 0.8", lower_or_equal, alpha, "< 0.9"))
      }
    if ( cronbachs_alpha_value >= 0.7 & cronbachs_alpha_value < 0.8 ) {
      base::print(base::paste("Acceptable reliability: 0.7", lower_or_equal, alpha, "< 0.8"))
    }
    if ( cronbachs_alpha_value >= 0.6 & cronbachs_alpha_value < 0.7 ) {
      base::print(base::paste("Questionable reliability: 0.6", lower_or_equal, alpha, "< 0.7"))
    }
    if ( cronbachs_alpha_value < 0.6 ) {
      base::print(base::paste("Poor reliability:", alpha, "< 0.6"))
    }

    ## with just strings
    # base::print(base::paste("CRONBACH'S alpha:", base::round(cronbachs_alpha_value, 3)))
    #
    # if ( cronbachs_alpha_value >= 0.9 ) {
    #   base::print(base::paste("Excellent reliability: alpha >= 0.9"))
    # }
    # if ( cronbachs_alpha_value >= 0.8 & cronbachs_alpha_value < 0.9 ) {
    #   base::print(base::paste("Good reliability: 0.8 <= alpha < 0.9"))
    # }
    # if ( cronbachs_alpha_value >= 0.7 & cronbachs_alpha_value < 0.8 ) {
    #   base::print(base::paste("Acceptable reliability: 0.7 <= alpha < 0.8"))
    # }
    # if ( cronbachs_alpha_value >= 0.6 & cronbachs_alpha_value < 0.7 ) {
    #   base::print(base::paste("Questionable reliability: 0.6 <= alpha < 0.7"))
    # }
    # if ( cronbachs_alpha_value < 0.6 ) {
    #   base::print(base::paste("Poor reliability: alpha < 0.6"))
    # }

    # * Descriptive analysis ##################################################

    base::print("DESCRIPTIVE STATISTICS")
    base::print(descriptive_statistics)

    # * Pearson’s correlation coefficients for each indicator #################

    base::print("PEARSON'S CORRELATION COEFFICIENTS")
    base::print(pearsons_corr_coeff)

    # * Boxplot #################################################################

    base::eval(boxplot_code)

    # * Histogram ###############################################################

    base::eval(histogram_code)

  }

    mdi_main <- data

  output <-
    base::list(
      mdi_main = mdi_main,
      mdi_detailed = base::list(
        boxplot = boxplot,
        histogram = histogram,
        descriptive_statistics = descriptive_statistics,
        cronbachs_alpha_value = cronbachs_alpha_value,
        pearsons_corr_coeff = pearsons_corr_coeff
      )
    )

  return(output)
}
