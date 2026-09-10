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
#' This function condenses socio-economic indicators into
#' a multiple deprivation index (MDI) \insertCite{Mogin2025_ejph}{healthiar}.
#' The reliability of the MDI is assessed using Cronbach's alpha \insertCite{Cronbach1951_p}{healthiar}.
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#multiple-deprivation-index}{Multiple deprivation index}}
#'
#'
#' \strong{Data completeness and imputation}
#'
#' Ensure the data set is as complete as possible. Otherwise, you can try to impute missing data,
#' but R^2 should be greater than or equal to 0.7.
#'
#' \strong{Plots}
#'
#' See the example below for how to reproduce the box plots and
#' the histogram after the \code{prepare_mdi} function call.
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
#'
#' @seealso
#' \itemize{
#'   \item Downstream: \code{\link{socialize}}
#' }
#'
#'
#' @references
#'
#' \insertAllCited{}
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
      (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
      )
  }

  ## Create helper function that calculates total MDI Cronbach's
  cronbach_alpha <- function(x) {
    # Listwise deletion of the geo units with missing values. The variances of
    # the items and the variance of the total score have to refer to the same
    # geo units, so na.rm in each of them separately would mix different
    # subsets and give an alpha that cannot be interpreted. Without this, one
    # single missing value made the whole alpha NA
    x <- x[stats::complete.cases(x), , drop = FALSE]
    # At least two geo units are needed to calculate a variance
    if (nrow(x) < 2) {
      return(NA_real_)
    }
    N <- ncol(x)  # Number of items
    item_variances <- purrr::map_dbl(x, stats::var)  # Variance of each item
    total_variance <- stats::var(rowSums(x))   # Variance of the total score

    ## Cronbach's alpha formula
    alpha <- (N / (N - 1)) * (1 - sum(item_variances) / total_variance)
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

  # Warn about missing values in the indicators.
  # The index of a geo unit with a missing indicator cannot be calculated, so
  # it stays NA, and that geo unit is left out of Cronbach's alpha and of the
  # descriptive statistics. Without this warning one single missing value made
  # the function abort with "missing value where TRUE/FALSE needed" when
  # printing the reliability (verbose = TRUE) or return an alpha of NA and
  # descriptive statistics of NA without saying anything (verbose = FALSE)
  indicator_names <-
    c("edu", "unemployed", "single_parent", "pop_change", "no_heating")

  n_missing_by_indicator <-
    data |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(indicator_names),
                    ~ sum(is.na(.x)))) |>
    unlist()

  if (any(n_missing_by_indicator > 0)) {

    indicators_with_missing <- n_missing_by_indicator[n_missing_by_indicator > 0]

    warning(
      paste0(
        "Missing values in ",
        toString(paste0(names(indicators_with_missing),
                                    " (", indicators_with_missing, ")")),
        ".\n",
        sum(!stats::complete.cases(data[, indicator_names])),
        " of ", nrow(data),
        " geographic unit(s) therefore get no MDI value, and they are not ",
        "included in Cronbach's alpha and in the descriptive statistics. ",
        "Consider imputing the missing data (see the Details section)."),
      call. = FALSE)
  }

  data <- data |>
    dplyr::mutate(
      dplyr::across(
        c(edu, unemployed, single_parent, pop_change, no_heating),
        normalize,
        .names = "norm_{.col}")
    )

  # Unweighted mean of the five indicators, i.e. all of them count the same.
  # This is why they are normalized first: they are entered in different units
  # (e.g. a percentage and a population change), and without the min-max
  # scaling above the indicator with the widest range would dominate the mean
  data$MDI <- with(
    data,
    (norm_edu + norm_unemployed + norm_single_parent + norm_pop_change + norm_no_heating) / 5
  )

  ## Create quantile ranks
  # ntile() gives the quantile 1 to the lowest MDI values
  data$MDI_index <- dplyr::ntile(data$MDI, n_quantile)

  # Assigned back to data, because the result of the pipe was discarded before
  # and the two columns stayed at the end of the table instead of right after
  # geo_id_micro, as documented in the Value section
  data <- data |>
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

  # na.rm = TRUE so that the statistics describe the geo units that do have a
  # value. Without it, one single missing value turned every statistic of the
  # affected indicator (and of the MDI) into NA
  descriptive_statistics <- sapply(data[c(indicators, "MDI")], function(x)
    tibble::tibble(
      MEAN = round(mean(x, na.rm = TRUE), 3),
      SD = round(stats::sd(x, na.rm = TRUE), 3),
      # If an indicator has no value at all, min() and max() with na.rm return
      # -Inf and Inf with a warning. NA says the same thing without pretending
      # to be a number
      MIN = if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
      MAX = if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
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

  boxplot_code <- substitute({ # save code and data in a variable to plot it later)
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
    at_pos <- seq_along(cols)
    graphics::axis(1, at = at_pos, labels = FALSE)  # Add custom x-axis tick marks
    ## Add rotated labels
    graphics::text(
      x = at_pos,
      ## position slightly below axis
      y = graphics::par("usr")[3] - 0.02 * diff(graphics::par("usr")[3:4]),
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

  histogram_code <- substitute({
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

    print(paste("CRONBACH'S", alpha, ":", round(cronbachs_alpha_value, 3)))

    # is.na() because the alpha is NA if fewer than two geographic units have
    # values in all indicators. Without this guard the comparisons below
    # aborted with "missing value where TRUE/FALSE needed"
    if ( is.na(cronbachs_alpha_value) ) {
      print(paste(
        "Reliability cannot be assessed:", alpha,
        "needs at least two geographic units without missing values"))
    } else {
      if ( cronbachs_alpha_value >= 0.9 ) {
        print(paste("Excellent reliability:", alpha, higher_or_equal, "0.9"))
      }
      if ( cronbachs_alpha_value >= 0.8 & cronbachs_alpha_value < 0.9 ) {
        print(paste("Good reliability: 0.8", lower_or_equal, alpha, "< 0.9"))
      }
      if ( cronbachs_alpha_value >= 0.7 & cronbachs_alpha_value < 0.8 ) {
        print(paste("Acceptable reliability: 0.7", lower_or_equal, alpha, "< 0.8"))
      }
      if ( cronbachs_alpha_value >= 0.6 & cronbachs_alpha_value < 0.7 ) {
        print(paste("Questionable reliability: 0.6", lower_or_equal, alpha, "< 0.7"))
      }
      if ( cronbachs_alpha_value < 0.6 ) {
        print(paste("Poor reliability:", alpha, "< 0.6"))
      }
    }
    ## with just strings
    # print(paste("CRONBACH'S alpha:", round(cronbachs_alpha_value, 3)))
    #
    # if ( cronbachs_alpha_value >= 0.9 ) {
    #   print(paste("Excellent reliability: alpha >= 0.9"))
    # }
    # if ( cronbachs_alpha_value >= 0.8 & cronbachs_alpha_value < 0.9 ) {
    #   print(paste("Good reliability: 0.8 <= alpha < 0.9"))
    # }
    # if ( cronbachs_alpha_value >= 0.7 & cronbachs_alpha_value < 0.8 ) {
    #   print(paste("Acceptable reliability: 0.7 <= alpha < 0.8"))
    # }
    # if ( cronbachs_alpha_value >= 0.6 & cronbachs_alpha_value < 0.7 ) {
    #   print(paste("Questionable reliability: 0.6 <= alpha < 0.7"))
    # }
    # if ( cronbachs_alpha_value < 0.6 ) {
    #   print(paste("Poor reliability: alpha < 0.6"))
    # }

    # * Descriptive analysis ##################################################

    print("DESCRIPTIVE STATISTICS")
    print(descriptive_statistics)

    # * Pearson’s correlation coefficients for each indicator #################

    print("PEARSON'S CORRELATION COEFFICIENTS")
    print(pearsons_corr_coeff)

    # * Boxplot and histogram ###################################################

    # The plots need at least one geo unit with an MDI value. Without this
    # guard, graphics::hist() aborted with the message "character(0)" when
    # every MDI was missing, i.e. when no geo unit had a value in all
    # indicators. The code of both plots is returned in mdi_detailed anyway
    if ( any(!is.na(data$MDI)) ) {

      eval(boxplot_code)

      eval(histogram_code)

    } else {
      print(paste(
        "No plots: no geographic unit has a value in all indicators,",
        "so the MDI could not be calculated for any of them"))
    }

  }

    mdi_main <- data

  output <-
    list(
      mdi_main = mdi_main,
      mdi_detailed = list(
        boxplot = boxplot,
        histogram = histogram,
        descriptive_statistics = descriptive_statistics,
        cronbachs_alpha_value = cronbachs_alpha_value,
        pearsons_corr_coeff = pearsons_corr_coeff
      )
    )

  return(output)
}
