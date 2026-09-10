#' Aggregate health impacts from multiple exposures

# DESCRIPTION ##################################################################
#' @description
#' This function aggregates health impacts from multiple exposures to environmental stressors.

# ARGUMENTS ####################################################################
#' @param output_attribute_exp_1,output_attribute_exp_2  Output of attribute() for exposure 1 and 2, respectively. Baseline health data and population must be identical in outputs 1 and 2.
#' @param exp_name_1,exp_name_2 \code{String} referring to the name of the environmental exposures 1 and 2
#' @param approach_multiexposure \code{String} specifying the multiple exposures approach to be used in the assessment. Options: "additive" (default), "multiplicative" or "combined". The \code{"multiplicative"} and \code{"combined"} approaches merge the exposures within each row and therefore require one single exposure value per exposure (e.g. the population-weighted mean); they cannot be used with exposure distributions, because the exposure categories of two exposures are not paired. Only \code{"additive"} accepts exposure distributions.

# DETAILS ######################################################################
#' @details
#'
#' \strong{Methodology}
#'
#' This function can add up the attributable health impacts from correlated exposures
#' applying one of the following methods \insertCite{Strak2024_report}{healthiar}:
#' \itemize{
#'  \item Additive \insertCite{Steenland2006-e}{healthiar}
#'  \item Multiplicative \insertCite{Jerrett2013-oup}{healthiar}
#'  \item Combined \insertCite{Steenland2006-e}{healthiar}
#'  }
#'
#' Detailed information about the methodology (including equations)
#' is available in the package vignette.
#' More specifically, see chapters:
#' \itemize{
#'  \item \href{https://swisstph.github.io/healthiar/articles/intro_to_healthiar.html#two-correlated-exposures}{Two correlated exposures}}
#'
# VALUE ########################################################################
#' @inherit attribute_master return

# EXAMPLES #####################################################################
#' @examples
#' # Goal: determine aggregated health impacts from multiple exposures
#' # Step 1: create assessment with exposure 1
#' output_attribute_exp_1 <- attribute_health(
#'   erf_shape = "log_linear",
#'   rr_central = 1.369,
#'   rr_increment = 10,
#'   exp_central = 8.85,
#'   cutoff_central = 5,
#'   bhd_central = 30747
#' )
#' output_attribute_exp_1$health_main$impact
#' # Step 2: create assessment with exposure 2
#' output_attribute_exp_2 <- attribute_mod(
#'   output_attribute = output_attribute_exp_1,
#'   exp_central = 10.9,
#'   rr_central = 1.031
#' )
#' output_attribute_exp_2$health_main$impact
#' # Step 3: aggregate impacts of the two assessments
#' results <- multiexpose(
#'   output_attribute_exp_1 = output_attribute_exp_1,
#'   output_attribute_exp_2 = output_attribute_exp_2,
#'   exp_name_1 = "pm2.5",
#'   exp_name_2 = "no2",
#'   approach_multiexposure = "multiplicative"
#' )
#' results$health_main$impact
#'
#'
#' @seealso
#' \itemize{
#'   \item Upstream: \code{\link{attribute_health}}, \code{\link{attribute_lifetable}}
#' }
#'
#' @references
#'
#' \insertAllCited{}
#'
#'
#' @author Alberto Castro & Axel Luyten
#'
#' @export



multiexpose <-
  function(
    output_attribute_exp_1,
    output_attribute_exp_2,
    exp_name_1,
    exp_name_2,
    approach_multiexposure = "additive"){

    # Capture all arguments and values
    input_args <-
      get_input_args(environment = base::environment(),
                     call = match.call())

    pop_fraction_type <- input_args$value$pop_fraction_type

    input_table_1 <- output_attribute_exp_1[["health_detailed"]][["input_table"]]
    input_table_2 <- output_attribute_exp_2[["health_detailed"]][["input_table"]]

    # Data validation ##########################################################

    # Exposure distributions cannot be merged across exposures.
    # In the additive approach each exposure keeps its own population
    # attributable fraction, which is correctly aggregated over its own exposure
    # categories, so distributions are fine there.
    # In the multiplicative and combined approaches, however, the risks (or the
    # fractions) of the exposures have to be merged within each row. The
    # exposure categories of two exposures are not paired: category 1 of pm2.5
    # has nothing to do with category 1 of no2 and the two exposures can even
    # have a different number of categories. There is therefore no defined way
    # to merge them, so this case is rejected instead of returning a number
    # that cannot be interpreted
    if (approach_multiexposure %in% c("multiplicative", "combined")) {

      exp_names_with_distribution <-
        c(exp_name_1, exp_name_2)[
          purrr::map_lgl(
            base::list(input_table_1, input_table_2),
            ~ base::any(.x$exp_type == "exposure_distribution"))]

      if (base::length(exp_names_with_distribution) > 0) {
        base::stop(
          base::paste0(
            "The ", approach_multiexposure, " approach cannot merge exposure ",
            "distributions (i.e. several exposure categories), ",
            "which were entered for: ",
            base::toString(exp_names_with_distribution), ".\n",
            "The exposure categories of two exposures are not paired, ",
            "so the risks cannot be merged across them.\n",
            "Please enter one single exposure value per exposure ",
            "(e.g. the population-weighted mean) ",
            "or use approach_multiexposure = \"additive\"."),
          call. = FALSE)
      }
    }

    # The life table projects one cohort, which can only be done under one
    # single assumption for each of these characteristics. If the two
    # assessments disagree, the bound table below has two values in the
    # corresponding column and get_impact_with_lifetable() aborted with
    # "the condition has length > 1"
    lifetable_characteristics <-
      c("health_outcome", "approach_exposure", "approach_newborns")

    for (characteristic in
         base::intersect(lifetable_characteristics,
                         base::intersect(base::names(input_table_1),
                                         base::names(input_table_2)))) {

      values <-
        base::unique(c(input_table_1[[characteristic]],
                       input_table_2[[characteristic]]))

      if (base::length(values) > 1) {
        base::stop(
          base::paste0(
            "The life table calculation needs one single value of ",
            characteristic,
            ", but the two assessments contain: ",
            base::toString(values), "."),
          call. = FALSE)
      }
    }


    # Add the exposure names to the input_table
    input_table_1_for_binding <-
      input_table_1 |>
      dplyr::mutate(exp_name = exp_name_1)

    input_table_2_for_binding <-
      input_table_2 |>
      dplyr::mutate(exp_name = exp_name_2)

    #Bind the tables together
    input_table <-
      dplyr::bind_rows(
        input_table_1_for_binding,
        input_table_2_for_binding) |>
    # Add the approach
      dplyr::mutate(
        approach_multiexposure = approach_multiexposure)

      # Calculate the health impacts for each case (uncertainty, category, geo area...)
      results <-
        get_impact(input_table = input_table,
                    pop_fraction_type = "paf")

      # Get the main and detailed output by aggregating and/or filtering cases (rows)
      output <-
        get_output(input_args = input_args,
                   input_table = input_table,
                   intermediate_calculations = results$intermediate_calculations,
                   results_raw = results$results_raw)

      # Put the column exp_name as first column because it is now relevant
      output[["health_detailed"]][c("input_table", "results_raw")] <-
        purrr::map(output[["health_detailed"]][c("input_table", "results_raw")],
                   ~ dplyr::select(.x,
                                   exp_name, dplyr::everything()))




    return(output)

  }
