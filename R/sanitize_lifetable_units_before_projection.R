#' Sanitize lifetable units before projection
#'
#' @description
#' This INTERNAL function removes wholly unpopulated lifetable units before
#' projection and stops if structurally inconsistent zero-population rows are
#' found inside units that would otherwise enter projection.
#'
#' @param lifetable_calculation \code{Data frame} with the lifetable input data
#' immediately before survival probabilities are calculated.
#'
#' @returns A \code{data frame} with the same columns as the input and
#' potentially fewer rows after dropping wholly unpopulated units.
#'
#' @author Alberto Castro & Axel Luyten
#'
#' @keywords internal



sanitize_lifetable_units_before_projection <- function(lifetable_calculation) {

  info_cols <- base::grep("^info_", base::names(lifetable_calculation), value = TRUE)

  grouping_cols <- base::intersect(
    c(
      "geo_id_macro",
      "geo_id_micro",
      "sex",
      info_cols,
      "erf_ci",
      "bhd_ci",
      "exp_ci",
      "dw_ci",
      "cutoff_ci",
      "duration_ci",
      "exp_name",
      "year_of_analysis",
      "approach_newborns",
      "approach_exposure",
      "health_outcome",
      "min_age",
      "max_age",
      "time_horizon",
      "approach_risk",
      "is_lifetable",
      "pop_fraction_type"
    ),
    base::names(lifetable_calculation)
  )

  if (base::length(grouping_cols) == 0) {
    base::stop(
      paste0(
        "Could not derive a grouping key for lifetable units before projection."
      ),
      call. = FALSE
    )
  }

  unit_status <- lifetable_calculation |>
    dplyr::summarise(
      .by = dplyr::all_of(grouping_cols),
      all_zero_pop_and_bhd = base::all(population == 0 & bhd == 0),
      any_zero_pop_and_positive_bhd = base::any(population == 0 & bhd > 0),
      any_zero_pop = base::any(population == 0),
      any_positive_pop = base::any(population > 0)
    )

  if (base::any(unit_status$any_zero_pop_and_positive_bhd)) {
    base::stop(
      paste0(
        "Found projected unit(s) with population = 0 and bhd > 0. ",
        "These rows are structurally inconsistent for lifetable projection."
      ),
      call. = FALSE
    )
  }

  if (base::any(unit_status$any_zero_pop & unit_status$any_positive_pop)) {
    base::stop(
      paste0(
        "Found projected unit(s) with zero-population age rows inside otherwise populated units. ",
        "Only wholly unpopulated units may be dropped before lifetable projection."
      ),
      call. = FALSE
    )
  }

  units_to_keep <- unit_status |>
    dplyr::filter(!all_zero_pop_and_bhd) |>
    dplyr::select(dplyr::all_of(grouping_cols))

  lifetable_calculation_filtered <-
    lifetable_calculation |>
    dplyr::semi_join(units_to_keep, by = grouping_cols)

  if (base::nrow(lifetable_calculation_filtered) == 0) {
    base::stop(
      paste0(
        "No lifetable rows remain after dropping wholly unpopulated units before projection."
      ),
      call. = FALSE
    )
  }

  lifetable_calculation_filtered
}
