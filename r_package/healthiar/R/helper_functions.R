#' Helper function: extract main health results

#' @description This helper function extracts the main results from a variable containing the ouput of an \code{attribute} call.
#' @param x \code{Variable containing the ouput of an \code{attribute} call}
#' @returns
#' This helper function returns the main health result(s) as a \code{vector}.
#' @author Axel Luyten
#' @note Helper function
#' @export

helper_extract_main_health_results <- function(x){
  y <- x |>
    purrr::pluck("health_main") |>
    dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
    dplyr::select(impact_rounded)  |>
    base::unlist() |>
    base::as.numeric()
}

#' Helper function: extract detailed health results

#' @description This helper function extracts the main results from a variable containing the ouput of an \code{attribute} call.
#' @param x \code{Variable containing the ouput of an \code{attribute} call}
#' @returns
#' This helper function returns the main health result(s) as a \code{vector}.
#' @author Axel Luyten
#' @note Helper function
#' @export

helper_extract_detailed_health_results <- function(x){
  y <- x |>
    purrr::pluck("health_detailed") |>
    purrr::pluck("impact_raw") |>
    dplyr::select(impact_rounded)  |>
    base::unlist() |>
    base::as.numeric()
}

#' Helper function: generate values with a seed

#' @description This helper function generates values with a seed
#' @param n \code{Numeric value} Number of values to be generated
#' @param min,max \code{Numeric values} Lower and upper limits
#' @param seed \code{Numeric values} Seed to be used
#' @returns
#' This helper function returns generated values as a \code{vector}.
#' @author Alberto Castro
#' @note Helper function
#' @export

helper_runif_with_seed <-
  function(n, min, max, seed){
    set.seed(seed)
    output <- runif(n, min, max)
  }

#' Helper function: extract main CBA results

#' @description This helper function extracts the main CBA results from a variable containing CBA results.
#' @param x \code{Variable containing the ouput of an \code{cba} call}
#' @returns
#' This helper function returns the main CBA result(s) as a \code{vector}.
#' @author Axel Luyten
#' @note Helper function
#' @export

helper_extract_main_cba_results <- function(x){
  y <- x |>
    purrr::pluck("cba_main") |>
    (\(x) if ("erf_ci" %in% names(x)) x |> dplyr::arrange(erf_ci) else x)() |> # Ascending order: central, lower, upper
    dplyr::select(net_benefit_rounded)  |>
    base::unlist() |>
    base::as.numeric()
}

#' Helper function: extract main monetization results

#' @description This helper function extracts the main monetization results from a variable containing monetization results.
#' @param x \code{Variable containing the ouput of an \code{include_monetization} call}
#' @returns
#' This helper function returns the main monetization result(s) as a \code{vector}.
#' @author Axel Luyten
#' @note Helper function
#' @export

helper_extract_main_monetization_results <- function(x){
  y <- x |>
    purrr::pluck("monetization_main") |>
    (\(x) if ("erf_ci" %in% names(x)) x |> dplyr::arrange(erf_ci) else x)() |> # Ascending order: central, lower, upper
    (\(df) if ("monetized_impact" %in% names(df))
      df |> dplyr::select(monetized_impact)
     else
       df)() |>
    base::unlist() |>
    base::as.numeric()
}

