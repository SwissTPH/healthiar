helper_extract_main_results <- function(x){
  y <- x |>
    purrr::pluck("health_main") |>
    dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
    dplyr::select(impact_rounded)  |>
    base::unlist() |>
    base::as.numeric()
}

helper_extract_detailed_results <- function(x){
  y <- x |>
    purrr::pluck("health_detailed") |>
    purrr::pluck("raw") |>
    dplyr::select(impact_rounded)  |>
    base::unlist() |>
    base::as.numeric()
}

runif_with_seed <-
  function(n, min, max, seed){
    set.seed(seed)
    output <- runif(n, min, max)
  }

