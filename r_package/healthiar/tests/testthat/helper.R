helper_extract_main_results <- function(x){
  y <- x |>
    purrr::pluck("health_main") |>
    dplyr::arrange(erf_ci) |> # Ascending order: central, lower, upper
    dplyr::select(impact_rounded)  |>
    base::unlist() |>
    base::as.numeric()
}
