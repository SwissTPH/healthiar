#' Create a scenario 2 by modifying an existing scenario 1 and determine attributable health impacts in it
#'
#' @description
#' This function assesses the attributable health impacts in a new scenario 2 which is obtained by modifying an existing scenario 1. Supply an existing attribute output and specify how scenario 1 should be modified to create scenario 2.
#' @param output_attribute_1 \code{List} containing the output of the function attribute() for scenario 1.
#' @inheritParams attribute_master
#' @returns
#' TBD. E.g. This function returns a \code{list} with the attributable
#' Moreover, the data frame includes columns such as:
#' \itemize{
#'  \item Attributable fraction
#'  \item Health impact
#'  \item Outcome metric
#'  \item And many more.
#' }
#' @examples
#' TBD
#' @author Alberto Castro & Axel Luyten
#' @export

attribute_mod <-
  function(
    output_attribute_1,
    erf_shape = NULL,
    rr_central = NULL, rr_lower = NULL, rr_upper = NULL,
    rr_increment = NULL,
    erf_eq_central = NULL, erf_eq_lower = NULL, erf_eq_upper = NULL,
    exp_central = NULL, exp_lower = NULL, exp_upper = NULL,
    prop_pop_exp = NULL,
    pop_exp = NULL,
    cutoff_central = NULL, cutoff_lower = NULL, cutoff_upper = NULL,
    bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
    geo_id_disaggregated = NULL, geo_id_aggregated = NULL,
    population = NULL,
    info = NULL,
    first_age_pop = NULL, last_age_pop = NULL,
    population_midyear_male = NULL, population_midyear_female = NULL,
    deaths_male = NULL, deaths_female = NULL,
    min_age = NULL, max_age = NULL,
    approach_exposure = NULL,
    approach_newborns = NULL,
    year_of_analysis = NULL
    ) {

    # Capture all arguments and values
    args_2 <- as.list(environment())

    # Removing output_attribute_1 from args
    args_2$output_attribute_1 <- NULL

    #Remove all arguments that are NULL in args_2 to avoid that they overwrite
    #those in args_1
    args_2 <- purrr::discard(args_2, is.null)

    # Extract args_1
    args_1 <- output_attribute_1[["health_detailed"]][["input_args"]]

    # Create a function to replace values in list of arguments
    # modifyList() and purrr::list_modify() do not work because require named lists
    # And the list elements are entered by users without name

    replace_list <- function(original, updated) {
      # If an element exists in updates, replace it; otherwise, keep original
      purrr::imap(original,
                  function(.x, .y){
                    if(.y %in% names(updated)){
                      .x<- updated[[.y]]
                      }else {.x <- .x}})
    }

    args_2_after_merge_with_arg1 <-
      replace_list(args_1, args_2)



    # Use the arguments attribute()
    output_attribute_2 <-
      do.call(healthiar:::attribute_master,
              args_2_after_merge_with_arg1)

    return(output_attribute_2)


  }
