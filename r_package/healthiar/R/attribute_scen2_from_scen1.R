#' Get attributable health impacts in scenario 2 based on scenario 1

#' @description The function uses the output of attribute for scenario 1 (output_attribute_scen1) to build scenario 2. The values of the arguments of this function (except output_attribute_scen1) are used to replace the values in the arguments of scenario 1 to build scenario 2.
#' @param output_attribute_scen1 \code{List} containing the output of the function attribute() for scenario 1.
#' @inheritParams attribute
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
#' @author Alberto Castro
#' @export

attribute_scen2_from_scen1 <-
  function(
    output_attribute_scen1,
    exp_central = NULL, exp_lower = NULL, exp_upper = NULL,
    bhd_central = NULL, bhd_lower = NULL, bhd_upper = NULL,
    population = NULL,
    prop_pop_exp = NULL,
    pop_exp = NULL,
    approach_exposure = NULL,
    approach_newborns = NULL,
    first_age_pop = NULL, last_age_pop = NULL,
    population_midyear_male = NULL, population_midyear_female = NULL,
    year_of_analysis = NULL,
    info = NULL){

    # Capture all arguments and values
    args2 <- as.list(environment())

    # Removing output_attribute_scen1 from args
    args2$output_attribute_scen1 <- NULL

    #Remove all arguments that are NULL in args2 to avoid that they overwrite
    #those in args1
    args2 <- purrr::discard(args2, is.null)

    # Extract args1
    args1 <- output_attribute_scen1[["health_detailed"]][["args"]]


    args2 <- modifyList(args1, args2)

    output_attribute_scen2 <-
      do.call(healthiar::attribute, args2)

    return(output_attribute_scen2)


  }
