#' Deaths by single age

#' @description
#' This function performs a cost-benefit analysis

#' @param age_group \code{Numeric vector} referring to the first year of n-years interval of the age groups. E.g. c(0, 20, 40, 60) means [0, 20), [20, 40), [40, 60), [60, )
#' @param population \code{Numeric vector} referring to mid-year population by age group.
#' @param bhd \code{Numeric vector} referring to the baseline health data (deaths) by age group.
#' @param fraction_lived \code{Numeric vector} or \code{value} (if all values the same in the vector) referring to the fraction of last year lived.

#' @examples
#' # Goal: Convert 5-year population and death data into 1-year data
#' results  <- prepare_lifetable(
#'   age_group = c(0, 5, 10, 15),
#'   population = c(3387900, 3401300, 3212300, 3026100),
#'   bhd = c(4727, 472, 557, 1323))
#'
#' @returns Baseline health data (deaths) by 1-year interval instead of interval.

#' @export



prepare_lifetable <-
  function(age_group,
           population,
           bhd,
           fraction_lived = 0.5) {

    # TODO: Error if different length
    # TODO: Give structure to code


    input_args_value <- base::list(
      age_group = age_group,
      population = population,
      bhd = bhd,
      fraction_lived = fraction_lived)


    # Error if different length
    error_if_different_length  <- function(var_names){

      # Store variables in a list
      vars_with_same_length <- input_args_value[var_names]

      # Get all lengths of at once
      lengths <- purrr::map_int(vars_with_same_length, length)

      # If not all lenghts are the same
      if (! base::length( base::unique(lengths) ) == 1) {

        #Error
        base::stop(
          base::paste0("The following variables must all have the same length: ",
                       base::paste0(base::names(vars_with_same_length),
                                    collapse = ", "),
                       "."),
          call. = FALSE
        )
      }
    }

    error_if_different_length(c("age_group", "population", "bhd"))

    # Error if_lower_than_min
    error_if_lower_than_min <- function(var_name, min){
      # Store var_value
      var_value <- input_args_value[[var_name]]

      if(base::any(var_value < min)){
        base::stop(
          base::paste0("The values of ", var_name, " cannot be lower than ", min, "."),
          call. = FALSE
        )
      }
    }

    # age_group 0 exists
    error_if_lower_than_min("age_group", min = 0)
    error_if_lower_than_min("fraction_lived", min = 0)
    # population and bhd cannot be 0 (or decimals close to)
    error_if_lower_than_min("population", min = 1)
    error_if_lower_than_min("bhd", min = 1)

    # Error if_lower_than_min
    error_if_higher_than_max <- function(var_name, max){
      # Store var_value
      var_value <- input_args_value[[var_name]]

      if(base::any(var_value > max)){
        base::stop(
          base::paste0("The values of ", var_name, " cannot be higher than ", max, "."),
          call. = FALSE
        )
      }
    }

    error_if_higher_than_max("fraction_lived", max = 1)


    # Get the interval_length base on the difference between values in age_group
    # It has to be constant across age_group values
    age_interval_length <- base::unique(base::diff(age_group))
    # Get the last_age
    # The last element of the age_group vector is the first year of the interval of n-years,
    # so sum age_interval_length
    # and subtract 1 because we are getting the 1-year interval age_group (instead of n-years)
    # and the first year of the interval is now only 1 below
    last_age <- dplyr::last(age_group) + age_interval_length - 1


    # Create table with all input data
    data_n_years <- tibble::tibble(
      age_group = age_group,
      population = population,
      bhd = bhd,
      fraction_lived = fraction_lived
      ) |>
      # Calculate hazard, prob_dying and prob_surviving for the n-years interval
      dplyr::mutate(
        hazard_rate = bhd / population,
        # According to AirQ+ formula
        # Assuming uniform distribution of deaths over the year
        prob_dying = age_interval_length * hazard_rate / (1 + (age_interval_length * (1 - fraction_lived) * hazard_rate)),
        prob_surviving = 1 - prob_dying
        )

    # Create codebook for 1_year and n_year
    # To be used to duplicate all input data as many times as the age interval
    one_vs_n_years <-
      tibble::tibble(
        age_group_1_year = age_group[1] : last_age,
        age_group_n_years = base::rep(age_group, each = age_interval_length),
        age_interval_index = base::rep(1:age_interval_length, times = base::length(age_group)),
        #age_interval_length = age_interval_length
      )

    # Join data to duplicate values in the intermediate ages of the interval
    data <-
      dplyr::left_join(one_vs_n_years,
                       data_n_years,
                       by = c("age_group_n_years" = "age_group")) |>
      # Adding suffix to column names to differentiate between _n_years and 1_year columns
      dplyr::rename_with(.cols = - dplyr::contains("age_"),
                         .fn = ~ base::paste0(., "_n_years"))

    # Create function to obtain entry_population
    get_entry_population <- function(prob_surviving_1_year,
                                     population_n_years,
                                     age_interval_index,
                                     age_interval_length) {

      # Using the formula of AirQ+ would be
      # entry_population_1_year = prob_surviving_1_year^(age_interval_index-1) * population_n_years/(0.5+prob_surviving_1_year+prob_surviving_1_year^2+prob_surviving_1_year^3+prob_surviving_1_year^4+0.5*prob_surviving_1_year^5),
      # but this is only for 5-years interval.
      # A solution for all lengths of interval is needed.

      # Get weights of the formula
      if (age_interval_length == 1) {
        weights <- 1
      } else {
        weights <- c(0.5, base::rep(1, age_interval_length - 1), 0.5)
      }

      # Get powers of the formula

      powers <- 0:age_interval_length

      # Use pmap_dbl to vectorialize the function
      # and consequently to accept vectors
      entry_population <- purrr::map_dbl(
        base::seq_along(prob_surviving_1_year),
        \(i) {
          numerator <- prob_surviving_1_year[i]^(age_interval_index[i] - 1) * population_n_years[i]
          denominator <- base::sum(weights * prob_surviving_1_year[i] ^ powers)

          # Divide numerator by denominator

          return(numerator / denominator)
        }
      )

      return(entry_population)
    }



    # Obtain entry_population, mid-year population and bhd (deaths)
    calculation <- data |>
      dplyr::mutate(
        # Get the value in prob_dying and prob_surviving for 1_year
        # prob_surviving_n_years = prob_surviving_1_year ^ age_interval_length
        prob_surviving_1_year = prob_surviving_n_years^(1/age_interval_length),
        prob_dying_1_year = 1 - prob_surviving_1_year,
        # Calculate entry population using the formula of AirQ+
        entry_population_1_year = get_entry_population(
          prob_surviving_1_year = prob_surviving_1_year,
          population_n_years = population_n_years,
          age_interval_index = age_interval_index,
          # For age_interval_length enter the value of the varible
          # And not the column which is a vector repeating the value (not needed)
          age_interval_length = {{age_interval_length}}
          ),
        # Calculate mid-year population for 1-year interval
        # This is the average between the entry-population in year y and y+1
        # lead() gives the value for y+1
        # colesce(x, 0) replaces the NA with 0
        # There is a NA at the end because the last row has not y+1
        # The assumption is that everybody is death in the row after the last one
        # *0.5 because it is the average
        population_1_year =  0.5 * (entry_population_1_year + dplyr::coalesce(dplyr::lead(entry_population_1_year), 0)),
        bhd_1_year = 2 * (entry_population_1_year - population_1_year)
      )

    # Fix the last element of each interval
    # According to the AirQ+ Life Table Manual,
    # the last element of the each interval is the total deaths minus the sum of the previous interval elements
    calculation_fixed <- calculation |>
      dplyr::group_by(age_group_n_years) |>
      dplyr::mutate(
        bhd_1_year = dplyr::if_else(
          age_interval_index == age_interval_length,
          bhd_n_years - base::sum(bhd_1_year[age_interval_index != age_interval_length]),
          bhd_1_year
        )
      ) |>
      dplyr::ungroup()


      output <- calculation_fixed |>
        dplyr::mutate(
          # Copy columns to indicate which ones are to be used in attribute_... functions
          population_for_attribute = population_1_year,
          bhd_for_attribute = bhd_1_year
        )







    return(output)



  }
