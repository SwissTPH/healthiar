#' Get Monte Carlo confidence intervals

#' @description
#' This function determines summary uncertainty (based on at least one input variable
#' with central, lower and upper estimate) based on attribute() or compare()
#' function output.
#' @description
#' Input variables that can included in the Monte Carlo simulation: relative risk, exposure, cutoff, baseline health data, disability weight, duration.

#' @param output_attribute \code{variable} in which the results of an \code{healthiar::attribute_...()} function are stored.
#' @param n_sim \code{numeric value} indicating the number of simulations to be performed.
#' @param seed \code{numeric value} for fixing the randomization. If empty, 123 is used as a default.

#' @details
#' Distributions used for simulation
#' @details
#' Relative risk values are simulated based on an optimized gamma distribution, which fits well as relative risks are positive and its distributions usually right-skewed. The gamma distribution best fitting the inputted central relative risk estimate and corresponding lower and upper 95\% confidence interval values is fitted using \code{stats::qgamma()} (with \code{rate = shape / rr_central}) and then \code{stats::optimize} is used to optimize the distribution parameters. Finally, \code{n_sim} relative risk values are simulated using \code{stats::rgamma()}.
#' @details
#' Exposure values are simulated based on a normal distribution using \code{stats::rnorm()} with \code{mean = exp_central} and a standard deviation based on corresponding lower and upper 95\% exposure confidence interval values.
#' @details
#' Cutoff values are simulated based on a normal distribution using \code{stats::rnorm()} with \code{mean = cutoff_central} and a standard deviation based on corresponding lower and upper 95\% cutoff confidence interval values.
#' @details
#' Baseline health data values are simulated based on a normal distribution using \code{stats::rnorm()} with \code{mean = bhd_central} and a standard deviation based on corresponding lower and upper 95\% exposure confidence interval values.
#' @details
#' Disability weights values of the morbidity health outcome of interest are simulated based on a beta distribution, as both the disability weights and the beta distribution are bounded by 0 and 1. The beta distribution best fitting the inputted central disability weight estimate and corresponding lower and upper 95\% confidence interval values is fitted using \code{stats::qgamma()} (the best fitting distribution parameters \code{shape1} and \code{shape2} are determined using \code{stats::optimize()}). Finally, \code{n_sim} disability weight values are simulated using \code{stats::rbeta()}.
#' @details
#' Duration values of the morbidity health outcome of interest are simulated based on a normal distribution using \code{stats::rnorm()} with \code{mean = duration_central} and a standard deviation based on corresponding lower and upper 95\% exposure confidence interval values.

#' @returns
#' This function returns confidence intervals for the attributable health impacts using a Monte Carlo simulation.

#' @author Axel Luyten

#' @examples
#' TODO

#' @export



summarize_uncertainty <- function(
    output_attribute,
    n_sim,
    seed = NULL) {

  # PREPARATION ################

  ## Decimals ######

  ## Set options
  user_options <- options()
  # Make sure that no rounding occurs
  options(digits = 15)


  ## Seeds ########
  ## Set seed for reproducibility
  if(base::is.null(seed)){seed <- 123}

  var_names <- c("rr", "exp", "cutoff", "bhd", "dw", "duration")
  seeds <- list()

  # Store seed
  for(i in 1:length(var_names)){
    seeds[[var_names[i]]] <- seed +i*1E3
  }

  ## Data sets ##########
  # Store the input data as entered in the arguments
  input_args <- output_attribute[["health_detailed"]][["input_args"]]
  input_table <- output_attribute[["health_detailed"]][["input_table"]]



  # VALIDATION
  # Uncertainty in erf_eq is currently not supported
  # It would require a more complex modelling
  if((!base::is.null(input_args$erf_eq_lower) |
      !base::is.null(input_args$erf_eq_lower))){
    base::stop("Sorry, the summary of uncertainty for erf_eq is not currently supported",
               call. = FALSE)
  }



  # FUNCTION TO SUMMARIZE UNCERTAINTY ######
  # Create the function that make the whole job
  # It is important to create it because for two cases (comparison)
  # the function has to be run more than once (avoid copy-pasted code)
  summarize_uncertainty_based_on_input <-
    function(input_args, input_table){

  ## N #####
  # Determine number of geographic units
  n_geo <-
    # Exceptionally, let's use here unique() instead of input_args
    # because in some cases the users do not enter the geo_id.
    # In that cases compile_input() provide a geo_id and it is shown in impact_raw
    base::length(base::unique(input_table$geo_id_disaggregated))

  # Get the dimension of the exposure
  # (i.e. if pop-weighted mean => 1, if exposure distribution => >1 )
  n_exp <- base::max(input_table$exposure_dimension)


  ## Boolean variables ####

  # Is there a confidence interval? I.e. lower and upper estimate?

  ci_in <- list()

  for (v in var_names){
    ci_in[[v]] <-
      !base::is.null(input_args[[paste0(v, "_lower")]]) &&
      !base::is.null(input_args[[paste0(v, "_upper")]])
  }

  # Beta and gamma functions ############################

  # betaExpert()
  # Copied from source code of prevalence::betaExpert() here:
  # https://github.com/cran/prevalence/blob/master/R/betaExpert.R

  betaExpert <-
    function(best, lower, upper, p = 0.95, method = "mode"){

      ## functions to optimize ~ mode
      f_mode <-
        function(x, mode, p, target){
          return(
            base::sum(
              (stats::qbeta(p = p,
                     shape1 = x,
                     shape2 = (x * (1 - mode) + 2 * mode - 1) / mode) -
                 target) ^ 2
            ))
        }

      f_mode_zero <-
        function(x, p, target){
          return((stats::qbeta(p = p, shape1 = 1, shape2 = x) - target) ^ 2)
        }

      f_mode_one <-
        function(x, p, target){
          return((stats::qbeta(p = p, shape1 = x, shape2 = 1) - target) ^ 2)
        }

      ## functions to optimize ~ mean
      f_mean <-
        function(x, mean, p, target){
          return(
            base::sum(
              (stats::qbeta(p = p,
                     shape1 = x,
                     shape2 = (x * (1 - mean)) / mean) -
                 target) ^ 2
            ))
        }

      ## define 'target' and 'p'
      if (!base::missing(lower) & base::missing(upper)){
        target <- lower
        p <- 1 - p
      } else if (!base::missing(upper) & base::missing(lower)){
        target <- upper
      } else if (!base::missing(upper) & !base::missing(lower)){
        target <- c(lower, upper)
        p <- c(0, p) + (1 - p) / 2
      }

      ## derive a and b (=shape1 and shape2)
      if (method == "mode"){
        if (best == 0){
          a <- 1
          b <- stats::optimize(f_mode_zero, c(0, 1000), p = p, target = target)$minimum
        } else if (best == 1) {
          a <- stats::optimize(f_mode_one, c(0, 1000), p = p, target = target)$minimum
          b <- 1
        } else {
          a <- stats::optimize(f_mode, c(0, 1000),
                        mode = best, p = p, target = target)$minimum
          b <- (a * (1 - best) + 2 * best - 1) / best
        }
      } else if (method == "mean"){
        a <- stats::optimize(f_mean, c(0, 1000),
                      mean = best, p = p, target = target)$minimum
        b <- (a * (1 - best)) / best
      }

      ## create 'out' dataframe
      out <- base::list(alpha = a, beta = b)
      base::class(out) <- "betaExpert"

      ## return 'out'
      return(out)
    }

  # Define helper functions for fitting a gamma distribution with optimization
  # for the relative risk.
  # NOTE: the functions were adapted from those provided by Sciensano

  # Set gamma distribution specs
  vector_probabilities <- c(0.025, 0.975)
  # shape parameter of the gamma distribution
  par <- 2

  ## Fit gamma distribution
  f_gamma <-
    function(par, central_estimate, vector_propabilities, lower_estimate, upper_estimate) {
      qfit <- stats::qgamma(p = vector_propabilities, shape = par, rate = par / central_estimate)
      return(base::sum((qfit - c(lower_estimate, upper_estimate))^2))
    }

  ## Optimize gamma distribution
  optim_gamma <-
    function(central_estimate, lower_estimate, upper_estimate) {
      vector_propabilities <- c(0.025, 0.975)
      f <- stats::optimize(f = f_gamma,
                    interval = c(0, 1e9),
                    central_estimate = central_estimate,
                    vector_propabilities = vector_probabilities,
                    lower_estimate = lower_estimate,
                    upper_estimate = upper_estimate)
      return(c(f$minimum, f$minimum / central_estimate))
    }

  ## Simulate values based on optimized gamma distribution
  sim_gamma <-
    function(n_sim, central_estimate, lower_estimate, upper_estimate) {
      fit <- optim_gamma(central_estimate, lower_estimate, upper_estimate)
      gamma <- stats::rgamma(n = n_sim, fit[1], fit[2])
      return(gamma)}


  # Simulate function #####################
  simulate <- function(central, lower, upper, distribution, n, seed){
    base::set.seed(seed)

    if(distribution == "gamma"){

      simulation <-
        #abs() because negative values have to be avoided
        base::abs(
          sim_gamma(
            n_sim = n,
            central_estimate = central,
            lower_estimate = lower,
            upper_estimate = upper))

    } else if (distribution == "normal"){
        simulation <-
          #abs() because negative values have to be avoided
          base::abs(
            stats::rnorm(
              n = n,
              mean = base::unlist(central),
              sd = (base::unlist(upper) - base::unlist(lower)) / (2 * qnorm(0.975))))

    } else if (distribution == "beta") {

      simulation_betaExpert <-
        betaExpert(
          best = central,
          lower = lower,
          upper = upper,
          method = "mean")

      simulation <-
        stats::rbeta(
          n = n,
          shape1 = base::as.numeric(base::unname(simulation_betaExpert["alpha"])),
          shape2 = base::as.numeric(base::unname(simulation_betaExpert["beta"])))
    }

  }

  ## Template and simulations #####
  sim_template <-
    tibble::tibble(
      geo_id_disaggregated = base::unique(input_table$geo_id_disaggregated),
      geo_id_number = 1:n_geo,
      sim_id = list(1:n_sim))

  # Identify the variable names with confidence interval
  var_names_with_ci <- base::names(ci_in)[unlist(ci_in)]
  # Identify the central variable names with confidence interval
  var_names_with_ci_central <- base::paste0(var_names_with_ci, "_central")
  # Identify those var_names_with_ci that have simulated values different in all geo units
  var_names_with_ci_geo_different <- var_names_with_ci[var_names_with_ci %in% c("exp", "bhd")]
  var_names_with_ci_geo_different_central <- base::paste0(var_names_with_ci_geo_different, "_central")
  # And now identical
  var_names_with_ci_geo_identical <- var_names_with_ci[var_names_with_ci %in% c("rr", "cutoff", "dw", "duration")]
  var_names_with_ci_geo_identical_central <- base::paste0(var_names_with_ci_geo_identical, "_central")
  # Columns to unnest below
  cols_to_unnest <- c("sim_id",
                      var_names_with_ci_central)


  # Define the mapping between variable names and their distributions
  sim_config <- base::list(
    rr = "gamma",
    exp = "normal",
    cutoff = "normal",
    bhd = "normal",
    dw = "beta",
    duration = "normal"
  )

  # Prepare the list to store the data in the for loop
  sim <- list()

  # Apply simulation for variables enabled in ci_in
  for (var in var_names_with_ci) {
    # Store distribution
    dist <- sim_config[[var]]
    # Store column name
    col_name <- paste0(var, "_central")

    # Store central, lower and upper estimate for the simulation below
    central <- as.numeric(input_args[[paste0(var, "_central")]])
    lower   <- as.numeric(input_args[[paste0(var, "_lower")]])
    upper   <- as.numeric(input_args[[paste0(var, "_upper")]])

    # Run simulate across all rows
    # (iteration across simulations, geo_units, exp categories...)
    sim[[col_name]] <- purrr::map(
      .x = sim_template$geo_id_number,
      ~ simulate(
        central = central,
        lower = lower,
        upper = upper,
        distribution = dist,
        n = n_sim,
        # Different seed for each geo_unit to avoid similar results across geo_units
        seed = seeds[[var]] + .x
      )
    )
  }



  # Identify the variables that have to be removed in input_args
  # (to be used below)
  vars_to_be_removed_in_input_args <-
    base::paste0(
      base::rep(var_names_with_ci, each = 3),
      c("_central", "_lower", "_upper"))

  # Prepare input_args to accommodate the new simulated values
  # (to be used below)
  input_args_prepared_for_replacement <-
    purrr::keep(input_args,
                !names(input_args) %in% vars_to_be_removed_in_input_args)

  # Deactivated code: Activate it if attribute is vectorialized
  # input_args_clean <-
  #   purrr::map(input_args_prepared_for_replacement,
  #              \(.x) if (base::is.null(.x)) return(NA) else .x)
  #
  #
  # input_args_tibble <- tibble::as_tibble(input_args_clean)
  #
  # input_args_for_all_sim <-
  #   dplyr::left_join(
  #     template_with_sim,
  #     input_args_tibble,
  #     by = "geo_id_disaggregated"
  #   )

  template_with_sim <-
    # Bind the template with the simulated values
    dplyr::bind_cols(sim_template, tibble::as_tibble(sim)) |>
    # Unnest to have table layout
    tidyr::unnest(dplyr::any_of(cols_to_unnest))


  new_values_for_replacement <- template_with_sim |>
    dplyr::select(-geo_id_disaggregated, -geo_id_number, -sim_id)


  input_args_for_all_sim <-
    purrr::pmap(new_values_for_replacement,
                \(...) {c(input_args_prepared_for_replacement, base::list(...))})

  # Deactivated: This code is super slightly faster by longer than the code above and thus more difficult to maintain
  # Let's keep it here for a while just in case we ever consider to take it back
  # # Create function to speed up the multiple calling of attribute_master
  # call_attribute_master <- function(args){
  #   healthiar:::attribute_master(
  #     is_lifetable = args$is_lifetable,
  #     approach_risk = args$approach_risk,
  #     exp_central = args$exp_central, exp_lower = args$exp_lower, exp_upper = args$exp_upper,
  #     prop_pop_exp = args$prop_pop_exp,
  #     pop_exp = args$pop_exp,
  #     cutoff_central = args$cutoff_central, cutoff_lower = args$cutoff_lower, cutoff_upper = args$cutoff_upper,
  #     rr_central = args$rr_central, rr_lower = args$rr_lower, rr_upper = args$rr_upper,
  #     rr_increment = args$rr_increment,
  #     erf_shape = args$erf_shape,
  #     erf_eq_central = args$erf_eq_central, erf_eq_lower = args$erf_eq_lower, erf_eq_upper = args$erf_eq_upper,
  #     bhd_central = args$bhd_central, bhd_lower = args$bhd_lower, bhd_upper = args$bhd_upper,
  #     population = args$population,
  #     approach_exposure = args$approach_exposure,
  #     approach_newborns = args$approach_newborns,
  #     first_age_pop = args$first_age_pop, last_age_pop = args$last_age_pop,
  #     population_midyear_male = args$population_midyear_male, population_midyear_female = args$population_midyear_female,
  #     year_of_analysis = args$year_of_analysis,
  #     min_age = args$min_age, max_age = args$max_age,
  #     dw_central = args$dw_central, dw_lower = args$dw_lower, dw_upper = args$dw_upper,
  #     duration_central = args$duration_central, duration_lower = args$duration_lower, duration_upper = args$duration_upper,
  #     geo_id_disaggregated = args$geo_id_disaggregated , geo_id_aggregated = args$geo_id_aggregated,
  #     info = args$info)
  # }
  #
  # output_sim <-
  #   purrr::map(input_args_for_all_sim, call_attribute_master)

  # Deactivated: This code is shorter than the code above
  # but it is slightly slower.
  # According to profvis: 1400 vs. 1280 ms for 100 simulations
  output_sim <-
    purrr::map(
      input_args_for_all_sim,
      \(.x) base::do.call(healthiar:::attribute_master, args = .x ))

  impact_main <- purrr::map(output_sim,"health_main")


  attribute_by_sim <-
    # Add columns (one row for each assessment)
    # input_args
    dplyr::mutate(template_with_sim,
                  input = input_args_for_all_sim,
                  output = output_sim,
                  impact_main = impact_main)


  # Identify the geo_id (aggregated or disaggregated) that is present in health_main
  # (to be used below)
  grouping_geo_var <-
    names(output_attribute$health_main)[grepl("geo_id", names(output_attribute$health_main))]


  # Summarize results getting the central, lower and upper estimate
  summary <-
    attribute_by_sim |>
    dplyr::select(dplyr::all_of(c("sim_id", "impact_main"))) |>
    tidyr::unnest(cols = impact_main) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grouping_geo_var))) |>
    dplyr::summarise(
      central_estimate = stats::quantile(x = impact, probs = c(0.5), na.rm = TRUE),
      lower_estimate = stats::quantile(x = impact, probs = c(0.025), na.rm = TRUE),
      upper_estimate = stats::quantile(x = impact, probs = c(0.975), na.rm = TRUE),
      .groups = "drop")

  # Store the results in a list keeping consistency in the structure with
  # other healthiar functions
  uncertainty <-
    list(
      uncertainty_main = summary,
      uncertainty_detailed = list(by_simulation = attribute_by_sim))

  return(uncertainty)
    }

  # GENERATE RESULTS USING THE FUNCTION ##################

  ## One case (no comparison) ####

  # Identify if this is one-case or two-case (comparison) assessment
  is_two_cases <- any(c("input_table_1", "input_table_2") %in% names(input_table))
  is_one_case <- !is_two_cases

  if(is_one_case){
    # Use summarize_uncertainty_based_on_input() only once
    uncertainty <-
      summarize_uncertainty_based_on_input(
        input_args = input_args,
        input_table = input_table)

  ## Two cases (comparison) ####
  } else if (is_two_cases){

    # Use summarize_uncertainty_based_on_input() twice:

    # Once for the scenario 1
    attribute_1 <-
      summarize_uncertainty_based_on_input(
        input_args = input_args[["input_args_1"]],
        input_table = input_table[["input_table_1"]])
    # Once for the scenario 2
    attribute_2 <-
      summarize_uncertainty_based_on_input(
        input_args = input_args[["input_args_2"]],
        input_table = input_table[["input_table_2"]])

    # Extract output 1 and 2
    output_1 <-
      attribute_1[["uncertainty_detailed"]][["by_simulation"]]|>
      dplyr::select(dplyr::contains(c("_id", "output")))

    output_2 <-
      attribute_2[["uncertainty_detailed"]][["by_simulation"]]|>
      dplyr::select(dplyr::contains(c("_id", "output")))

    # Identify the id columns of the outputs (to be used below)
    id_cols <-
      # We use output 1 but we could use output 2
      # (same structure because same type of assessment)
      output_1 |>
      dplyr::select(dplyr::contains("_id"))

    # Put ouputs together in one single tibble and run compare across rows
    attribute_by_sim <-
      id_cols |>
      dplyr::mutate(
        output_1 = output_1$output,
        output_2 = output_2$output)|>
      dplyr::mutate(
        output_compare =
          purrr::pmap(base::list(output_1, output_2, input_args$approach_comparison),
                      healthiar::compare),
        impact_main = purrr::map(output_compare,"health_main"))

    # Identify the geo_ids used in health_main (to be used below)
    grouping_geo_var <-
      names(output_attribute$health_main)[grepl("geo_id", names(output_attribute$health_main))]

    # Summarize results getting the central, lower and upper estimate
    summary <-
      attribute_by_sim |>
      dplyr::select(dplyr::all_of(c("sim_id", "impact_main"))) |>
      tidyr::unnest(cols = impact_main) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_geo_var))) |>
      dplyr::summarise(
        central_estimate = stats::quantile(x = impact, probs = c(0.5), na.rm = TRUE),
        lower_estimate = stats::quantile(x = impact, probs = c(0.025), na.rm = TRUE),
        upper_estimate = stats::quantile(x = impact, probs = c(0.975), na.rm = TRUE),
        .groups = "drop")

    # Store the results in a list keeping consistency in the structure with
    # other healthiar functions
    uncertainty <-
      list(
        uncertainty_main = summary,
        uncertainty_detailed = list(by_simulation = attribute_by_sim))

  }

  # RETURN ####################################################################

  on.exit(options(user_options))

  return(uncertainty)

}

