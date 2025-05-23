#' Get Monte Carlo confidence intervals

#' @description
#' This function determines summary uncertainty (based on at least one variable
#' with central, lower and upper estimate) based on attribute() or compare()
#' function output.

#' @param results \code{variable} in which the results of an attribute function is stored.
#' @param n_sim \code{numeric value} indicating the number of simulations to be performed.
#' @param seed \code{numeric value} fixing the randomization. If empty, a fix default value is assigned.

#' @returns
#' This function returns confidence intervals for the attributable health impacts using a Monte Carlo simulation.

#' @author Axel Luyten

#' @examples
#' TODO

#' @export

#' @keywords internal


summarize_uncertainty <- function(
    results,
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
  input_args <- results[["health_detailed"]][["input_args"]]
  input_table <- results[["health_detailed"]][["input_table"]]



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
  simulate <- function(var_name, distribution, n, seed){

    var_value_central <- input_args[[paste0(var_name, "_central")]]
    var_value_lower <- input_args[[paste0(var_name, "_lower")]]
    var_value_upper <- input_args[[paste0(var_name, "_upper")]]

    if(distribution == "gamma"){
      base::set.seed(seed)
      simulation <-
        #abs() because negative values have to be avoided
        base::abs(
          sim_gamma(
            n_sim = n,
            central_estimate = var_value_central,
            lower_estimate = var_value_lower,
            upper_estimate = var_value_upper))
    } else if (distribution == "normal"){

        base::set.seed(seed)
        simulation <-
          #abs() because negative values have to be avoided
          base::abs(
            stats::rnorm(
              n = n,
              mean = base::unlist(var_value_central),
              sd = (base::unlist(var_value_upper) - base::unlist(var_value_lower)) / (2 * qnorm(0.975))))

    } else if (distribution == "beta") {

      base::set.seed(seed)
      simulation_betaExpert <-
        betaExpert(
          best = var_value_central,
          lower = var_value_lower,
          upper = var_value_upper,
          method = "mean")

      base::set.seed(seed)
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
    if (ci_in[[var]]) {
      # Store distribution
      dist <- sim_config[[var]]
      # Store column name
      col_name <- paste0(var, "_central")

      # Run simulate across all rows
      # (iteration across simulations, geo_units, exp categories...)
      sim[[col_name]] <- purrr::map(
        .x = sim_template$geo_id_number,
        ~ simulate(
          var_name = var,
          distribution = dist,
          n = n_sim,
          # Different seed for each geo_unit to avoid similar results across geo_units
          seed = seeds[[var]] + .x
        )
      )
    }
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

  template_with_sim <-
    # Bind the template with the simulated values
    dplyr::bind_cols(sim_template, tibble::as_tibble(sim)) |>
    # Unnest to have table layout
    tidyr::unnest(dplyr::any_of(cols_to_unnest))

  attribute_by_sim <-
    # Add columns (one row for each assessment)
    # input_args
    dplyr::mutate(template_with_sim,
      input_args =
        purrr::pmap(template_with_sim,
                    \(...) {c(input_args_prepared_for_replacement, base::list(...))})) |>
    # simulation id
    dplyr::mutate(
      sim_id = dplyr::row_number(), .before = dplyr::everything())|>
    # input_table, impact_raw and output (as in attribute_master)
    dplyr::mutate(
      input_table =
        purrr::map(input_args, healthiar:::compile_input),
      impact_raw =
        purrr::map2(input_table, "paf", healthiar:::get_impact),
      output =
        purrr::pmap(base::list(input_args,input_table, impact_raw), healthiar:::get_output),
      # Extract impact main to use these data in a easier way below
      impact_main =
        purrr::map(output,"health_main"))

  # Identify the geo_id (aggregated or disaggregated) that is present in health_main
  # (to be used below)
  grouping_geo_var <-
    names(results$health_main)[grepl("geo_id", names(results$health_main))]

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
      names(results$health_main)[grepl("geo_id", names(results$health_main))]

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

