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

#' @author Alberto Castro & Axel Luyten

#' @examples
#' # Goal: obtain summary uncertainty for an existing attribute_health()
#' # output
#'
#' ## First create an assessment
#' attribute_health_output <- attribute_health(
#'   erf_shape = "log_linear",
#'   rr_central = 1.369,
#'   rr_lower = 1.124,
#'   rr_upper = 1.664,
#'   rr_increment = 10,
#'   exp_central = 8.85,
#'   exp_lower = 8,
#'   exp_upper = 10,
#'   cutoff_central = 5,
#'   bhd_central = 30747,
#'   bhd_lower = 28000,
#'   bhd_upper = 32000
#' )
#'
#' ## Then run Monte Carlo simulation
#' results <- summarize_uncertainty(
#'   output_attribute = attribute_health_output,
#'   n_sim = 100
#' )
#'
#' results$uncertainty_main$impact # Central, lower and upper estimates

#' @export


summarize_uncertainty <- function(
    output_attribute,
    n_sim,
    seed = NULL) {

  # PREPARATION ################

  ## Decimals ######

  ## Set options
  user_options <- base::options()
  # Make sure that no rounding occurs
  base::options(digits = 15)


  ## Seeds ########
  ## Set seed for reproducibility
  if(base::is.null(seed)){seed <- 123}

  var_names <- c("rr", "exp", "cutoff", "bhd", "dw", "duration")
  seeds <- base::list()

  # Store seed
  for(i in 1:base::length(var_names)){
    seeds[[var_names[i]]] <- seed +i*1E3
  }

  ## Data sets ##########
  # Store the input data as entered in the arguments
  input_args <- output_attribute[["health_detailed"]][["input_args"]]
  input_table <- output_attribute[["health_detailed"]][["input_table"]]



  # DATA VALIDATION ####
  # Uncertainty in erf_eq is currently not supported
  # It would require a more complex modelling
  if((!base::is.null(input_args$erf_eq_lower) |
      !base::is.null(input_args$erf_eq_lower))){
    base::stop("Sorry, the summary of uncertainty for erf_eq_... is not currently supported",
               call. = FALSE)
  }

  if(# If exposure distribution
    base::unique(output_attribute$health_detailed$impact_disaggregated$exposure_type) == "exposure_distribution" &&
    # If uncertainty in exposure
    (!base::is.null(input_args$exp_lower) |
      !base::is.null(input_args$exp_upper))){
    base::stop("Sorry, the summary of uncertainty for exp_... in exposure distributions is not currently supported",
               call. = FALSE)
  }




  # FUNCTION TO SUMMARIZE UNCERTAINTY ######
  # Create sub-functions to be used inside summarize_uncertainty_based_on_input (see below)
  # and in compare() out that function

  # Get results of simulations organized by geo unit
  get_attribute_by_geo <- function(attribute_by_sim, geo_id){

    if(geo_id == "geo_id_disaggregated"){

      columns_to_unnest <- attribute_by_sim |>
      dplyr::select(dplyr::contains(c("geo_id", "_central", "impact"))) |>
      base::names()

    }else if(geo_id == "geo_id_aggregated"){

      columns_to_unnest <- attribute_by_sim |>
        dplyr::select(dplyr::contains(c("geo_id_aggregated", "impact"))) |>
        base::names()

    }

    attribute_by_geo <- attribute_by_sim |>
      # Remove super-detailed information by simulation
      # any_of() because input is not available in compare
      # (otherwise too large output and slow)
      dplyr::select( !dplyr::contains(c("input", "output")))|>
      # Unnest to have the information per" row
      tidyr::unnest(cols = dplyr::all_of(columns_to_unnest)) |>
      # Keep only unique rows
      # If geo_id_aggregated avaialable,
      # then geo_ and aggregated impact have the same dimension as geo_id_disaggregated
      # and this creates duplicates
      base::unique() |>
      # Put column geo_id first because it is now the sort criteria
      dplyr::select(dplyr::all_of(geo_id), dplyr::everything()) |>
      # Sort rows by geo_id
      dplyr::arrange(dplyr::across(dplyr::all_of(geo_id)))
  }

  # Get uncertainty
  get_summary <- function(attribute, grouping_var){

    summary <-
      attribute |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_var))) |>
      dplyr::summarise(
        central_estimate = stats::quantile(x = impact, probs = c(0.5), na.rm = TRUE, names = FALSE),
        lower_estimate = stats::quantile(x = impact, probs = c(0.025), na.rm = TRUE, names = FALSE),
        upper_estimate = stats::quantile(x = impact, probs = c(0.975), na.rm = TRUE, names = FALSE),
        .groups = "drop") |>
      # Change to same format as other output from healthiar
      tidyr::pivot_longer(
        # data = summary,
        cols = !dplyr::contains("geo_id"),
        names_to = "impact_ci", values_to = "impact"
      ) |>
      dplyr::mutate(
        impact_rounded = base::round(impact, digits = 0)
      )
  }


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
    # In that cases compile_input() provide a geo_id and it is shown in results_raw
    base::length(base::unique(input_table$geo_id_disaggregated))

  # If exposure dimension is implemented:
  # Get the dimension of the exposure
  # (i.e. if pop-weighted mean => 1, if exposure distribution => >1 )
  # n_exp <- base::max(output_attribute$health_detailed$impact_disaggregated$exposure_dimension)


  ## Boolean variables ####

  # Is there a confidence interval? I.e. lower and upper estimate?

  ci_in <- list()

  for (v in var_names){
    ci_in[[v]] <-
      !base::is.null(input_args[[base::paste0(v, "_lower")]]) &&
      !base::is.null(input_args[[base::paste0(v, "_upper")]])
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
  sim_template <- input_table |>
  dplyr::select(geo_id_disaggregated) |>
  # If exposure dimension is implemented: dplyr::select(geo_id_disaggregated, exposure_dimension) |>
  base::unique()|>
  dplyr::mutate(geo_id_number = 1:n_geo, .after = geo_id_disaggregated) |>
  dplyr::mutate(sim_id = base::list(1:n_sim))

  # Identify the variable names with confidence interval
  var_names_with_ci <- base::names(ci_in)[unlist(ci_in)]
  # Identify the central variable names with confidence interval
  var_names_with_ci_central <- base::paste0(var_names_with_ci, "_central")
  # Identify those var_names_with_ci that have simulated values different in all geo units
  var_names_with_ci_geo_different <- var_names_with_ci[var_names_with_ci %in% c("exp", "bhd")]

  if(base::length(var_names_with_ci_geo_different) >= 1){
    var_names_with_ci_geo_different_central <-
      base::paste0(var_names_with_ci_geo_different, "_central")
  }

  # And now identical
  var_names_with_ci_geo_identical <- var_names_with_ci[var_names_with_ci %in% c("rr", "cutoff", "dw", "duration")]

  if(base::length(var_names_with_ci_geo_identical) >= 1){
    var_names_with_ci_geo_identical_central <-
      base::paste0(var_names_with_ci_geo_identical, "_central")
  }



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
  sim <- base::list()


  # Apply simulation for variables that have confidence interval
  for (var in var_names_with_ci) {

    # Store distribution
    dist <- sim_config[[var]]
    # Store column name
    col_name <- base::paste0(var, "_central")

    # Store central, lower and upper estimate for the simulation below
    central <- base::as.numeric(input_args[[base::paste0(var, "_central")]])
    lower   <- base::as.numeric(input_args[[base::paste0(var, "_lower")]])
    upper   <- base::as.numeric(input_args[[base::paste0(var, "_upper")]])


    # Run simulate across all rows

    # First those variable that are different for all geo units (exp and bhd)
    # Simulations must be DIFFERENT  in all geo units
    if(var %in% var_names_with_ci_geo_different){

      sim[[col_name]] <- purrr::pmap(
        base::list(sim_template$geo_id_number),
        function(geo_id_number) {
        # If exposure dimension is implemented
        #base::list(sim_template$geo_id_number, sim_template$exposure_dimension),
        #function(geo_id_number, exposure_dimension) {
          simulate(
          central = central,
          lower = lower,
          upper = upper,
          distribution = dist,
          n = n_sim,
          # Different seed for each geo_unit to avoid similar results across geo_units
          # Minus 1 to keep the same seed as exposure dimension = 1
          seed = seeds[[var]] + geo_id_number)}
         # If exposure dimension is implemented
         # seed = seeds[[var]] + geo_id_number + base::as.integer(exposure_dimension)-1)}
      )

      # Second for those variable that are common for all geo units (rr, cutoff, dw and duration)
      # The simulated value must be IDENTICAL in all geo units
      # Not across geo_id but across sim_id
    } else if (var %in% var_names_with_ci_geo_identical ){

      sim[[col_name]] <-
        base::list(
          simulate(
          central = central,
          lower = lower,
          upper = upper,
          distribution = dist,
          n = n_sim,
          # Different seed for each geo_unit to avoid similar results across geo_units
          seed = seeds[[var]]))

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
                !base::names(input_args) %in% vars_to_be_removed_in_input_args)

  template_with_sim <-
    # Bind the template with the simulated values
    dplyr::bind_cols(sim_template, tibble::as_tibble(sim[var_names_with_ci_central])) |>
    # Unnest to have table layout
    tidyr::unnest(dplyr::any_of(c("sim_id",
                                  var_names_with_ci_central)))

  geo_ids <-
    base::names(template_with_sim)[base::names(template_with_sim) %in% c("geo_id_aggregated", "geo_id_disaggregated")]

  template_with_sim_grouped <-
    template_with_sim |>
    dplyr::group_by(sim_id) |>
    dplyr::summarize(
      # Pack in lists the values that are different in geo unit (as input_args)
      dplyr::across(dplyr::all_of(c("geo_id_disaggregated",
                                    var_names_with_ci_central)),
                    ~ base::list(.x)))

  if( base::length(var_names_with_ci_geo_identical) >= 1 ){
    template_with_sim_grouped <-  template_with_sim_grouped |>
      # Keep only unique values because they identical for all geo_id_disaggregated
      dplyr::mutate(dplyr::across(dplyr::all_of(var_names_with_ci_geo_identical_central),
                                  ~ purrr::map(.x, base::unique)))

  }

  only_new_values_for_replacement <-
    dplyr::select(template_with_sim_grouped,
                  -dplyr::any_of(c("sim_id", "geo_id_disaggregated")))
                                   #geo_ids)))

  # Replace the values
  input_args_for_all_sim <-
    purrr::pmap(only_new_values_for_replacement,
                \(...) {c(input_args_prepared_for_replacement, base::list(...))})

  output_sim <-
    purrr::map(
      input_args_for_all_sim,
      \(.x) base::do.call(healthiar:::attribute_master, args = .x ))

  # Extract impact
  impact_disaggregated <- purrr::map(
    output_sim,
    \(x) x$health_detailed$impact_disaggregated$impact
  )

  # Extract geo_id_aggregated already with the right format to be added below
  # Avoid warning if geo_id_aggregated is NULL with the if statement
  if(is.null(input_args[["geo_id_aggregated"]])){
    geo_id_aggregated <- NULL
  } else {
    geo_id_aggregated <- purrr::map(output_sim,
    \(x) x$health_detailed$impact_disaggregated$geo_id_aggregated
  )}


  # Create a tibble with the input, output and impact
  attribute_by_sim_disaggregated <-
    # Add columns (one row for each assessment)
    dplyr::mutate(template_with_sim_grouped,
                  geo_id_aggregated = geo_id_aggregated,
                  input = input_args_for_all_sim,
                  output = output_sim,
                  impact = impact_disaggregated)

  # Obtain results of simulations organized by geo unit
  attribute_by_geo_id_disaggregated <-
    get_attribute_by_geo(attribute_by_sim = attribute_by_sim_disaggregated,
                         geo_id = "geo_id_disaggregated")

  # Get summary (uncertainty) for each geo_id_disaggregated
  summary_by_geo_id_disaggregated <-
    get_summary(attribute = attribute_by_geo_id_disaggregated,
                grouping_var = "geo_id_disaggregated")


  if( !"geo_id_aggregated" %in% names(output_attribute$health_main) ){

    attribute_by_sim <- attribute_by_sim_disaggregated

    summary <- summary_by_geo_id_disaggregated

  } else {
    # If there is geo_id_aggregated,
    # export these attribute_by_sim and summary

    # Create a tibble with the input, output health_main and impact
    attribute_by_sim <- attribute_by_sim_disaggregated |>
      # Modify geo_id_aggregated and impact column to the right format
      # i.e. by geo_id_aggregated (less rows) and not by geo_id_disaggregated (more rows)
      dplyr::mutate(
        geo_id_aggregated = purrr::map(
          output_sim,
          \(x) x$health_main$geo_id_aggregated),
        impact = purrr::map(
          output_sim,
          \(x) x$health_main$impact)
        )

    # Obtain results of simulations organized by geo unit

    attribute_by_geo_id_aggregated <-
      get_attribute_by_geo(attribute_by_sim = attribute_by_sim,
                           geo_id = "geo_id_aggregated")


    # Summarize results getting the central, lower and upper estimate
    summary <- get_summary(attribute = attribute_by_geo_id_aggregated,
                           grouping_var = "geo_id_aggregated")


  }


  # Store the results in a list keeping consistency in the structure with
  # other healthiar functions
  uncertainty <-
    base::list(
      uncertainty_main = summary,
      uncertainty_detailed =
        base::list(attribute_by_sim = attribute_by_sim,
                   attribute_by_sim_disaggregated = attribute_by_sim_disaggregated,
                   attribute_by_geo_id_disaggregated = attribute_by_geo_id_disaggregated,
                   uncertainty_by_geo_id_disaggregated = summary_by_geo_id_disaggregated))

  return(uncertainty)
    }

  # GENERATE RESULTS USING THE FUNCTION ##################

  ## One case (no comparison) ####

  # Identify if this is one-case or two-case (comparison) assessment
  is_two_cases <- base::any(c("input_table_1", "input_table_2") %in% base::names(input_table))
  is_one_case <- !is_two_cases


  if(is_one_case){
    # Use summarize_uncertainty_based_on_input() only once
    uncertainty <-
      c(output_attribute,
        summarize_uncertainty_based_on_input(
          input_args = input_args,
          input_table = input_table))

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
      attribute_1[["uncertainty_detailed"]][["attribute_by_sim_disaggregated"]]|>
      dplyr::select(dplyr::contains(c("_id", "output")))

    output_2 <-
      attribute_2[["uncertainty_detailed"]][["attribute_by_sim_disaggregated"]]|>
      dplyr::select(dplyr::contains(c("_id", "output")))


    # Extract simulation values 1 and 2
    id_cols_and_sim_1 <-
      attribute_1[["uncertainty_detailed"]][["attribute_by_sim_disaggregated"]] |>
      dplyr::select(
        !dplyr::all_of(c("input", "output", "impact")))

    id_cols_and_sim_2 <-
      attribute_2[["uncertainty_detailed"]][["attribute_by_sim_disaggregated"]] |>
      dplyr::select(
        !dplyr::all_of(c("input", "output", "impact")))


    # Identify the id columns of the outputs (to be used below)
    id_cols <-
      # We use output 1 but we could use output 2
      # (same structure because same type of assessment)
      output_1 |>
      dplyr::select(dplyr::contains("_id")) |>
      base::names()

    # Put together ids and sim cols
    id_cols_and_sim <-
      dplyr::left_join(id_cols_and_sim_1,
                       id_cols_and_sim_2,
                       by = id_cols,
                       suffix = c("_1", "_2"))

    # Put outputs together in one single tibble and run compare across rows
    attribute_by_sim_disaggregated <-
      id_cols_and_sim |>
      dplyr::mutate(
        output_1 = output_1$output,
        output_2 = output_2$output)|>
      dplyr::mutate(
        output_compare =
          purrr::pmap(base::list(output_1, output_2, input_args$approach_comparison),
                      healthiar::compare),
        impact = purrr::map(output_compare,
                            \(x) x$health_detailed$impact_disaggregated$impact)
        )

    # Obtain results of simulations organized by geo unit
    attribute_by_geo_id_disaggregated <-
      get_attribute_by_geo(attribute_by_sim = attribute_by_sim_disaggregated,
                           geo_id = "geo_id_disaggregated")

    # Get summary (uncertainty) for each geo_id_disaggregated
    summary_by_geo_id_disaggregated <-
      get_summary(attribute = attribute_by_geo_id_disaggregated,
                  grouping_var = "geo_id_disaggregated")


    if(! "geo_id_aggregated" %in% names(output_attribute$health_main)){
      attribute_by_sim <- attribute_by_geo_id_disaggregated
      summary <- summary_by_geo_id_disaggregated

    } else{

      attribute_by_sim <- attribute_by_sim_disaggregated |>
        dplyr::mutate(
          impact = purrr::map(output_compare,
                                          \(x) x$health_main$impact)
          )

      # Obtain results of simulations organized by geo unit
      attribute_by_geo_id_aggregated <-
        get_attribute_by_geo(attribute_by_sim = attribute_by_sim,
                             geo_id = "geo_id_aggregated")


      # Get summary (uncertainty) for each geo_id_disaggregated
      summary <- get_summary(attribute = attribute_by_geo_id_aggregated,
                                    grouping_var = "geo_id_aggregated")
    }


    # Store the results in a list keeping consistency in the structure with
    # other healthiar functions

    uncertainty <-
      c(output_attribute,
        base::list(
          uncertainty_main = summary,
          uncertainty_detailed =
            base::list(attribute_by_sim = attribute_by_sim,
                       attribute_by_sim_disaggregated = attribute_by_sim_disaggregated,
                       attribute_by_geo_id_disaggregated = attribute_by_geo_id_disaggregated,
                       uncertainty_by_geo_id_disaggregated = summary_by_geo_id_disaggregated)))

  }

  # RETURN ####################################################################

  on.exit(options(user_options))

  return(uncertainty)

}

