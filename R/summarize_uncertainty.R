#' Get Monte Carlo confidence intervals

# DESCRIPTION ##################################################################
#' @description
#' This function determines summary uncertainty (based on central, lower and upper estimates of at least one input variable) using attribute() or compare()
#' function output by Monte Carlo simulation.
#' @description
#' Input variables that will be processed are:
#' \itemize{
#'   \item{relative_risk (\code{rr_...})}
#'   \item{exposure (\code{exp_...})}
#'   \item{cutoff (\code{cutoff_...})}
#'   \item{baseline health data (\code{bhd_...})}
#'   \item{disability weight (\code{dw_...})}
#'   \item{duration (\code{duration_...})}
#'   }

# ARGUMENTS ####################################################################
#' @param output_attribute \code{variable} in which the output of a \code{healthiar::attribute_...()} function call are stored.
#' @param n_sim \code{numeric value} indicating the number of simulations to be performed.
#' @param seed \code{numeric value} for fixing the randomization. If empty, 123 is used as a default.

# DETAILS ######################################################################
#' @details
#' \strong{Method}
#' @details
#' For each processed input variable with a provided 95\% confidence interval value, a distribution is fitted (see below). From these, \code{n_sim} input value sets are sampled to compute \code{n_sim} attributable impacts. The median value of these attributable impacts is reported as the central estimate, and the 2.5th and 97.5th percentiles define the lower and upper bounds of the 95\% summary uncertainty confidence interval, respectively. Aggregated central, lower and upper estimates are obtained by summing the corresponding values of each lower level unit.

#' @details
#' \strong{Distributions used for simulation}
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

# VALUE ########################################################################
#' @returns
#' This function returns a summary uncertainty central estimate and correcponding lower and upper confidence intervals for the attributable health impacts by Monte Carlo simulation.

# EXAMPLES #####################################################################
#' @examples
#' # Goal: obtain summary uncertainty for an existing attribute_health() output
#' # First create an assessment
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
#' # Then run Monte Carlo simulation
#' results <- summarize_uncertainty(
#'   output_attribute = attribute_health_output,
#'   n_sim = 100
#' )
#' results$uncertainty_main$impact # Central, lower and upper estimates

#' @author Alberto Castro & Axel Luyten

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

  ## Revant variables ##########
  # Store the input data as entered in the arguments

  input_args <- output_attribute$health_detailed$input_args
  input_table <- output_attribute$health_detailed$input_table

  is_two_cases <- base::any(c("input_table_scen_1", "input_table_scen_2") %in% base::names(input_table))
  is_one_case <- !is_two_cases

  if(is_one_case){
    input_args_to_check <- output_attribute$health_detailed$input_args
    input_table_to_check <- output_attribute$health_detailed$input_table
  } else {
    input_args_to_check <- output_attribute$health_detailed$input_args$input_args_scen_1
    input_table_to_check <- output_attribute$health_detailed$input_table$input_table_scen_1
    #Same as input_args_scen_2 (data validation of compare())
  }



  input_arg_names_passed <- input_args_to_check$is_entered_by_user |>
    purrr::keep(~.x == TRUE) |>
    base::names()

  is_lifetable <- base::unique(input_table_to_check$is_lifetable)
  exp_type <- base::unique(input_table_to_check$exp_type)


  # DATA VALIDATION ####
  ## Error if uncertainty in erf_eq_... ####
  # Uncertainty in erf_eq is currently not supported
  # It would require a more complex modelling
  if((!base::is.null(input_args_to_check$value$erf_eq_lower) |
      !base::is.null(input_args_to_check$value$erf_eq_lower))){
    base::stop("Sorry, the summary of uncertainty for erf_eq_... is not currently supported.",
               call. = FALSE)
  }

  ## Error if exposure distribution and uncertainty in exp_...####
  if(# If exposure distribution
    exp_type == "exposure_distribution" &&
    # If uncertainty in exposure
    (!base::is.null(input_args$value$exp_lower) |
      !base::is.null(input_args$value$exp_upper))){
    base::stop("Sorry, the summary of uncertainty for exp_... in exposure distributions is not currently supported.",
               call. = FALSE)
  }

  ## Error if no argument with uncertainty ####
  if(# No argument used has _lower or _upper)
    ! base::any(base::grepl("_upper|_lower", input_arg_names_passed))){
    base::stop("Please enter an assessment with uncertainty (..._lower and ..._upper) in any argument.",
               call. = FALSE)
  }




  # FUNCTION TO SUMMARIZE UNCERTAINTY ######
  # Create sub-functions to be used inside summarize_uncertainty_based_on_input (see below)
  # and in compare() out that function

  # Get uncertainty
  get_summary <- function(attribute){

    summary <-
      attribute |>
      dplyr::summarise(
        .by = dplyr::any_of(c("geo_id_macro", "geo_id_micro")),
        central_estimate = stats::quantile(x = impact, probs = c(0.5), na.rm = TRUE, names = FALSE),
        lower_estimate = stats::quantile(x = impact, probs = c(0.025), na.rm = TRUE, names = FALSE),
        upper_estimate = stats::quantile(x = impact, probs = c(0.975), na.rm = TRUE, names = FALSE)) |>
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
    # Let's use here unique() and input_table instead of input_args
    # because in some cases the users do not enter the geo_id.
    # In that cases compile_input() provide a geo_id and it is shown in results_raw
    base::length(base::unique(input_table$geo_id_micro))

  ## Boolean variables ####

  # Is there a confidence interval? I.e. lower and upper estimate?

  ci_in <- list()

  for (v in var_names){
    ci_in[[v]] <-
      !base::is.null(input_args$value[[base::paste0(v, "_lower")]]) &&
      !base::is.null(input_args$value[[base::paste0(v, "_upper")]])
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
    dplyr::select(geo_id_micro) |>
    base::unique()|>
    dplyr::mutate(geo_id_number = 1:n_geo) |>
    dplyr::mutate(sim_id = base::list(1:n_sim))

  # Identify the variable names with confidence interval
  var_names_with_ci <- base::names(ci_in)[unlist(ci_in)]
  var_names_with_ci_in_name <- base::gsub("rr", "erf", var_names_with_ci) |> base::paste0("_ci")
  # Identify those var_names_with_ci that have simulated values different in all geo units
  var_names_with_ci_geo_different <- base::intersect(var_names_with_ci, c("exp", "bhd"))
  # And now identical
  var_names_with_ci_geo_identical <- base::intersect(var_names_with_ci,  c("rr", "cutoff", "dw", "duration"))


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

    # Store central, lower and upper estimate for the simulation below
    central <- base::as.numeric(input_args$value[[base::paste0(var, "_central")]])
    lower   <- base::as.numeric(input_args$value[[base::paste0(var, "_lower")]])
    upper   <- base::as.numeric(input_args$value[[base::paste0(var, "_upper")]])


    # Run simulate across all rows

    # First those variable that are different for all geo units (exp and bhd)
    # Simulations must be DIFFERENT  in all geo units
    if(var %in% var_names_with_ci_geo_different){

      sim[[var]] <- purrr::pmap(
        base::list(sim_template$geo_id_number),
        function(geo_id_number) {
          simulate(
          central = central,
          lower = lower,
          upper = upper,
          distribution = dist,
          n = n_sim,
          # Different seed for each geo_unit to avoid similar results across geo_units
          seed = seeds[[var]] + geo_id_number)}
      )

      # Second for those variable that are common for all geo units (rr, cutoff, dw and duration)
      # The simulated value must be IDENTICAL in all geo units
      # Not across geo_id but across sim_id
    } else if (var %in% var_names_with_ci_geo_identical ){

      sim[[var]] <-
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

  # Identify the variables that have to be deleted in input_args
  # (to be used below)
  args_to_be_replaced_by_sim <-
    base::paste0(
      base::rep(var_names_with_ci, each = 3),
      c("_central", "_lower", "_upper"))
  # variable that were not passed by the user
  args_not_passed <-
    base::names(input_args$is_entered_by_user)[!base::unlist(input_args$is_entered_by_user)]
  # All args to be removed
  args_to_be_removed_in_input_args <-
    base::unique(c(args_to_be_replaced_by_sim, args_not_passed))

  # Prepare input_args to accommodate the new simulated values
  # (to be used below)
  input_args_prepared_for_replacement <-
    # Remove the arguments that will be replace with the simulation including upper and lower
    input_args$value[! base::names(input_args$value) %in% args_to_be_removed_in_input_args]


  template_with_sim <-
    # Bind the template with the simulated values
    dplyr::bind_cols(sim_template, tibble::as_tibble(sim[var_names_with_ci])) |>
    # Unnest to have table layout
    tidyr::unnest(dplyr::any_of(c("sim_id", var_names_with_ci)))

  geo_ids <-
    base::intersect(base::names(template_with_sim), c("geo_id_macro", "geo_id_micro"))


  if( base::length(var_names_with_ci_geo_identical) >= 1 ){
    test <-  template_with_sim |>
      # Keep only unique values because they identical for all geo_id_micro
      dplyr::mutate(dplyr::across(dplyr::all_of(var_names_with_ci_geo_identical),
                                  ~ purrr::map(.x, base::unique)))

  }

  only_new_values_for_replacement <-
    dplyr::select(template_with_sim,
                  -dplyr::any_of(c("sim_id", "geo_id_micro")))
                                   #geo_ids)))

  # Replace the values
  input_args_for_attribute <-
    purrr::pmap(only_new_values_for_replacement,
                \(...) {c(input_args_prepared_for_replacement, base::list(...))})



  args_df <- input_table |>
    dplyr::filter(dplyr::if_all(.cols = dplyr::all_of(var_names_with_ci_in_name),
                                .fns = ~ .x == "central")) |>
    dplyr::select(- dplyr::all_of(var_names_with_ci)) |>
    dplyr::inner_join(template_with_sim,
                      by = "geo_id_micro",
                      relationship = "many-to-many") |>
    dplyr::mutate(geo_id_micro = base::paste0(geo_id_micro, "_sim_", sim_id))

  # Call attribute_health once with all arguments vectorized
  output_sim_after_impact <- healthiar:::get_impact(input_table = args_df,
                                                    pop_fraction_type = "paf")

  output_sim <- healthiar:::get_output(results_raw = output_sim_after_impact$results_raw)[["health_detailed"]][["results_by_geo_id_micro"]]

  results_by_geo_id_micro <- output_sim |>
    dplyr::mutate(geo_id_micro = base::gsub("_sim_.*", "", geo_id_micro))

  attribute_by_sim_disaggregated <- results_by_geo_id_micro



  # Obtain results of simulations organized by geo unit
  attribute_by_geo_id_micro <- attribute_by_sim_disaggregated

  # Get summary (uncertainty) for each geo_id_micro
  summary_by_geo_id_micro <-
    get_summary(attribute = attribute_by_geo_id_micro)

  attribute_by_sim <- attribute_by_sim_disaggregated


  if( !"geo_id_macro" %in% names(output_attribute$health_main) ){

    summary <- summary_by_geo_id_micro

  } else {

    summary_by_geo_id_macro <- summary_by_geo_id_micro |>
      # Sum impacts
      dplyr::summarise(impact = base::sum(impact),
                       .by = c("geo_id_macro", "impact_ci")) |>
      # Round
      dplyr::mutate(impact_rounded = round(impact))

    summary <- summary_by_geo_id_macro
  }

  # Store the results in a list keeping consistency in the structure with
  # other healthiar functions
  uncertainty <-
    base::list(
      uncertainty_main = summary,
      uncertainty_detailed =
        base::list(attribute_by_sim = attribute_by_sim,
                   attribute_by_sim_disaggregated = attribute_by_sim_disaggregated,
                   attribute_by_geo_id_micro = attribute_by_geo_id_micro,
                   uncertainty_by_geo_id_micro = summary_by_geo_id_micro))

  return(uncertainty)
    }

  # GENERATE RESULTS USING THE FUNCTION ##################

  ## One case (no comparison) ####

  # Identify if this is one-case or two-case (comparison) assessment



  if(is_one_case){
    # Use summarize_uncertainty_based_on_input() only once
    uncertainty <-
      c(output_attribute,
        summarize_uncertainty_based_on_input(
          input_args = input_args,
          input_table = input_table))

  ## Two cases (comparison) ####
  } else if (is_two_cases){



    # Once for the scenario 1
    attribute_scen_1 <-
      summarize_uncertainty_based_on_input(
        input_args = input_args[["input_args_scen_1"]],
        input_table = input_table[["input_table_scen_1"]])

    # Once for the scenario 2
    attribute_scen_2 <-
      summarize_uncertainty_based_on_input(
        input_args = input_args[["input_args_scen_2"]],
        input_table = input_table[["input_table_scen_2"]])

    # Extract simulation values 1 and 2
    # Extract output 1 and 2
    output_scen_1 <-
      attribute_scen_1[["uncertainty_detailed"]][["attribute_by_sim_disaggregated"]]

    output_scen_2 <-
      attribute_scen_2[["uncertainty_detailed"]][["attribute_by_sim_disaggregated"]]

    scenario_specific_arguments <-
      c("exp_central", "exp_lower", "exp_upper",
        "bhd_central", "bhd_lower", "bhd_upper",
        "population",
        "prop_pop_exp",
        "pop_exp",
        "year_of_analysis",
        "info",
        "impact", "pop_fraction")

    # Identify the id columns of the outputs (to be used below)
    scen_joining_cols <-
      # We use output 1 but we could use output 2
      # (same structure because same type of assessment)
      # base::names(output_scen_1) |>
      # base::grep(pattern = "_id", x = _, value = TRUE)
      healthiar:::find_joining_columns(
        df_1 = output_scen_1,
        df_2 = output_scen_2,
        except = scenario_specific_arguments)

    # Put together ids and sim cols
    output_both_scen <-
      dplyr::left_join(output_scen_1,
                       output_scen_2,
                       by = scen_joining_cols,
                       suffix = c("_scen_1", "_scen_2"))



    if(input_args$approach_comparison == "delta"){

      attribute_by_sim_disaggregated <- output_both_scen |>
        dplyr::mutate(
          impact = impact_scen_1 - impact_scen_2,
          impact_rounded = base::round(impact)
        )

    } else if(input_args$approach_comparison == "pif"){


      output_sim_after_impact <- healthiar:::get_impact(input_table = output_both_scen,
                                                        pop_fraction_type = "pif")

      output_sim <- healthiar:::get_output(results_raw = output_sim_after_impact$results_raw)[["health_detailed"]][["results_by_geo_id_micro"]]

      results_by_geo_id_micro <- output_sim


    }

    # Obtain results of simulations organized by geo unit
  attribute_by_geo_id_micro <- attribute_by_sim_disaggregated


  # Get summary (uncertainty) for each geo_id_micro
  summary_by_geo_id_micro <-
    get_summary(attribute = attribute_by_geo_id_micro)

  attribute_by_sim <- attribute_by_sim_disaggregated


  if( !"geo_id_macro" %in% names(output_attribute$health_main) ){

    summary <- summary_by_geo_id_micro

  } else {

    summary_by_geo_id_macro <- summary_by_geo_id_micro |>
      # Sum impacts
      dplyr::summarise(impact = base::sum(impact),
                       .by = c("geo_id_macro", "impact_ci")) |>
      # Round
      dplyr::mutate(impact_rounded = round(impact))

    summary <- summary_by_geo_id_macro
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
                       attribute_by_geo_id_micro = attribute_by_geo_id_micro,
                       uncertainty_by_geo_id_micro = summary_by_geo_id_micro)))

  }

  # RETURN ####################################################################

  on.exit(options(user_options))

  return(uncertainty)

}

