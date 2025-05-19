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

  ## Set options
  user_options <- options()
  # Make sure that no rounding occurs
  options(digits = 15)



  ## Set seed for reproducibility
  if(base::is.null(seed)){seed <- 123}

  var_names <- c("rr", "exp", "cutoff", "bhd", "dw", "duration")
  seeds <- list()

  # Store seed
  for(i in 1:length(var_names)){
    seeds[[var_names[i]]] <- seed #+i*1E
  }


  # Store the input data as entered in the arguments
  input_args <- results[["health_detailed"]][["input_args"]]
  input_table <- results[["health_detailed"]][["input_table"]]

  # Identify if this is one-case or two-case (comparison) assessment
  is_two_cases <- any(c("input_table_1", "input_table_2") %in% names(input_table))
  is_one_case <- !is_two_cases


  # CREATE FUNCTION TO SUMMARIZE UNCERTAINTY ######
  summarize_uncertainty_based_on_input <-
    function(input_args, input_table){
  ## Determine number of geographic units
  n_geo <-
    # Exceptionally, let's use here unique() instead of input_args
    # because in some cases the users do not enter the geo_id.
    # In that cases compile_input() provide a geo_id and it is shown in impact_raw
    base::length(base::unique(input_table$geo_id_disaggregated))

  # Sequence (vector) of exposure_dimension
  # Use impact_raw because it was obtained in compiled_input

  n_exp <- base::max(input_table$exposure_dimension)
  seq_exposure_dimension <- 1:n_exp

  # Total number of iterations
  n_total_it = n_sim * n_geo * n_exp

  # Number of assessments (i.e. condensing exposure distributions)
  n_ass <- n_sim * n_geo

  ass_id <- base::as.numeric(base::rep(1:n_ass, each = n_exp))

  # Store boolean variables

  # Is population-weighted mean exposure?
  is_pwm_exposure <-
    base::unique(input_table$exposure_type == "population_weighted_mean")
  # Is categorical?
  is_categorical_exposure <-
    ! is_pwm_exposure

  # Is there a confidence interval? I.e. lower and upper estimate?

  ci_in <- list()

  for (v in var_names){
    ci_in[[v]] <-
      !base::is.null(input_args[[paste0(v, "_lower")]]) &&
      !base::is.null(input_args[[paste0(v, "_upper")]])
  }


  value_in_dw_central <-
    !base::is.null(input_args$dw_central)

  value_in_erf_eq_central <-
    !base::is.null(input_args$erf_eq_central)




  # Define functions #################################################
  # betaExpert()
  ## Copied from source code of prevalence::betaExpert() here:
  ### https://github.com/cran/prevalence/blob/master/R/betaExpert.R

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

  ## Define helper functions for fitting a gamma distribution with optimization
  ## for the relative risk.
  ### NOTE: the functions were adapted from those provided by Sciensano

  ## Set gamma distribution specs
  vector_probabilities <- c(0.025, 0.975)
  par <- 2 ## shape parameter of the gamma distribution

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


  # Create template and run simulations #####################


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
              sd = (base::unlist(var_value_upper) - base::unlist(var_value_lower)) / (2 * 1.96)))

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



  geo_id_disaggregated <- base::unique(input_table$geo_id_disaggregated)

  ci_in_template <-
    tibble::tibble(
      geo_id_disaggregated = geo_id_disaggregated,
      geo_id_number = 1:n_geo,
      sim_id = list(1:n_sim))

  sim <- ci_in_template


  if(ci_in[["rr"]]){


    sim <-
      sim |>
      dplyr::mutate(
        rr_central =
          purrr::map(
            .x = geo_id_number,
            ~ simulate(var_name = "rr",
                     distribution = "gamma",
                     n = n_sim,
                     seed = seeds[["rr"]] + .x)))
      #
      # dplyr::rowwise() |>
      # dplyr::mutate(
      #   rr_central =
      #     base::list(simulate(var_name = "rr",
      #                         distribution = "gamma",
      #                         n = n_sim,
      #                         seed = seeds[["rr"]]+geo_id_number)))
  }

  if(ci_in[["exp"]]){

    sim <-
      sim |>
      dplyr::rowwise() |>
      dplyr::mutate(
        exp_central =
          base::list(simulate(var_name = "exp",
                              distribution = "normal",
                              n = n_sim,
                              seed = seeds[["exp"]]+geo_id_number)))
  }

  if(ci_in[["cutoff"]]){

    sim <-
      sim |>
      dplyr::mutate(
        cutoff_central =
          purrr::map(
            .x = geo_id_number,
            ~ simulate(var_name = "cutoff",
                       distribution = "normal",
                       n = n_sim,
                       seed = seeds[["cutoff"]] + .x)))

    # sim <-
    #   sim |>
    #   dplyr::rowwise() |>
    #   dplyr::mutate(
    #     cutoff_central =
    #       base::list(simulate(var_name = "cutoff",
    #                           distribution = "normal",
    #                           n = n_sim,
    #                           seed = seeds[["cutoff"]]+geo_id_number)))
  }

  if(ci_in[["bhd"]]){

    sim <-
      sim |>
      dplyr::mutate(
        bhd_central =
          purrr::map(
            .x = geo_id_number,
            ~ simulate(var_name = "bhd",
                       distribution = "normal",
                       n = n_sim,
                       seed = seeds[["bhd"]] + .x)))

    # sim <-
    #   sim |>
    #   dplyr::rowwise() |>
    #   dplyr::mutate(
    #     bhd_central =
    #       base::list(simulate(var_name = "bhd",
    #                           distribution = "normal",
    #                           n = n_sim,
    #                           seed = seeds[["bhd"]]+geo_id_number)))
  }

  if(ci_in[["dw"]]){

    sim <-
      sim |>
      dplyr::mutate(
        dw_central =
          purrr::map(
            .x = geo_id_number,
            ~ simulate(var_name = "dw",
                       distribution = "beta",
                       n = n_sim,
                       seed = seeds[["dw"]] + .x)))

    # sim <-
    #   sim |>
    #   dplyr::rowwise() |>
    #   dplyr::mutate(
    #     dw_central =
    #       base::list(simulate(var_name = "dw",
    #                           distribution = "beta",
    #                           n = n_sim,
    #                           seed = seeds[["dw"]]+geo_id_number)))
}

  if(ci_in[["duration"]]){

    sim <-
      sim |>
      dplyr::mutate(
        duration_central =
          purrr::map(
            .x = geo_id_number,
            ~ simulate(var_name = "duration",
                       distribution = "normal",
                       n = n_sim,
                       seed = seeds[["duration"]] + .x)))

    # sim <-
    #   sim |>
    #   dplyr::rowwise() |>
    #   dplyr::mutate(
    #     duration_central =
    #       base::list(simulate(var_name = "duration",
    #                           distribution = "normal",
    #                           n = n_sim,
    #                           seed = seeds[["duration"]]+geo_id_number)))
  }

  if((!base::is.null(input_args$erf_eq_lower) |
      !base::is.null(input_args$erf_eq_lower))){
    base::stop("Sorry, the summary of uncertainty for erf_eq is not currently supported",
               call. = FALSE)
  }


  var_names_in_sim <- base::names(ci_in)[unlist(ci_in)]
  var_names_in_sim_central <- base::paste0(var_names_in_sim, "_central")
  cols_to_unnest <- c("sim_id",
                      var_names_in_sim_central)

  vars_to_be_removed_in_input_args <-
    base::paste0(
      base::rep(var_names_in_sim, each = 3),
      c("_central", "_lower", "_upper"))

  input_args_prepared_for_replacement <-
    purrr::keep(input_args,
                !names(input_args) %in% vars_to_be_removed_in_input_args)

  template <- tidyr::unnest(sim, dplyr::any_of(cols_to_unnest))

  attribute_by_sim <-
    dplyr::mutate(template,
      input_args =
        purrr::pmap(template,
                    \(...) {c(input_args_prepared_for_replacement, base::list(...))})) |>
    dplyr::mutate(
      sim_id = dplyr::row_number(), .before = dplyr::everything())|>
    dplyr::mutate(
      input_table =
        purrr::map(input_args, healthiar:::compile_input),
      impact_raw =
        purrr::map2(input_table, "paf", healthiar:::get_impact),
      output =
        purrr::pmap(base::list(input_args,input_table, impact_raw), healthiar:::get_output),
      impact_main =
        purrr::map(output,"health_main"))

  grouping_geo_var <-
    names(results$health_main)[grepl("geo_id", names(results$health_main))]

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

  uncertainty <-
    list(
      uncertainty_main = summary,
      uncertainty_detailed = list(by_simulation = attribute_by_sim))

  return(uncertainty)
    }

  # GENERATE RESULTS USING THE FUNCTION ##################
  # One case (no comparison)

  if(is_one_case){
    uncertainty <-
      summarize_uncertainty_based_on_input(
        input_args = input_args,
        input_table = input_table)
  } else if (is_two_cases){

    # Simulate and summarize data for the scenario 1 and 2

    attribute_1 <-
      summarize_uncertainty_based_on_input(
        input_args = input_args[["input_args_1"]],
        input_table = input_table[["input_table_1"]])

    attribute_2 <-
      summarize_uncertainty_based_on_input(
        input_args = input_args[["input_args_2"]],
        input_table = input_table[["input_table_2"]])

    output_1 <-
      attribute_1[["uncertainty_detailed"]][["by_simulation"]]|>
      dplyr::select(dplyr::contains(c("_id", "output")))


    output_2 <-
      attribute_2[["uncertainty_detailed"]][["by_simulation"]]|>
      dplyr::select(dplyr::contains(c("_id", "output")))

    id_cols <-
      output_1 |>
      dplyr::select(dplyr::contains("_id"))



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

    grouping_geo_var <-
      names(results$health_main)[grepl("geo_id", names(results$health_main))]

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

    uncertainty <-
      list(
        uncertainty_main = summary,
        uncertainty_detailed = list(by_simulation = attribute_by_sim))






  }





  # OUTPUT ####################################################################

  on.exit(options(user_options))

  return(uncertainty)


}

