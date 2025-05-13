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
    seed = NULL
    ) {

  ## Set options
  user_options <- options()
  options(digits = 15) # Make sure that no rounding occurs

  ## Set seed for reproducibility
  if(base::is.null(seed)){seed <- 123}

  var_names <- c("rr", "exp", "cutoff", "bhd", "dw", "duration")
  seeds <- list()

  for(i in 1:length(var_names)){
    seeds[[var_names[i]]] <- seed #+i
  }

  # Store the input data as entered in the arguments
  input_args <- results[["health_detailed"]][["input_args"]]
  input_table <- results[["health_detailed"]][["input_table"]]

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

  # Store seed


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


  simulate <- function(var_name, distribution, n){

    var_value_central <- input_args[[paste0(var_name, "_central")]]
    var_value_lower <- input_args[[paste0(var_name, "_lower")]]
    var_value_upper <- input_args[[paste0(var_name, "_upper")]]

    if(distribution == "gamma"){
      base::set.seed(seeds[[var_name]])
      simulation <-
        #abs() because negative values have to be avoided
        base::abs(
          sim_gamma(
            n_sim = n,
            central_estimate = var_value_central,
            lower_estimate = var_value_lower,
            upper_estimate = var_value_upper))
    } else if (distribution == "normal"){

        base::set.seed(seeds[[var_name]])
        simulation <-
          #abs() because negative values have to be avoided
          base::abs(
            stats::rnorm(
              n = n,
              mean = base::unlist(var_value_central),
              sd = (base::unlist(var_value_upper) - base::unlist(var_value_lower)) / (2 * 1.96)))

    } else if (distribution == "beta") {

      base::set.seed(seeds[[var_name]])
      simulation_betaExpert <-
        betaExpert(
          best = var_value_central,
          lower = var_value_lower,
          upper = var_value_upper,
          method = "mean")

      base::set.seed(seeds[[var_name]])
      simulation <-
        stats::rbeta(
          n = n,
          shape1 = base::as.numeric(base::unname(simulation_betaExpert["alpha"])),
          shape2 = base::as.numeric(base::unname(simulation_betaExpert["beta"])))
    }

  }




  sim <- list()

  if(ci_in[["rr"]]){

    sim[["rr"]] <-
      simulate(var_name = "rr",
               distribution = "gamma",
               n = n_total_it)
  }

  if(ci_in[["exp"]]){

    sim[["exp"]] <-
      simulate(var_name = "exp",
               distribution = "normal",
               n = n_total_it)
  }

  if(ci_in[["cutoff"]]){

    sim[["cutoff"]] <-
      simulate(var_name = "cutoff",
               distribution = "normal",
               n = n_total_it)
  }

  if(ci_in[["bhd"]]){

    sim[["bhd"]] <-
      simulate(var_name = "bhd",
               distribution = "normal",
               n = n_total_it)
  }

  if(ci_in[["dw"]]){

    sim[["dw"]] <-
      simulate(var_name = "dw",
               distribution = "beta",
               n = n_total_it)
}

  if(ci_in[["duration"]]){

    sim[["duration"]] <-
      simulate(var_name = "duration",
               distribution = "normal",
               n = n_total_it)
  }

  if((!base::is.null(input_args$erf_eq_lower) |
      !base::is.null(input_args$erf_eq_lower))){
    base::stop("Sorry, the summary of uncertainty for erf_eq is not currently supported",
               call. = FALSE)
  }



  # Create function to replace the input value by the simulated values
  # (to be used below to create the df with simulated values)
  sim_vars <- base::names(sim)
  sim_vars_ci <-
    base::replace(
      base::paste0(sim_vars, "_ci"),
      base::paste0(sim_vars, "_ci") %in% "rr_ci", "erf_ci")
  sim_vars_and_ci <- c(sim_vars, sim_vars_ci)

  # Get unique combinations of grouping variables
  input_groups <- input_table |>
    dplyr::select(-dplyr::any_of(sim_vars_and_ci))|>
    dplyr::distinct()

  n_groups <- base::nrow(input_groups)

  # Expand the data
  input_groups_expanded <- input_groups[base::rep(1:n_groups, each = n_sim), ]

  template <-
    tibble::tibble(
      sim_id = base::as.numeric(base::rep(1:n_sim, each=n_geo)),
      geo_id_disaggregated =
        base::as.character(base::rep(base::unique(input_groups$geo_id_disaggregated),
                                     times=n_sim))) |>
    dplyr::left_join(input_groups,
                     by = "geo_id_disaggregated",
                     relationship = "many-to-many")|>
    dplyr::select(dplyr::all_of(c("sim_id", "geo_id_disaggregated", "exposure_dimension")))

  sim_df <-
    dplyr::left_join(template, input_groups,
                     by = c("geo_id_disaggregated", "exposure_dimension"),
                     relationship = "many-to-one") |>
    dplyr::bind_cols(sim)

  sim_df[sim_vars_ci] <- base::paste0("central_" , sim_df$sim_id)


  impact_raw_sim <-
    healthiar:::get_impact(input_table = sim_df,
                           pop_fraction_type = "paf")

  impact_sim <-
    healthiar:::get_output(impact_raw = impact_raw_sim)[["health_main"]]


  # Determine 95% CI of impact #################################################

  # * Single geo unit ##########################################################

  if ( ( n_geo == 1 ) ) {

    ## CI of aggregated impact
    ### Because there's only 1 geo unit the aggregated impact is the same as the geo unit impact
    summarized_ci <- stats::quantile(x = impact_sim$impact,
                                     probs = c(0.025, 0.5, 0.975),
                                     na.rm = TRUE)

    summarized_ci <- base::unname(summarized_ci) # Unname to remove percentiles from the names vector
    summarized_ci <- tibble::tibble(central_estimate = summarized_ci[2],
                                    lower_estimate = summarized_ci[1],
                                    upper_estimate = summarized_ci[3])

    # * Multiple geo units ###################################################
  } else if ( n_geo > 1 ) {
    grouping_geo_var <-
      names(impact_sim)[grepl("geo_id", names(impact_sim))]

     ## CIs of impact per geo unit
    impact_per_geo_unit <- impact_sim |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grouping_geo_var))) |>
      dplyr::summarize(
        impact_central = stats::quantile(x = impact, probs = c(0.5), na.rm = TRUE),
        impact_lower = stats::quantile(x = impact, probs = c(0.025), na.rm = TRUE),
        impact_upper = stats::quantile(x = impact, probs = c(0.975), na.rm = TRUE)
      )

    results[["uncertainty_detailed"]][["geo_specific"]] <- impact_per_geo_unit

    ## CIs of impact aggregated over geo units
    summarized_ci <- impact_per_geo_unit |>
      dplyr::summarize(
        central_estimate = base::sum(impact_central),
        lower_estimate = base::sum(impact_lower),
        upper_estimate = base::sum(impact_upper)
      )

  }

  # Output #####################################################################
  on.exit(options(user_options))

  results[["uncertainty_main"]] <- summarized_ci

  results[["uncertainty_detailed"]][["raw"]] <- impact_sim # to check interim results during development

  return(results)


}

