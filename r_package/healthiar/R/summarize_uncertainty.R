#' Get Monte Carlo confidence intervals

#' @description
#' This function determines summary uncertainty (based on at least one variable
#' with central, lower and upper estimate) based on attribute() or compare()
#' function output.

#' @param results \code{variable} in which the results of an attribute function is stored.
#' @param n_sim \code{numeric value} indicating the number of simulations to be performed.

#' @returns
#' This function returns confidence intervals for the attributable health impacts using a Monte Carlo simulation.

#' @author Axel Luyten

#' @examples
#' TODO

#' @export

#' @keywords internal



summarize_uncertainty <- function(
    results,
    n_sim
    ) {

  ## SCRIPT STRUCTURE
  ## For each variable with a confidence interval a distribution is fitted, and
  ## then n_sim values are simulated based on the distribution.
  ## Then n_sim "new" impacts are calculated using the simulated values.
  ## These steps are seperated for RR and AR pathways (see script outline).
  ## Depending on the calculation pathway specified by the user in the attribute
  ## call (singe exp vs exp dist; iteration yes/no), different code blocks are
  ## triggered.
  ## Lastly, the confidence intervals are determined based on the n_sim "new"
  ## impact vlalues (same code for RR and AR pathways)

  ## Set options
  user_options <- options()
  options(digits = 15) # Make sure that no rounding occurs

  ## Set seed for reproducibility
  set.seed(123)

  # Store the input data as entered in the arguments
  input_args <- results[["health_detailed"]][["input_args"]]
  input_table <- results[["health_detailed"]][["input_table"]]

  ## Determine number of geographic units
  n_geo <-
    # Exceptionally, let's use here unique() instead of input_args
    # because in some cases the users do not enter the geo_id.
    # In that cases compile_input() provide a geo_id and it is shown in impact_raw
    length(unique(input_table$geo_id_disaggregated))

  # Is population-weighted mean exposure?
  is_pwm_exposure <-
    base::unique(input_table$exposure_type == "population_weighted_mean")
  # Is categorical?
  is_categorical_exposure <-
    ! is_pwm_exposure

  # Sequence (vector) of exposure_dimension
  # Use impact_raw because it was obtained in compiled_input
  n_exp <- base::max(input_table$exposure_dimension)
  seq_exposure_dimension <- 1:n_exp





  # Define betaExpert function #################################################
  ## Copied from source code of prevalence::betaExpert() here:
  ### https://github.com/cran/prevalence/blob/master/R/betaExpert.R

  betaExpert <-
    function(best, lower, upper, p = 0.95, method = "mode"){

      ## functions to optimize ~ mode
      f_mode <-
        function(x, mode, p, target){
          return(
            sum(
              (qbeta(p = p,
                     shape1 = x,
                     shape2 = (x * (1 - mode) + 2 * mode - 1) / mode) -
                 target) ^ 2
            ))
        }

      f_mode_zero <-
        function(x, p, target){
          return((qbeta(p = p, shape1 = 1, shape2 = x) - target) ^ 2)
        }

      f_mode_one <-
        function(x, p, target){
          return((qbeta(p = p, shape1 = x, shape2 = 1) - target) ^ 2)
        }

      ## functions to optimize ~ mean
      f_mean <-
        function(x, mean, p, target){
          return(
            sum(
              (qbeta(p = p,
                     shape1 = x,
                     shape2 = (x * (1 - mean)) / mean) -
                 target) ^ 2
            ))
        }

      ## define 'target' and 'p'
      if (!missing(lower) & missing(upper)){
        target <- lower
        p <- 1 - p
      } else if (!missing(upper) & missing(lower)){
        target <- upper
      } else if (!missing(upper) & !missing(lower)){
        target <- c(lower, upper)
        p <- c(0, p) + (1 - p) / 2
      }

      ## derive a and b (=shape1 and shape2)
      if (method == "mode"){
        if (best == 0){
          a <- 1
          b <- optimize(f_mode_zero, c(0, 1000), p = p, target = target)$minimum
        } else if (best == 1) {
          a <- optimize(f_mode_one, c(0, 1000), p = p, target = target)$minimum
          b <- 1
        } else {
          a <- optimize(f_mode, c(0, 1000),
                        mode = best, p = p, target = target)$minimum
          b <- (a * (1 - best) + 2 * best - 1) / best
        }
      } else if (method == "mean"){
        a <- optimize(f_mean, c(0, 1000),
                      mean = best, p = p, target = target)$minimum
        b <- (a * (1 - best)) / best
      }

      ## create 'out' dataframe
      out <- list(alpha = a, beta = b)
      class(out) <- "betaExpert"

      ## return 'out'
      return(out)
    }

  # Relative risk ##############################################################
  # Use input_table because approach risk a default value
  # (i.e. the value might not be available in input_args)
  if ( unique(input_table$approach_risk) == "relative_risk" ) {

    ## Define helper functions for fitting a gamma distribution with optimization
    ## for the relative risk.
    ### NOTE: the functions were adapted from those provided by Sciensano

    ## Set gamma distribution specs
    vector_probabilities <- c(0.025, 0.975)
    par <- 2 ## shape parameter of the gamma distribution

    ## Fit gamma distribution
    f_gamma <-
      function(par, central_estimate, vector_propabilities, lower_estimate, upper_estimate) {
        qfit <- qgamma(p = vector_propabilities, shape = par, rate = par / central_estimate)
        return(sum((qfit - c(lower_estimate, upper_estimate))^2))
      }

    ## Optimize gamma distribution
    optim_gamma <-
      function(central_estimate, lower_estimate, upper_estimate) {
        vector_propabilities <- c(0.025, 0.975)
        f <- optimize(f = f_gamma,
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
        rgamma(n = n_sim, fit[1], fit[2]) }

    ## Create empty tibble to store simulated values & results in
    dat <-
      tibble::tibble(
        # geo_id_disaggregated must be always character
        # to avoid inconsistency with likely numeric formats entered by users
        # and because no math operation is needed for this variable
        # geo_id_disaggregated from input_table insteado of input_args because
        # if users do not enter any data there, a default id is assigned in compile_input()
        geo_id_disaggregated =
          as.character(
            rep(unique(input_table$geo_id_disaggregated),
                each = n_sim * n_exp)),
        rr =
          rep(NA, times = n_sim * n_geo * n_exp),
        rr_increment =
          rep(input_args$rr_increment,
              times = n_sim * n_geo * n_exp),
        erf_shape =
          rep(input_args$erf_shap,
              times = n_sim * n_geo * n_exp),
        exp =
          rep(NA,
              times = n_sim * n_geo * n_exp),
        cutoff =
          rep(NA,
              times = n_sim * n_geo * n_exp),
        bhd =
          rep(NA,
              times = n_sim * n_geo * n_exp),
        dw =
          rep(NA,
              times = n_sim * n_geo * n_exp),
        rr_conc =
          rep(NA,
              times = n_sim * n_geo * n_exp),
        paf =
          rep(NA,
              times = n_sim * n_geo * n_exp),
        prop_pop_exp =
          rep(NA,
              times = n_sim * n_geo * n_exp)
      )



    # * Simulate values for input variables ####################################
    ## based on the provided 95% confidence intervals

    # * * rr ###################################################################

    # * * * rr CIs, both single and multiple geo unit case #####################
    if ( !base::is.null(input_args$rr_lower))  {

      dat <- dat |>
        # Gamma distribution with optimization to generate simulated RR's
        dplyr::mutate(
          rr = sim_gamma(
            n_sim = n_sim * n_geo * n_exp,
            central_estimate = input_args$rr_central,
            lower_estimate = input_args$rr_lower,
            upper_estimate = input_args$rr_upper)
        )

    # * * * No rr CIs, both single and multiple geo unit case ##################
    } else {

      dat <- dat |>
        dplyr::mutate(
          rr = input_args$rr_central)
    }

    # * * exp ##################################################################

    # * * * Single exposure ####################################################

    # * * * * exp CIs, single geo unit #########################################
    if (
      ( !base::is.null(input_args$rr_lower) ) &
      ( is_pwm_exposure ) &
      ( n_geo == 1 )
      ) {

      ## Determine standard deviation (sd) based on the formula:
      ## (exp_upper - exp_lower) / (2 * 1.96)
      sd_exp <-
      (input_args$exp_upper - input_args$exp_lower) / (2 * 1.96)

      ## Simulate values
      dat <- dat |>
        dplyr::mutate(
          exp = rnorm(
            n_sim,
            mean = input_args$exp_central,
            sd = sd_exp))

    # * * * * No exp CIs, single geo unit ######################################
    } else if (
      ( base::is.null(input_args$exp_lower) ) &
      ( is_pwm_exposure ) &
      ( n_geo == 1 )
      ) {

      ## Assign central value to all rows as no CI's present for exp variable
      dat <- dat |>
        dplyr::mutate(exp = input_args$exp_central)

    # * * * * exp CIs, multiple geo units ######################################
    } else if ( ( !base::is.null(input_args$exp_lower) ) &
                ( is_pwm_exposure ) &
                ( n_geo > 1 ) ) {

      ## For each geo unit, fit a normal distribution and assign to dat
      ## Fit distribution based on each geo units central, lower and upper bhd values

      dat_with_exp_ci <- input_table |>
        dplyr::select(geo_id_disaggregated, exp_ci, exp) |>
        dplyr::distinct() |>
        tidyr::pivot_wider(
          names_from = exp_ci,
          names_prefix = "exp_",
          values_from = exp)

      simulated_data <- dat_with_exp_ci |>
        dplyr::rowwise() |>
        dplyr::mutate(
          ## Generate n_sim simulated values for each row
          exp = list(
            rnorm(
              n_sim,
              mean = exp_central,
              sd = (exp_upper - exp_lower) / (2 * 1.96)
            )
          )) |>
        dplyr::ungroup() |>
        ## Expand each row so each simulated value has its own row
        tidyr::unnest(exp) |>
        ## Keep only relevant columns
        dplyr::select(geo_id_disaggregated, exp)


      ## Add simulated exposure values to dat tibble
      dat <-
        dat |>
        dplyr::select(-exp) |>
        dplyr::bind_cols(
          simulated_data |> dplyr::select(-geo_id_disaggregated)
          ) |>
        dplyr::relocate(exp, .after = erf_shape)



    # * * * * No exp CIs, multiple geo units ###################################
    } else if (
      ( base::is.null(input_args$exp_lower) ) &
      ( is_pwm_exposure ) &
      ( n_geo > 1 )
      ) {

      ## Add central exposure value of each geo unit to dat tibble using left_join
      dat <- dat |>
        dplyr::select(-exp) |>
        dplyr::left_join(
          x = _,
          y = input_table |>
            dplyr::select(exp, geo_id_disaggregated) |>
            dplyr::distinct(),
          by = "geo_id_disaggregated"
        ) |>
        dplyr::relocate(exp, .after = erf_shape)

    }

    # * * *  Exposure distribution #############################################

    # * * *  * exp CIs, single geo unit #####################
    if (
      ( !base::is.null(input_args$exp_lower) ) &
      ( is_categorical_exposure ) &
      ( n_geo == 1 )
      ) {

      ## Vectors needed for simulation below
      exp_central <- input_args$exp_central
      exp_lower <- input_args$exp_lower
      exp_upper <- input_args$exp_upper
      prop_pop_exp <- input_args$prop_pop_exp


      # Simulate nsim exposure values (normal distribution) for each exp cat
      # using the corresultsponding values of exp_lower & exp_upper
      dat_exp_dist <- tibble::tibble(
        row_id = 1:n_sim) |>

        dplyr::bind_cols(
          purrr::map(
            # .x refers to the values 1, 2, ..., (nr. of exposure categories)
            .x = seq_exposure_dimension,
            .f = ~ tibble::tibble(
              !!paste0("exp_", .x) :=                     # .x refers to the xth element of the vector
                rnorm(n_sim,
                      mean = exp_central[.x],
                      sd = (exp_upper[.x] - exp_lower[.x]) / (2 * 1.96)),
              !!paste0("prop_pop_exp_", .x) := prop_pop_exp[.x])) |>
            purrr::reduce(dplyr::bind_cols))

      # Merge dat & dat_exp_dist
      dat <- cbind(dat, dat_exp_dist) |>
        dplyr::select(-exp)

      # * * * * No exp CIs, single geo unit ####################################
    } else if ( (base::is.null(input_args$exp_lower) ) &
                ( is_categorical_exposure ) &
                ( n_geo == 1 ) ) {

      # Vectors needed for simulation below (exp_central & prop_pop_exp)
      exp_central <- input_args$exp_central
      prop_pop_exp <- input_args$prop_pop_exp

      # Create a column for each exposure categories and each prop_pop_exp value
      dat_exp_dist <- tibble::tibble(
        row_id = 1:n_sim) |>

        dplyr::bind_cols(
          purrr::map(.x = seq_exposure_dimension,
            .f = ~ tibble::tibble(
              !!paste0("exp_", .x) := exp_central[.x],                     # .x refers to the xth element of the vector
              !!paste0("prop_pop_exp_", .x) := prop_pop_exp[.x])) |>
            purrr::reduce(dplyr::bind_cols))

      # Merge dat & dat_exp_dist
      dat <- cbind(dat, dat_exp_dist) |>
        dplyr::select(-exp)

      # * * * * No exp CIs, multiple geo units #################################
    } else if (
      ( base::is.null(input_args$exp_lower) ) &
      ( is_categorical_exposure ) &
      ( n_geo > 1 )
      ) {

      # Vectors needed for simulation below (exp_central & prop_pop_exp)
      # Extract exposure values per geo_id_disaggregated and save in a sub-list

      dat <- dat |>
        dplyr::select(-exp, -prop_pop_exp) |>
        dplyr::left_join(input_table |>
                    dplyr::filter(erf_ci == "central") |>
                    dplyr::select(geo_id_disaggregated, exp, prop_pop_exp),
                  by = "geo_id_disaggregated") |>
        tidyr::unnest_wider(c(exp, prop_pop_exp), names_sep = "_")

      # * * * *  Exp CIs, multiple geo units ###################################

    } else if (
      ( ! base::is.null(input_args$exp_lower) ) &
      ( is_categorical_exposure ) &
      ( n_geo > 1 )
      ) {


      ## Generate a tibble with 1000 simulations per exposure category for each geo_id

      ## Vectors needed for simulation of exposure values (exp_central, exp_lower, exp_upper & prop_pop_exp central)
      ### Pull exposures for all geo id's
      exp_central <- input_table |>
        dplyr::filter(exp_ci == "central") |>
        dplyr::select(geo_id_disaggregated, exp_central = exp)
      exp_lower <- input_table |>
        dplyr::filter(exp_ci == "lower") |>
        dplyr::select(geo_id_disaggregated, exp_lower = exp)
      exp_upper <- input_table |>
        dplyr::filter(exp_ci == "upper") |>
        dplyr::select(geo_id_disaggregated, exp_upper = exp)
      dat_exp <- cbind(exp_central,
                       exp_lower |> dplyr::select(-geo_id_disaggregated),
                       exp_upper |> dplyr::select(-geo_id_disaggregated)
                       )
      prop_pop_exp <- input_table |>
        dplyr::select(geo_id_disaggregated, prop_pop_exp) |>
        dplyr::distinct(geo_id_disaggregated, prop_pop_exp, .keep_all = TRUE)

      ## Create vectors of column names
      exp_columns <- paste0("exp_", seq_exposure_dimension)
      prop_columns <- paste0("prop_pop_exp_", seq_exposure_dimension)

      ## Create empty tibble to be filled in loop below
      ### @ AC: sorry for the loop : /
      ###  I tried to avoid it, but failed. To be changed.
      dat_sim <- tibble::tibble(
        geo_id_disaggregated = character(0)  # Initialize geo_id_disaggregated as numeric
      ) |>
        dplyr::bind_cols(
          purrr::set_names(rep(list(numeric(0)), length(exp_columns)), exp_columns),
          purrr::set_names(rep(list(numeric(0)), length(prop_columns)), prop_columns)
        )

      ## Loop through geo ID's
      for (i in exp_central$geo_id_disaggregated){

        ## Create temp tibble to store simulated values in
        temp <- tibble::tibble(
          geo_id_disaggregated = rep(i, times = n_sim)) |>
          dplyr::bind_cols(
            purrr::map(
              ## .x will take the values 1, 2, ..., (nr. of exposure categories)
              .x = seq_exposure_dimension,
              .f = ~ tibble::tibble(
                !!paste0("exp_", .x) :=
                  ## For each exposure category generate n_sim simulated values
                  rnorm(
                    n_sim,
                    mean = dat_exp |> dplyr::filter(geo_id_disaggregated == i) |> dplyr::pull(exp_central) |> unlist(x = _) |> dplyr::nth(.x) ,
                    sd = ( dat_exp |> dplyr::filter(geo_id_disaggregated == i) |> dplyr::pull(exp_upper) |> unlist(x = _) |> dplyr::nth(.x) -
                             dat_exp |> dplyr::filter(geo_id_disaggregated == i) |> dplyr::pull(exp_lower) |> unlist(x = _) |> dplyr::nth(.x) ) / (2 * 1.96) # Formula: exp_upper - exp_lower) / (2 * 1.96)
                  ),
                !!paste0("prop_pop_exp_", .x) := prop_pop_exp|> dplyr::filter(geo_id_disaggregated == i) |> dplyr::pull(prop_pop_exp) |> unlist(x = _) |> dplyr::nth(.x)
              )) |>
              purrr::reduce(dplyr::bind_cols)
          )


        ## Add simulated values of current iteration to dat_sim tabble
        dat_sim <- dat_sim |>
          dplyr::bind_rows(temp)

      }


      # Add simulated values of all geo units to dat tibble
      dat <- dat |>
        dplyr::bind_cols(dat_sim |> dplyr::select(-geo_id_disaggregated)) |>
        dplyr::select(-exp)

    }

    # * * cutoff ###############################################################

    # * * * cutoff CIs, both single and multiple geo unit case ##################
    if ( !is.null(input_args$cutoff_lower) ) {

      ## Determine standard deviation (sd) based on the formula:
      ## (cutoff_upper - cutoff_lower) / (2 * 1.96)
      sd_cutoff <-
        (input_args$cutoff_upper - input_args$cutoff_lower) / (2 * 1.96)

      dat <- dat |>
        dplyr::mutate(
          cutoff = rnorm(
            n_sim * n_geo * n_exp,
            mean = input_args$cutoff_central,
            sd = sd_cutoff))

    # * * * No cutoff CIs, both single and multiple geo unit case ##############
    } else if ( is.null(input_args$cutoff_lower) ) {

      dat <- dat |>
        dplyr::mutate(cutoff = input_args$cutoff_central)
    }

    # * * bhd ##################################################################

    # * * * bhd CIs & single geo unit ##########################################
    if ( (!base::is.null(input_args$bhd_lower)) &
      ( n_geo == 1 ) ) {

      ## Determine standard deviation (sd) based on the formula:
      ## (bhd_upper - bhd_lower) / (2 * 1.96)
      sd_bhd <- #(bhd_upper - bhd_lower) / (2 * 1.96)
        (base::unlist(input_args$bhd_upper) - base::unlist(input_args$bhd_lower)) / (2 * 1.96)
      dat <- dat |>
        dplyr::mutate(
          bhd = rnorm(
            n_sim,
            mean = base::unlist(input_args$bhd_central),
            sd = sd_bhd))

    # * * * No bhd CIs & single geo unit ##########################################
    } else if ( (base::is.null(input_args$bhd_lower)) &
                ( n_geo == 1 ) ) {

      dat <- dat |>
        dplyr::mutate(bhd = base::unlist(input_args$bhd_central))

    # * * * bhd CIs & multiple geo units ##########################################
    } else if (
      (!base::is.null(input_args$bhd_lower)) &
      ( n_geo > 1 )
      ) {

      ## For each geo unit, fit a normal distribution and assign to dat
      ## Fit distribution based on each geo units central, lower and upper bhd values

      dat_with_bhd_ci <- input_table |>
        dplyr::select(geo_id_disaggregated, bhd_ci, bhd) |>
        dplyr::distinct() |>
        tidyr::pivot_wider(
          names_from = bhd_ci,
          names_prefix = "bhd_",
          values_from = bhd)

      simulated_data <- dat_with_bhd_ci |>
        dplyr::rowwise() |>
        dplyr::mutate(
          ## Generate n_sim simulated values for each row
          bhd = list(
            rnorm(
              n_sim,
              mean = bhd_central,
              sd = (bhd_upper - bhd_lower) / (2 * 1.96)
            )
          )) |>
        dplyr::ungroup() |>
        ## Expand each row so each simulated value has its own row
        tidyr::unnest(bhd) |>
        ## Keep only relevant columns
        dplyr::select(geo_id_disaggregated, bhd)

      ## Add simulated exposure values to dat tibble
      dat <- dat |>
        dplyr::select(-bhd) |>
        dplyr::bind_cols(
          simulated_data |> dplyr::select(-geo_id_disaggregated)
        ) |>
        dplyr::relocate(bhd, .after = cutoff)

    # * * * No bhd CI's & multiple geo units ######################################
    } else if (
      (base::is.null(input_args$bhd_lower)) &
      ( n_geo > 1 )
      ) {

      ## Add central exposure value of each geo unit to dat tibble using left_join
      dat <- dat |>
        dplyr::select(-bhd) |>
        dplyr::left_join(
          x = _,
          y = input_table |>
            dplyr::select(bhd, geo_id_disaggregated) |>
            dplyr::distinct(),
          by = "geo_id_disaggregated"
        ) |>
        dplyr::relocate(bhd, .after = cutoff)


    }

    # * * dw ###################################################################

    # * * * dw CIs, both single and multiple geo unit case #####################
    if ( (!base::is.null(input_args$dw_lower)) &
         ( n_geo == 1 ) ) {

      ## beta distribution using prevalence::betaExpert()
      ### Determine the alpha and beta parameters needed to fit beta distribution using the (source code of the) prevalence::betaExpert() function
      dw_sim <- betaExpert(
        input_args$dw_central,
        input_args$dw_lower,
        input_args$dw_upper,
        method = "mean")

      ### Simulate nsim disability weight values
      dat <- dat |>
        dplyr::mutate(
          dw = rbeta(
            n = n_sim,
            shape1 = as.numeric(unname(dw_sim["alpha"])),
            shape2 = as.numeric(unname(dw_sim["beta"]))))

      ## ALTERNATIVE: Using normal distribution
      ## Define standard deviation = (dw_upper - dw_lower) / (2 * 1.96)
      # sd_dw <-
      #   (results[["health_detailed"]][["impact_raw"]] |> dplyr::filter(dw_ci == "upper") |> dplyr::pull(dw) |> dplyr::first() -
      #      results[["health_detailed"]][["impact_raw"]] |> dplyr::filter(dw_ci == "lower") |> dplyr::pull(dw) |>  dplyr::first()) / (2 * 1.96)
      # dat <- dat |>
      #   dplyr::mutate(
      #     dw = rnorm(
      #       n_sim,
      #       mean = results[["health_detailed"]][["impact_raw"]] |>
      #         dplyr::filter(dw_ci == "central") |>
      #         dplyr::pull(dw) |>
      #         dplyr::first(),
      #       sd = sd_dw))

      ## ALTERNATIVE: Using beta distribution using qbeta()
      # dat <- dat |>
      #   dplyr::mutate(dw = sim_beta(n_sim = n_sim,
      #                               dw_central = dw_central,
      #                               vector_dw_ci = vector_dw_ci))

    # * * * No dw CIs, both single and multiple geo unit case ##################
    } else if ( !base::is.null(input_args$dw_central) &
                base::is.null(input_args$dw_lower) &
                n_geo == 1 ) {
      dat <- dat |>
        dplyr::mutate(dw = input_args$dw_central)

    # * * * No dw inputted, both single and multiple geo unit case #############
    } else if (base::is.null(input_args$dw_central)) {

      dat <- dat |>
        dplyr::mutate(dw = 1)

    }

    # * rr_conc ################################################################

    # * * Single exposure case #################################################
    if ( is_pwm_exposure ) {

    ## Calculate rr_conc using healthiar::get_risk
    dat <- dat |>
      dplyr::mutate(
        rr_conc = purrr::pmap(
          list(rr = rr, exp = exp, cutoff = cutoff, rr_increment = rr_increment, erf_shape = erf_shape),
          function(rr, exp, cutoff, rr_increment, erf_shape){
            rr_conc <- healthiar::get_risk(
              rr = rr,
              exp = exp,
              cutoff = cutoff,
              rr_increment = rr_increment,
              erf_shape = erf_shape,
              erf_eq = NULL)
            return(rr_conc)
          }
        )
      )

    dat$rr <- base::unlist(dat$rr)

    # * * Exposure distribution case ###########################################
    } else if ( is_categorical_exposure ) {

      # Calc rr_conc for each exp cat
      dat <- dat |>
        dplyr::bind_cols(
          ## .x will take the values 1, 2, ..., until (nr. of exposure categories)
          purrr::map(
            .x = seq_exposure_dimension,
            .f = ~ tibble::tibble(
              !!paste0("rr_conc_", .x) :=
                get_risk(rr = dat$rr,
                         exp = dat[[!!paste0("exp_", .x)]], # Selects xth element of the vector
                         cutoff = dat$cutoff[1],
                         rr_increment = dat$rr_increment[1],
                         erf_shape = dat$erf_shape[1],
                         erf_eq = NULL)
              )
            )
          )
    }

    # * PAF ####################################################################


    # * * Single exposure case #################################################

    ## Determine PAF with healthiar:::get_pop_fraction()
    if ( is_pwm_exposure ) {

      dat <- dat |>
        dplyr::mutate(
          paf = purrr::pmap(
            list(rr_conc = rr_conc, prop_pop_exp = base::unlist(input_args$prop_pop_exp)),
            function(rr_conc, prop_pop_exp){
              paf <- healthiar:::get_pop_fraction(
                rr_conc_1 = rr_conc,
                rr_conc_2 = 1,
                prop_pop_exp_1 = prop_pop_exp,
                prop_pop_exp_2 = prop_pop_exp)
              return(paf)
            }
          )
        )

      dat$paf <- base::unlist(dat$paf)

    # * * Exposure distribution case ###########################################

    } else if ( is_categorical_exposure ) {

      ## Determine product_x = rr_conc_x * prop_pop_exp_x
      ## This is an intermediate step towards PAF calculation
      dat <- dat |>
        dplyr::bind_cols(
          purrr::map(
            .x = seq_exposure_dimension,
            .f = ~ tibble::tibble(
              !!paste0("product_", .x) := dat[[!!paste0("rr_conc_", .x)]] * dat[[!!paste0("prop_pop_exp_", .x)]]
            )
          )
        )

      ## WORKING: CALCULATE PAF FOLLOWING EXCEL EXAMPLE FROM NIPH
      # Excel located here: ..\best-cost\r_package\testing\input\noise_niph
      # NOTE 2024-11-26: PAF matches the result in the Excel sheet "Relative_risk_IHD_WHO_2003a" exactly
      dat <- dat |>
        dplyr::mutate(sum_product = rowSums(dplyr::across(dplyr::contains("product_")))) |>
        dplyr::mutate(paf = ( sum_product - 1 ) / sum_product)



      ## NOT WORKING: CALCULATE PAF WITH healthiar:::get_pop_fraction()
      # to calculate paf per exp band
      # dat <- dat |>
      #   dplyr::select(-c(rr_conc, paf, paf_weighted, prop_pop_exp))
      #
      # # Initial try (calculate a paf for each exposure category)
      # dat <- dat |>
      #   dplyr::rowwise(data = _) |> # Calculate PAF row by row
      #   dplyr::mutate(
      #     # Use dplyr::across to iterate over corresponding "rr_conc_" and "prop_pop_exp_" pairs
      #     dplyr::across(.cols = dplyr::starts_with("rr_conc_"),
      #                   .fns = ~ healthiar:::get_pop_fraction(
      #                     rr_conc_1 = as.numeric(.x),
      #                     rr_conc_2 = 1,
      #                     prop_pop_exp_1 = as.numeric(dat[[gsub("rr_conc_", "prop_pop_exp_", dplyr::cur_column())]]),
      #                     prop_pop_exp_2 = as.numeric(dat[[gsub("rr_conc_", "prop_pop_exp_", dplyr::cur_column())]])),
      #                   .names = "paf_{stringr::str_remove(.col, 'rr_conc_')}")) |>
      #   # Sum impacts across noise bands to obtain total impact
      #   dplyr::mutate(paf = rowSums(dplyr::across(dplyr::starts_with("paf_"))))

    }

    # *  Get impact ############################################################
    ## Multiply PAFs with bhd

      dat <- dat |>
        dplyr::mutate(impact_total = paf * bhd * dw, .after = paf)
    ## If there is no dw used in the preceding attribute call, dw is set to 1 earlier in script

    # Absolute risk ############################################################

    # * Simulate values for input variables ####################################
  } else if ( unique(input_table$approach_risk) == "absolute_risk" ) {

    ## Create (empty) tibble to store simulated values & results in
    dat <- tibble::tibble(
      row_id = 1:(n_sim*n_geo),
      geo_id_disaggregated = rep(1:n_geo, each = n_sim))

    # * * exp ##################################################################

    # * * * exp CI's & single geo unit #########################################

    if (
      (! base::is.null(input_args$exp_lower)) &
      ( n_geo == 1 )
    ) {

      # browser()

      ## Vectors needed for simulation below
      exp_central <- input_table |>
        dplyr::filter(exp_ci == "central") |>
        (\(x) if ("duration_ci" %in% colnames(x)) dplyr::filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) dplyr::filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::pull(exp) |>
        base::unlist(x = _)
      exp_lower <- input_table |>
        dplyr::filter(exp_ci == "lower") |>
        (\(x) if ("duration_ci" %in% colnames(x)) dplyr::filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) dplyr::filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::pull(exp) |>
        base::unlist(x = _)
      exp_upper <- input_table |>
        dplyr::filter(exp_ci == "upper") |>
        (\(x) if ("duration_ci" %in% colnames(x)) dplyr::filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) dplyr::filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::pull(exp) |>
        base::unlist(x = _)
      pop_exp <- input_table |>
        dplyr::filter(exp_ci == "central") |>
        (\(x) if ("duration_ci" %in% colnames(x)) dplyr::filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) dplyr::filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::pull(pop_exp) |>
        base::unlist(x = _)

      ## Simulate 1000 exposure values (normal distribution) for each noise band
      ## using the corresponding values of exp_lower & exp_upper
      dat <- dat |>
        dplyr::bind_cols(
          purrr::map(.x = seq_along(
            # .x will take the values 1, 2, ..., (nr. of exposure categories)
            input_table |>
              dplyr::filter(exp_ci == "central") |>
              (\(x) if ("duration_ci" %in% colnames(x)) dplyr::filter(x, duration_ci == "central") else x)() |>
              (\(x) if ("dw_ci" %in% colnames(x)) dplyr::filter(x, dw_ci == "central") else x)() |>
              dplyr::filter(erf_ci == "central") |>
              dplyr::pull(exp) |>
              # dplyr::first() |>
              base::unlist(x = _)),
            .f = ~ tibble::tibble(
              !!paste0("exp_", .x) := # Refers to the xth element of the vector
                rnorm(n_sim,
                      mean = exp_central[.x],
                      sd = (exp_upper[.x] - exp_lower[.x]) / (2 * 1.96)),
              !!paste0("pop_exp_", .x) := pop_exp[.x])) |>
            purrr::reduce(dplyr::bind_cols))


    # * * * exp CI's & multiple geo units ######################################
    } else if (
      ( ! base::is.null(input_args$exp_lower) ) &
      ( n_geo > 1 )
      ) {

      ## Create exp vectors needed for simulation below
      exp_central <- input_table |>
        dplyr::filter(exp_ci == "central") |>
        (\(x) if ("duration_ci" %in% colnames(x)) dplyr::filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) dplyr::filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::select(geo_id_disaggregated, exposure_dimension, exp) |>
        dplyr::group_by(geo_id_disaggregated) |>
        dplyr::summarize(exp_central = list(exp), .groups = "drop")
      exp_lower <- input_table |>
        dplyr::filter(exp_ci == "lower") |>
        (\(x) if ("duration_ci" %in% colnames(x)) dplyr::filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) dplyr::filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::select(geo_id_disaggregated, exposure_dimension, exp) |>
        dplyr::group_by(geo_id_disaggregated) |>
        dplyr::summarize(exp_lower = list(exp), .groups = "drop")
      exp_upper <- input_table |>
        dplyr::filter(exp_ci == "upper") |>
        (\(x) if ("duration_ci" %in% colnames(x)) dplyr::filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) dplyr::filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::select(geo_id_disaggregated, exposure_dimension, exp) |>
        dplyr::group_by(geo_id_disaggregated) |>
        dplyr::summarize(exp_upper = list(exp), .groups = "drop")
      ## Bind vectors
      dat_exp <- cbind(exp_central,
                       exp_lower |> dplyr::select(-geo_id_disaggregated),
                       exp_upper |> dplyr::select(-geo_id_disaggregated))
      ## Create vector with populations exposed
      pop_exp <- input_table |>
        dplyr::filter(exp_ci == "central") |>
        (\(x) if ("duration_ci" %in% colnames(x)) dplyr::filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) dplyr::filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::select(geo_id_disaggregated, pop_exp) |>
        dplyr::group_by(geo_id_disaggregated) |>
        dplyr::summarize(pop_exp = list(pop_exp), .groups = "drop")
      ## Create vectors of column names
      exp_columns <- paste0("exp_", seq_exposure_dimension)
      pop_columns <- paste0("pop_exp_", seq_exposure_dimension)

      ## Create empty tibble to be filled in loop below
      ## @ AC: sorry for the loop (again) : P

      dat_sim <- tibble::tibble(
        geo_id_disaggregated = character(0)  # Initialize geo_id_disaggregated as numeric
      ) |>
        dplyr::bind_cols(
          purrr::set_names(rep(list(numeric(0)), length(exp_columns)), exp_columns),
          purrr::set_names(rep(list(numeric(0)), length(pop_columns)), pop_columns)
        )

      for (i in exp_central$geo_id_disaggregated){

        ## Create temp tibble to store simulated values in
        temp <- tibble::tibble(
          geo_id_disaggregated = rep(i, times = n_sim)) |>
          dplyr::bind_cols(
            purrr::map(
              # .x will take the values 1, 2, ..., (nr. of exposure categories)
              .x = seq_along(
                dat_exp |>
                  dplyr::filter(geo_id_disaggregated == i) |>
                  dplyr::pull(exp_central) |>
                  base::unlist(x = _)
              ),
              .f = ~ tibble::tibble(
                !!paste0("exp_", .x) :=
                  ## For each exposure category generate n_sim simulated values
                  rnorm(
                    n_sim,
                    mean = dat_exp |> dplyr::filter(geo_id_disaggregated == i) |> dplyr::pull(exp_central) |> unlist(x = _) |> dplyr::nth(.x) ,
                    # Formula for standard deviation (sd): exp_upper - exp_lower) / (2 * 1.96)
                    sd = ( dat_exp |> dplyr::filter(geo_id_disaggregated == i) |> dplyr::pull(exp_upper) |> unlist(x = _) |> dplyr::nth(.x) -
                             dat_exp |> dplyr::filter(geo_id_disaggregated == i) |> dplyr::pull(exp_lower) |> unlist(x = _) |> dplyr::nth(.x) ) / (2 * 1.96)
                  ),
                !!paste0("pop_exp_", .x) := pop_exp|> dplyr::filter(geo_id_disaggregated == i) |> dplyr::pull(pop_exp) |> unlist(x = _) |> dplyr::nth(.x)
              )) |>
              purrr::reduce(dplyr::bind_cols)
          )

        ## Add simulated values of current iteration to dat_sim tabble

        dat_sim <- dat_sim |>
          dplyr::bind_rows(temp)

      }

      # Add simulated values of all geo units to dat tibble
      dat <- dat |>
        dplyr::bind_cols(dat_sim |> dplyr::select(-geo_id_disaggregated))

      # * * * no exp CI's & single geo unit case ###############################
    } else if (
      ( base::is.null(input_args$exp_lower) ) &
      ( n_geo == 1  )
    ) {


      # browser()

      ## Create exp and prop_exp vectors
      exp_central <- input_table |>
        dplyr::filter(exp_ci == "central") |>
        (\(x) if ("duration_ci" %in% colnames(x)) dplyr::filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) dplyr::filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::pull(exp) |>
        base::unlist(x = _)
      pop_exp <- input_table |>
        dplyr::filter(exp_ci == "central") |>
        (\(x) if ("duration_ci" %in% colnames(x)) dplyr::filter(x, duration_ci == "central") else x)() |>
        (\(x) if ("dw_ci" %in% colnames(x)) dplyr::filter(x, dw_ci == "central") else x)() |>
        dplyr::filter(erf_ci == "central") |>
        dplyr::pull(pop_exp) |>
        base::unlist(x = _)

      ## Add exp and prop_exp to the dat dataframe
      ### Values taken from the exp_central and pop_exp vectors created above
      dat <- dat |>
        dplyr::bind_cols(
          purrr::map(.x = seq_along(
            # .x will take the values 1, 2, ..., (nr. of exposure categories)
            input_table |>
              dplyr::filter(exp_ci == "central") |>
              (\(x) if ("duration_ci" %in% colnames(x)) dplyr::filter(x, duration_ci == "central") else x)() |>
              (\(x) if ("dw_ci" %in% colnames(x)) dplyr::filter(x, dw_ci == "central") else x)() |>
              dplyr::filter(erf_ci == "central") |>
              dplyr::pull(exp) |>
              # dplyr::first() |>
              base::unlist(x = _)),
            .f = ~ tibble::tibble(
              !!paste0("exp_", .x) := exp_central[.x],
              !!paste0("pop_exp_", .x) := pop_exp[.x])) |>
            purrr::reduce(dplyr::bind_cols))

      # * * * no exp CI's & multiple geo unit case #############################
    }  else if (
    ( base::is.null(input_args$exp_lower) ) &
    ( n_geo > 1 )
    ) {

    ## Create exp and prop_exp vectors
    exp_central <- input_table |>
      dplyr::filter(exp_ci == "central") |>
      (\(x) if ("duration_ci" %in% colnames(x))dplyr::filter(x, duration_ci == "central") else x)() |>
      (\(x) if ("dw_ci" %in% colnames(x))dplyr::filter(x, dw_ci == "central") else x)() |>
      dplyr::filter(erf_ci == "central") |>
      dplyr::select(geo_id_disaggregated, exposure_dimension, exp) |>
      dplyr::group_by(geo_id_disaggregated) |>
      dplyr::summarize(exp_central = list(exp), .groups = "drop")

    pop_exp <- input_table |>
      dplyr::filter(exp_ci == "central") |>
      (\(x) if ("duration_ci" %in% colnames(x))dplyr::filter(x, duration_ci == "central") else x)() |>
      (\(x) if ("dw_ci" %in% colnames(x))dplyr::filter(x, dw_ci == "central") else x)() |>
      dplyr::filter(erf_ci == "central") |>
      dplyr::select(geo_id_disaggregated, pop_exp) |>
      dplyr::group_by(geo_id_disaggregated) |>
      dplyr::summarize(pop_exp = list(pop_exp), .groups = "drop")

    ## Create vectors of column names
    exp_columns <- paste0("exp_", seq_exposure_dimension)
    pop_columns <- paste0("pop_exp_", seq_exposure_dimension)

    ## Create empty tibble to be filled in loop below
    ## @ AC: sorry for the loop (again) : P
    dat_sim <- tibble::tibble(
      geo_id_disaggregated = character(0)  # Initialize geo_id_disaggregated as numeric
    ) |>
      dplyr::bind_cols(
        purrr::set_names(rep(list(numeric(0)), length(exp_columns)), exp_columns),
        purrr::set_names(rep(list(numeric(0)), length(pop_columns)), pop_columns)
      )

    for (i in exp_central$geo_id_disaggregated){

      ## Create temp tibble to store simulated values in
      temp <- tibble::tibble(
        geo_id_disaggregated = rep(i, times = n_sim)) |>
        dplyr::bind_cols(
          purrr::map(
            # .x will take the values 1, 2, ..., (nr. of exposure categories)
            .x = seq_along(
              exp_central |>
                dplyr::filter(geo_id_disaggregated == i) |>
                dplyr::pull(exp_central) |>
                base::unlist(x = _)
            ),
            .f = ~ tibble::tibble(
              !!paste0("exp_", .x) := exp_central |> dplyr::filter(geo_id_disaggregated == i) |> dplyr::pull(exp_central) |> unlist(x = _) |> dplyr::nth(.x),
              !!paste0("pop_exp_", .x) := pop_exp |> dplyr::filter(geo_id_disaggregated == i) |> dplyr::pull(pop_exp) |> unlist(x = _) |> dplyr::nth(.x)
            )) |>
            purrr::reduce(dplyr::bind_cols)
        )

      ## Add simulated values of current iteration to dat_sim tabble
      dat_sim <- dat_sim |>
        dplyr::bind_rows(temp)

    }

    # browser()

    # Add simulated values of all geo units to dat tibble
    dat <- dat |>
      dplyr::bind_cols(dat_sim |> dplyr::select(-geo_id_disaggregated))

    }

    # * * dw ###################################################################

    # browser()

    # * * * dw CIs, both single and multiple geo unit case #####################
    if ( (!base::is.null(input_args$dw_lower)) &
         ( n_geo == 1 ) ) {

      ## beta distribution using prevalence::betaExpert()
      ### Determine the alpha and beta parameters needed to fit beta distribution using the (source code of the) prevalence::betaExpert() function
      dw_sim <- betaExpert(
        input_args$dw_central,
        input_args$dw_lower,
        input_args$dw_upper,
        method = "mean")

      ### Simulate nsim disability weight values
      dat <- dat |>
        dplyr::mutate(
          dw = rbeta(
            n = n_sim,
            shape1 = as.numeric(unname(dw_sim["alpha"])),
            shape2 = as.numeric(unname(dw_sim["beta"]))))

      ## ALTERNATIVE: Using normal distribution
      ## Define standard deviation = (dw_upper - dw_lower) / (2 * 1.96)
      # sd_dw <-
      #   (results[["health_detailed"]][["impact_raw"]] |> dplyr::filter(dw_ci == "upper") |> dplyr::pull(dw) |> dplyr::first() -
      #      results[["health_detailed"]][["impact_raw"]] |> dplyr::filter(dw_ci == "lower") |> dplyr::pull(dw) |>  dplyr::first()) / (2 * 1.96)
      # dat <- dat |>
      #   dplyr::mutate(
      #     dw = rnorm(
      #       n_sim,
      #       mean = results[["health_detailed"]][["impact_raw"]] |>
      #         dplyr::filter(dw_ci == "central") |>
      #         dplyr::pull(dw) |>
      #         dplyr::first(),
      #       sd = sd_dw))

      ## ALTERNATIVE: Using beta distribution using qbeta()
      # dat <- dat |>
      #   dplyr::mutate(dw = sim_beta(n_sim = n_sim,
      #                               dw_central = dw_central,
      #                               vector_dw_ci = vector_dw_ci))

      # * * * No dw CIs, both single and multiple geo unit case ################
    } else if ( !is.null(input_args$dw_central) & is.null(input_args$dw_lower) ) {
      dat <- dat |>
        dplyr::mutate(dw = input_args$dw_central)

      # * * * No dw inputted, both single and multiple geo unit case ###########
    } else if (is.null(input_args$dw_central) & is.null(input_args$dw_lower)) {

      dat <- dat |>
        dplyr::mutate(dw = 1)

    }

    # * * erf_eq #################################################################

    # * * * No erf_eq CI's, both single and multiple geo unit case ###############
    if ( !is.null(input_args$erf_eq_central) & is.null(input_args$erf_eq_lower) ) {

    ## Calculate risk for each noise band
    dat <- dat |>
      ## NOTE: not using rowwise & ungroup because results not altered but running time much longer
      # dplyr::rowwise() |>
      dplyr::mutate(
        dplyr::across(.cols = dplyr::starts_with("exp_"),
                      .fns = ~ healthiar::get_risk(exp = .x, erf_eq = input_table$erf_eq |> dplyr::first(x = _)) / 100,
                      .names = "risk_{stringr::str_remove(.col, 'exp_')}")
      ) # |>
      # dplyr::ungroup()

    # * * * erf_eq CI's & multiple geo unit case ###############################
    } else if ( !is.null(input_args$erf_eq_central) &
                !is.null(input_args$erf_eq_lower) ){

      ## For each exp category, create 3 risk (ri) columns: e.g. ri_1_central, ri_1_lower, ri_1_upper & add to dat
      ### For the columns ri_..._central use the erf_eq_central, for ri_..._lower use the erf_eq_lower, ...

      ## Calculate risk estimates for each exposure band
      dat <- dat |>
        ## NOTE: not using rowwise & ungroup because results not altered but running time much longer

        ### Central risk estimates
        # dplyr::rowwise() |>
        dplyr::mutate(
          dplyr::across(.cols = dplyr::starts_with("exp_"),
                        .fns = ~ healthiar::get_risk(exp = .x, erf_eq = input_table |> dplyr::filter(erf_ci == "central") |> dplyr::pull(erf_eq) |> dplyr::first()) / 100,
                        .names = "ri_central_{stringr::str_remove(.col, 'exp_')}")
        ) |>
        ### Lower risk estimates
        dplyr::mutate(
          dplyr::across(.cols = dplyr::starts_with("exp_"),
                        .fns = ~ healthiar::get_risk(exp = .x, erf_eq = input_table |> dplyr::filter(erf_ci == "lower") |> dplyr::pull(erf_eq) |> dplyr::first()) / 100,
                        .names = "ri_lower_{stringr::str_remove(.col, 'exp_')}")
        ) |>
        ### Upper risk estimates
        dplyr::mutate(
          dplyr::across(.cols = dplyr::starts_with("exp_"),
                        .fns = ~ healthiar::get_risk(exp = .x, erf_eq = input_table |> dplyr::filter(erf_ci == "upper") |> dplyr::pull(erf_eq) |> dplyr::first()) / 100,
                        .names = "ri_upper_{stringr::str_remove(.col, 'exp_')}")
        ) # |>
        # dplyr::ungroup()

      ## For each noise band for each row fit a normal distribution using the risk_..._... columns and simulate 1 value (for that specific row)
      ## Corresponding ri_central_..., ri_lower_... and ri_upper_... columns are used
      dat <- dat |>
        dplyr::rowwise() |>
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::starts_with("ri_central_"),
            .fns = ~ rnorm(
            1, # Only 1 simulation
            mean = .x,
            sd = ( dat[[gsub("ri_central_", "ri_upper_", dplyr::cur_column())]] -
                     dat[[gsub("ri_central_", "ri_lower_", dplyr::cur_column())]] ) / (2 * 1.96) # Formula: exp_upper - exp_lower) / (2 * 1.96)
          ),
          .names = "risk_{stringr::str_remove(.col, 'ri_central_')}"
        )
        ) |>
        dplyr::ungroup()
    }



    # * Get impact AR pathway ##################################################

    ## Calculate impact per noise band
    ### impact_X = risk_X * pop_X * dw

    dat <- dat |>
      dplyr::mutate(
        ## Iterate over corresponding "risk_" and "pop_" columns
        ### Also multiply with the disability weight
        dplyr::across(dplyr::starts_with("risk_"), ~ as.numeric(.x) * as.numeric(dat[[gsub("risk_", "pop_exp_", dplyr::cur_column())]]) * as.numeric(dw),
                      .names = "impact_{stringr::str_remove(.col, 'risk_')}")) |>
      # Sum impacts across noise bands to obtain total impact
      dplyr::mutate(impact_total = rowSums(dplyr::across(dplyr::starts_with("impact_"))))

  }

  # Determine 95% CI of impact #################################################

  # * Single geo unit ##########################################################

  if ( ( n_geo == 1 ) ) {

    ## CI of aggregated impact
    ### Because there's only 1 geo unit the aggregated impact is the same as the geo unit impact
    ci <- stats::quantile(x = dat |> dplyr::pull(impact_total) |> base::unlist(),
                   probs = c(0.025, 0.5, 0.975),
                   na.rm = TRUE)

    ci <- unname(ci) # Unname to remove percentiles from the names vector
    ci <- tibble::tibble(central_estimate = ci[2],
                 lower_estimate = ci[1],
                 upper_estimate = ci[3])

  # * Multiple geo units ###################################################
  } else if ( n_geo > 1 ) {

    # browser()

    ## CIs of impact per geo unit
    impact_per_geo_unit <- dat |>
      dplyr::group_by(geo_id_disaggregated) |>
      dplyr::summarize(
        impact_central = stats::quantile(
          x = impact_total,
          probs = c(0.5),
          na.rm = TRUE
          ),
        impact_lower = stats::quantile(
          x = impact_total,
          probs = c(0.025),
          na.rm = TRUE
        ),
        impact_upper = stats::quantile(
          x = impact_total,
          probs = c(0.975),
          na.rm = TRUE
        )
        )

    results[["uncertainty_detailed"]][["geo_specific"]] <- impact_per_geo_unit

    ## CIs of impact aggregated over geo units
    ci <- impact_per_geo_unit |>
      dplyr::summarize(
        central_estimate = sum(impact_central),
        lower_estimate = sum(impact_lower),
        upper_estimate = sum(impact_upper)
        )

  }

  # Output #####################################################################
  on.exit(options(user_options))

  results[["uncertainty_main"]] <- ci

  results[["uncertainty_detailed"]][["raw"]] <- dat # to check interim results during development

  return(results)

}

