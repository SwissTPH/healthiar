runif_with_seed <-
  function(n, min, max, seed){
    set.seed(seed)
    output <- runif(n, min, max)
  }

# The log-log exposure-response function published by Pozzer et al. (2023),
# "Mortality attributable to ambient air pollution: a review of global
# estimates", GeoHealth 7(1) (doi:10.1029/2022GH000711), which rescales the
# relative risk of the epidemiological study from its increment to the
# exposure level as
#
#   RR(C) = ((C + 1) / (C0 + 1)) ^ beta
#   beta  = log(RR_increment) / (log(increment + C0 + 1) - log(C0 + 1))
#
# where C is the exposure and C0 the effect threshold. Written out here so
# that the expected values of the log-log tests do not come from healthiar
# itself
rr_at_exp_pozzer <-
  function(exp, cutoff, rr, rr_increment){

    beta <-
      base::log(rr) /
      (base::log(rr_increment + cutoff + 1) - base::log(cutoff + 1))

    ((exp + 1) / (cutoff + 1)) ^ beta
  }
