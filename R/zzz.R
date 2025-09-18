.onAttach <- function(libname, pkgname) {
  # packageStartupMessage("This is version ", packageVersion(healthiar)," of ", healthiar,)
  packageStartupMessage("***** DISCLAIMER *****\nThe developers are not liable for any calculation errors or inaccuracies resulting from the use of the R package healthiar. It is work in progress and the functions of this R package can change at any moment.\n***** HAPPY CODING *****")
}

# Trick to prevent CMD check notes no visible binding for global variable
.onLoad <- function(libname, pkgname) {

  # Manually (time consuming)
  # col_names <- c(
  #   "benefit", "cost", "benefit_rounded", "cost_rounded",
  #   "net_benefit", "net_benefit_rounded", "cbr", "roi",
  #   "impact_scen_1", "impact_scen_2", "impact", "impact_sum", "impact_rate", "impact_rate_std", "total_impact",
  #   "impact_per_100k_inhab", "impact_per_100k_inhab_std", "impact_weight", "impact_rounded",
  #   "impact_by_age_and_year", "impact_by_age_and_year_long",
  #   "geo_id_micro", "exp_length",
  #   "age_group", "age_start", "age_end", "min_age", "max_age","age_interval_index", "age_group_n_years", "age_order",
  #   "impact_yll", "impact_yld",
  #   "bhd", "bhd_n_years", "bhd_1_year", "bhd_sum",
  #   "absolute_risk_as_percent",
  #   "dw", "duration",
  #   "rr", "rr_increment", "erf_shape", "erf_eq", "rr_at_exp", "rr_at_exp_scen_1", "rr_at_exp_scen_2",
  #   "deaths", "prob_survival", "modification_factor", "hazard_rate",
  #   "prob_survival", "prob_survival_mod", "prob_survival_until_midyear", "prob_survival_until_midyear_mod",
  #   "prob_surviving_n_years", "prob_surviving_1_year",
  #   "population", "population_by_geo_table", "total_population", "ref_prop_pop", "population_sum",
  #   "population_n_years", "population_1_year",
  #   "pop_weight",
  #   "entry_population_1_year",
  #   "entry_population_yoa", "midyear_population_yoa",
  #   "projection_if_unexposed_by_age_and_year", "projection_if_exposed_by_age_and_year",
  #   "year",
  #   "input_data",
  #   "cutoff",
  #   "exp_scen_1", "exp_scen_2", "exp_std",
  #   "pop_exp", "prop_pop_exp", "prop_pop_exp_scen_1", "prop_pop_exp_scen_2",
  #   "pop_fraction",
  #   "inflation_factor", "discount_factor", "monetized_impact",
  #   "poll_weighted",
  #   "prob_dying",
  #   "norm_edu", "norm_unemployed", "norm_single_parent", "norm_pop_change", "ind", "values", "density",
  #   "MDI", "MDI_index", "social_ranking",
  #   "parameter", "parameter_string",
  #   "value", "first", "last", "absolute_quantile",
  #   "overall", "absolute_overall",
  #   "difference_compared_with", "difference_type",
  #   "is_paf_from_deprivation", "sim_id", "output_sim_after_impact", "var")

  # More "automatic" way to list the columns
  # Paste here the output of check() regarding the missing global variables
  # There is a full list
  missing_global_variables_raw <-
    "age_group benefit geo_id_micro impact impact_scen_1 impact_scen_2
    net_benefit MDI MDI_index absolute_overall absolute_quantile
    absolute_risk_as_percent age_end age_end_over_min_age
    age_group_n_years age_interval_index age_order age_start bhd
    bhd_1_year bhd_n_years bhd_sum cutoff data_by_age deaths
    density difference_compared_with difference_type discount_factor
    duration dw end_population_yoa entry_population_1_year
    entry_population_yoa erf_eq erf_shape exp_length
    exp_scen_1 exp_scen_2 exp_std first hazard_rate hazard_rate_mod
    impact_by_age_and_year impact_by_age_and_year_long
    impact_per_100k_inhab impact_per_100k_inhab_std impact_rate
    impact_rate_std impact_rounded impact_sum impact_weight impact_yld
    impact_yll ind inflation_factor input_data
    is_attributable_from_deprivation is_paf_from_deprivation last max_age
    midyear_population_yoa min_age modification_factor monetized_impact
    norm_edu norm_no_heating norm_pop_change norm_single_parent
    norm_unemployed output_sim_after_impact overall parameter
    parameter_string poll_weighted pop_exp pop_fraction pop_weight
    population population_1_year population_by_geo_table
    population_n_years population_sum prob_dying prob_survival
    prob_survival_mod prob_survival_until_midyear
    prob_survival_until_midyear_mod prob_surviving_1_year
    prob_surviving_n_years projection_if_exposed_by_age_and_year
    projection_if_unexposed_by_age_and_year prop_pop_exp
    prop_pop_exp_scen_1 prop_pop_exp_scen_2 ref_prop_pop rr
    rr_at_exp rr_at_exp_scen_1 rr_at_exp_scen_2 rr_increment setNames
    sex sim_id social_ranking total_impact total_population value values var
    year year_of_analysis"

  col_names <- base::unlist(base::strsplit(missing_global_variables_raw, "\\s+"))

  utils::globalVariables(col_names)
}


