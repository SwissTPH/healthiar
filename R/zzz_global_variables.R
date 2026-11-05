# Prevent CMD check notes no visible binding for global variable
# Paste here the output of check() regarding the missing global variables
# Please alphabetically
# How to update this list:
# 1) Run checkhelper::fix_globals()
# 2) Copy the list of variables inside utils::globalVariables() from the console
# 3) Paste the content below 
# 4) Keep .data, which is not outputed by checkhelper::fix_globals()

utils::globalVariables(unique(c(
# DELETE AND PASTE BELOW THE VARIABLES FROM checkhelper::fix_globals()
# cba: 
"benefit", "net_benefit", 
# compare: 
"impact", "impact_scen_1", "impact_scen_2", 
# compile_input: 
"age_group", "age_start", "exp_length", "geo_id_micro", "max_age", "min_age", "sex", 
# daly: 
"impact", "impact_yld", "impact_yll", "population", "population_yld", "population_yll", 
# get_impact: 
"absolute_risk_as_percent", "bhd", "cutoff", "duration", "dw", "erf_eq", "impact", "pop_exp", "pop_fraction", "population", 
# get_impact_with_lifetable: 
"age_end", "age_start", "bhd", "data_by_age", "deaths", "fraction_lived", "hazard_rate", "hazard_rate_mod", "impact_by_age_and_year", "impact_by_age_and_year_long", "is_exposed_age", "max_age", "midyear_population_yoa", "min_age", "modification_factor", "pop_fraction", "population", "prob_survival", "prob_survival_mod", "prob_survival_until_midyear", "projection_if_exposed_by_age_and_year", "projection_if_unexposed_by_age_and_year", "year_of_analysis", 
# get_impact_with_lifetable : 
"age_end", "age_start", "population", "year", 
# get_output : 
"population", 
# get_ref_prop_pop: 
"age_group", "population", "ref_population", "ref_prop_pop", 
# get_risk_and_pop_fraction: 
"cutoff", "erf_shape", "exp_scen_1", "exp_scen_2", "pop_fraction", "prop_pop_exp", "prop_pop_exp_scen_1", "prop_pop_exp_scen_2", "rr", "rr_at_exp", "rr_at_exp_scen_1", "rr_at_exp_scen_2", "rr_increment", 
# monetize: 
"impact_yld", "impact_yll", "monetized_impact", "monetized_impact_unadjusted_yld", "monetized_impact_unadjusted_yll", "monetized_impact_yld", "monetized_impact_yll", "population", "year", 
# monetize : 
"discount_factor", "monetized_impact", "year", 
# prepare_exposure: 
"bin", "poll", "pop", 
# prepare_exposure : 
"bin", "coverage_fraction", "poll", "pop", 
# prepare_lifetable: 
"age_group_n_years", "age_interval_index", "bhd_1_year", "bhd_n_years", "entry_population_1_year", "fraction_lived_n_years", "hazard_rate", "hazard_rate_n_years", "population_1_year", "population_n_years", "prob_dying", "prob_dying_1_year", "prob_surviving_1_year", 
# prepare_mdi: 
"MDI", "MDI_index", 
# socialize: 
"absolute_overall", "absolute_quantile", "age_order", "difference_compared_with", "difference_type", "first", "impact_rate", "impact_rate_std", "impact_sum", "is_attributable_from_deprivation", "is_paf_from_deprivation", "last", "overall", "parameter", "parameter_string", "population_sum", "social_ranking", "value", 
# socialize : 
"bhd_sum", "population_sum", 
# standardize: 
"bhd", "bhd_per_100k_inhab", "bhd_per_100k_inhab_std", "exp_std", "impact", "impact_per_100k_inhab", "impact_per_100k_inhab_std", "pop_fraction_std", "pop_weight", "population", "total_impact", "total_population", 
# summarize_uncertainty: 
"geo_id_micro", "impact", "impact_rounded", "impact_scen_1", "impact_scen_2", "sim_id", 
# summarize_uncertainty : 
"central", "geo_id_micro", "impact", "lower", "sim_id", "simulated", "upper", 
# validate_input_attribute: 
"age_group", "is_not_consecutive", 
# validate_input_attribute : 
"var",


# KEEP THIS (DO NOT DELETE!)  
".data"
)))