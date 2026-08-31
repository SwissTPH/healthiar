Main changes but not complete list. For this propose see Github commits.

# healthiar v0.2.5
* 11 August 2026

## New features
- attribute_lifetable() and prepare_lifetable() now have 
the new argument fraction_lived to enter 
the fraction of year lived specific for each age group. 
Previously, 0.5 was internally fixed for all age groups.
- attribute_health() has been expanded to now accept 
the argument cutoff_ together with erf_eq_ 
in the absolute risk approach (consistent with relative risk risk). 
A warning is shown in this case because, for absolute risk,
it is assumed that erf_eq_ already 
contains a cutoff.
- Fractional positive deaths and bhd values (with values between 0 and 1) 
are now allowed for lifetable calculations.
- The attributable deaths can now be quantifed with constant exposure 
in attribute_lifetable(). Previously, only possible with single year exposure.

## Bug Fixes
- Error when installing the package without package dependences has been prevented.
- The wrong behaviour of erf_eq_lower and erf_eq_upper as string in attribute_health(), 
providing the same result for both, has been corrected.
- The wrong behaviour of socialize() 
providing deciles for other requested quantiles has been fixed.
- The wrong behaviour of get_paf() for categorical exposure distributions has been fixed.

## Documentation
- Bugs in references of function descriptions and vignette have been fixed.
- More clear instructions on how to install the package has been provided.
- Contributing and a Code of Conduct files (sections in pkgdown web pages) 
have been created to provide information on how to interact with the community.
- The final version of paper published in the Journal of Open Source Software 
(JOSS) is now available.
- An updated citation of the package is now available refering to the JOSS paper.
- CITATION.cff has been added.
- The function descriptions and vignette have been updated to 
include instructions of new features.

## Testing
- Additional internal tests were added. Now a total of 382 test.


# healthiar v0.2.4
* 12 March 2026

## Bug Fixes
- Previously, monetize() used the argument inflation_factor for both adjusting discount_rate and increasing value overtime. 
This has been resolved by introducing a new argument called real_growth_rate.  
Now, inflation_rate is used exclusively for adjusting discount_rate, 
while real_growth_rate handles value growth overtime. 

## Other improvements
- get_discount_factor() previously had inflation_rate as argument. 
Now, not anymore to keep different concepts separated. 
- get_inflation_factor() previously had discount_rate as argument. 
Now, not anymore to keep different concepts separated. 

# healthiar v0.2.3
* 19 February 2026

## New features
- Using compare() after standardizing results from attribute_health() now works
- For the sub-group analysis, exposure value can be specific for each geo_id_micro AND info column
- The argument social_indicator and geo_id_micro in socialize() can now use tidy data
- monetize() works after compare()
- prepare_exposure() works for gridded population data 

## Bug Fixes
- Using socialize() and standardize() without ref_prop_pop now works properly
- Now attribute_lifetable() allows all lengths for age group (before, only 100 age categories were allowed)
- The data validation now identifies correctly whether the argument n_years was entered by the user or not

## Testing
- 31 additional internal tests

## Other improvements
- The argument n_years has no default value anymore in monetize() and discount()
- Error message if monetize() is used after compare() with different year of analysis or baseline health data in the two scenarios (not attributable to policy intervention) 
- More complete warning message if absolute risk and cutoff is provided 



# healthiar v0.2.2
* 08 January 2026

## Bug Fixes
- results_raw now stratifies by info arguments 

## Documentation
- New structure of vignette (by topic instead of by function)
- Amendments in readme file in terms of contents and structure including URL to new healthiar website
- Updated and fixed citation

## Testing
- Around 70 additional internal tests for attribute_health()



# healthiar v0.2.1
* 06 November 2025

## Bug Fixes 
- time_horizon did not work in attribute_lifetable()
- population was not summed correct in attribute_lifetable()
- health_detailed in attribute_health() provided a duplicated rows for results by geo_id_micro

## Other improvements
- Better sampling in summarize_uncertainty() using RNG and package parallel
- Other changes to comply with with manual review of CRAN 



# healthiar v0.1.1 
* 19 September 2025

## Other improvements
- Small changes to comply with with automatic review of CRAN 



# healthiar v0.1.0

* 19 September 2025 (1st submission to CRAN, automatic review)

## New Features
- New function get_inflation_rate()
- New argument in cba(): inflation rate
- geo ids can now have different number of exposure categories

## Bug Fixes 
- Argument time_horizon was not working

## Renamings
- discount_years was renamed to n_years
- inflatoin was renamed to inflation_rate
- attribute_by_sim was renamed to impact_by_sim
- positive_impact was renamed to impact_benefit

## Other improvements
- attribute_by_sim_disaggregated is not anymore available as output
- attribute_by_geo_id_micro is not anymore available as output
- Faster performance of summarize_uncertainty()




# healthiar v0.0.4

* 01 September 2025

## New Features
- New function prepare_lifetable()
- Argument info can have different values
- Argument info also available in monetize()
- Data validation in socialize()

## Renamings
- geo_id_disaggregated is now geo_id_micro
- geo_id_aggregated is now geo_id_macro
- output_attribute_1 in attribute_mod() is now output_attribute
- Arguments with suffix _1 and _2 in compare() are now _scen_1 and _scen_2 
- Arguments with exposure_ are now exp_
- Arguments with suffix _1 and _2 in multiexpose() are now _exp_1 and _exp_2 
- And many other renames in output columns and variables

## Other improvements
- More consistent sum of results
- Higher speed because of shorter and/or optimized code in attribute_...() functions


# healthiar v0.0.3

* 14 July 2025

## New Features
- No lists (vectors) as input for multiple geo units
- Arguments age_group & sex in attribute_health() and attribute_lifetable()
- Detailed results of simulations by geo unit in summarize_uncertainty()
- New structure of input_args

## Bug Fixes 
- Fixed bug in socialize()
- Fixed bug attribute_lifetable()

## Renamings
- Rename: impact_raw is now results_raw
- Rename: listed_output_attribute is now output_attribute

## Other improvements
- Higher performance of attribute_()
- Data validation in monetize()
- Data validation in summarize_uncertainty()
- More validation in attribute_...()


# healthiar v0.0.2

* 02 June 2025

## New Features
- New function standardize()
- Improved and corrected function socialize() 
- Expanded usability of summarize_uncertainty()
- Enabled single exposure in absolute risk 

## Bug Fixes
- Fixed compare() to avoid errors when using erf_eq
- Fixed warning in socialize()
- Fixed bug in prepare_exposure()

## Renamings
- Arguments results or output_healthiar become output_attribute
- Internal variable rr_conc (visible in results of attribute functions) becomes rr_at_exp

## Other improvements
- Input data validation in compare()
- Warning if cutoff is NULL and 0 as default



# healthiar v0.0.1

* 05 May 2025

## New Features
- Version number was added
- Columns with health impacts were moved to the front in results

## Bug Fixes
- Results for impact per 100k inhab. have been corrected
- Exposure lower than cut-off must result in zero health impact

## Renamings
- get_pop_fraction() becomes intern function
- get_mdi() was renamed to prepare_mdi()
- get_multiexposure() was renamed to multiexpose()
- get_daly was renamed to daly()

## Other improvements
- Custom warning and error messages are now available in attribute_health()

