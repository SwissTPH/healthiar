Main changes but not complete list. For this propose see Github.
# healthiar 0.0.3

* WP Meeting 14 July 2025

## New Features
- No lists (vectors) as input for multiple geo units
- Arguments age_group & sex in attribute_health() and attribute_lifetable()
- Detailed results of simulations by geo unit in summarize_uncertainty()
- New structure of input_args

 
## Bug Fixes 

- Fixed bug in socialize()
- Fixed bug attribute_lifetable()


## Improvements
- Higher performance of attribute_()
- Data validation in monetize()
- Data validation in summarize_uncertainty()
- More validation in attribute_...()


## Others
- Rename: impact_raw is now results_raw
- Rename: listed_output_attribute is now output_attribute



# healthiar 0.0.2

* WP Meeting 02 June 2025

## New Features
- New function standardize()
- Improved and corrected function socialize() 
- Expanded usability of summarize_uncertainty()
- Enabled single exposure in absolute risk 


## Bug Fixes

- Fixed compare() to avoid errors when using erf_eq
- Fixed warning in socialize()
- Fixed bug in prepare_exposure()

## Improvements
- Input data validation in compare()
- Warning if cutoff is NULL and 0 as default

## Others
- Arguments results or output_healthiar become output_attribute
- Internal variable rr_conc (visible in results of attribute functions) becomes rr_at_exp





# healthiar 0.0.1

* WP Meeting 05 May 2025

## New Features
- Version number was added
- Columns with health impacts were moved to the front in results



## Bug Fixes

- Results for impact per 100k inhab. have been corrected
- Exposure lower than cut-off must result in zero health impact

## Improvements
- Custom warning and error messages are now available in attribute_health()


## Others
- get_pop_fraction() becomes intern function
- get_mdi() was renamed to prepare_mdi()
- get_multiexposure() was renamed to multiexpose()
- get_daly was renamed to daly()

