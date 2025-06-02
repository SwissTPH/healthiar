Main changes but not complete list. For this propose see Github.

# healthiar 0.0.2

* WP Meeting 02.06.2025

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

* WP Meeting 05.05.2025

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

