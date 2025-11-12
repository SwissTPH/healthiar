# Calculates reference proportion of population

This function calculates reference proportion of population. To be used
in
[`socialize()`](https://github.com/SwissTPH/healthiar/reference/socialize.md)
and
[`standardize()`](https://github.com/SwissTPH/healthiar/reference/standardize.md)
in case that `ref_prop_pop` is not provided.

## Usage

``` r
get_ref_prop_pop(df)
```

## Arguments

- df:

  `Data frame` or `tibble` with the data by `geo_id_micro` and
  `age_group` including a column for `population`

## Value

A `tibble` with the columns

- `age_group` containing `numeric` age values

- `ref_prop_pop` containing `numeric` values

## Author

Alberto Castro & Axel Luyten
