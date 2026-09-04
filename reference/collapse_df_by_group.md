# Collapse rows by grouping columns

This function aggregates a data frame into one row per group, pasting
the values of the columns that have different values within a group and
(optionally) summing the columns specified in `sum_col_names`

## Usage

``` r
collapse_df_by_group(
  df,
  group_col_names,
  sum_col_names = NULL,
  multi_value_col_names = NULL
)
```

## Arguments

- df:

  `Data frame or tibble` containing the data

- group_col_names:

  `String vector` containing the column names in `df` that serve as
  grouping columns.

- sum_col_names:

  `String vector` containing the column names in `df` that have to be
  summed within each group (e.g. impacts). Optional.

- multi_value_col_names:

  `String vector` containing the columns names in `df` that do not have
  a unique value (but different values).

## Value

This function returns a `data frame` or `tibble` with one row per group,
keeping the columns and the column order of `df`

## Author

Alberto Castro & Axel Luyten
