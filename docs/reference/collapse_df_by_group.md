# Collapse rows by grouping columns

This function paste rows with different values and keep only unique rows
following grouping columns

## Usage

``` r
collapse_df_by_group(
  df,
  group_col_names,
  multi_value_col_names = NULL,
  ci_col_names = NULL,
  only_unique_rows = TRUE
)
```

## Arguments

- df:

  `Data frame or tibble` containing the data

- group_col_names:

  `String vector` containing the column names in `df` that serve as
  grouping columns.

- multi_value_col_names:

  `String vector` containing the columns names in `df` that do not have
  a unique value (but different values).

- ci_col_names:

  `String vector` containing the column names in `df` that refer to
  confidence interval bounds (suffix \_ci).

- only_unique_rows:

  `Boolean` that determines if the final data frame or tibble must only
  keep unique rows

## Value

This function returns a `data frame` or `tibble` with lower number rows
after collapsing

## Author

Alberto Castro & Axel Luyten
