# Find columns with multiple values

This function find data frame or tibble column names with different
values in their rows (i.e. not a unique value)

## Usage

``` r
find_multi_value_col_names(df, group_col_names = NULL)
```

## Arguments

- df:

  `Data frame or tibble` containing the data

- group_col_names:

  `String vector` that refers to the column names in `df` that serve as
  grouping columns.

## Value

This function returns a `string vector` with the names of the columns
with multiple values

## Author

Alberto Castro & Axel Luyten
