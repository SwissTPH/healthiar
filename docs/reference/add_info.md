# Add meta-information to the data frame containing the input data

This function adds meta-information of the input data within the data
frame containing the input data.

## Usage

``` r
add_info(df, info)
```

## Arguments

- df:

  `Data frame` containing the input data

- info:

  `String` or `Data frame` with one row or `Vector` of length 1 showing
  additional information or id for the pollutant.

## Value

This function returns a `data frame` with binding the input data with
the info columns (info\_ is added to the column names)

## Author

Alberto Castro & Axel Luyten
