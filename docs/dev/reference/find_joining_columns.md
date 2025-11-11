# Find joining columns

This function finds columns in two data frames that have same name and
identical values (excluding exceptions).- The resulting joining columns
are used to dplyr::x_join data frames and add the vector to by=.

## Usage

``` r
find_joining_columns(df_1, df_2, except = NULL)
```

## Arguments

- df_1:

  First `Data frame`

- df_2:

  Second `Data frame`

- except:

  `Vector` of strings showing columns that have to be excluded although
  they fulfill the inclusion criteria (same name and value)

## Value

This function returns a `vector` of strings

## Author

Alberto Castro & Axel Luyten
