# Attributable health cases based on relative risk

This function calculates the health impacts for each uncertainty and geo
area.

## Usage

``` r
get_impact(input_table, pop_fraction_type)
```

## Arguments

- input_table:

  `Data frame` containing all input data.

## Value

TBD. E.g. This function returns a `data.frame` with one row for each
value of the concentration-response function (i.e. central, lower and
upper bound confidence interval. Moreover, the data frame includes
columns such as:

- Attributable fraction

- Health impact

- Outcome metric

- And many more.

## Author

Alberto Castro & Axel Luyten
