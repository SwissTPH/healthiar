# Get input data and PAF

This function calculates the population attributable fraction (PAF)
based on the input data and puts the results in additional columns
joined to the input data frame.

## Usage

``` r
get_risk_and_pop_fraction(input_table, pop_fraction_type)
```

## Arguments

- input_table:

  `Data frame` with the input data

- pop_fraction_type:

  `String` indicating the type of the population fraction. Options:
  "paf" or "pif"

## Value

This function returns a `data.frame` with the input data adding a column
for the population attributable fraction Moreover, the data frame
includes columns such as:

- Attributable fraction

- Health impact

- Outcome metric

- And many more.

## Author

Alberto Castro & Axel Luyten
