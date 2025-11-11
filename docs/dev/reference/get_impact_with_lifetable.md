# Get population impact over time

Get population impact over time

## Usage

``` r
get_impact_with_lifetable(input_with_risk_and_pop_fraction)
```

## Arguments

- input_with_risk_and_pop_fraction:

  `Data frame` with the input data (including risk and population
  fraction)

## Value

This function returns a `data.frame` with one row for each value of the
concentration-response function (i.e. central estimate, lower and upper
bound confidence interval). Moreover, the data frame include columns
such as:

- Attributable fraction

- Health impact

- Outcome metric

- And many more.

## Author

Alberto Castro & Axel Luyten
