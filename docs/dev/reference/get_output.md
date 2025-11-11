# Obtain and store output

This function distributes and store outputs by level of detail by
aggregating or filtering impacts.

## Usage

``` r
get_output(
  input_args = NULL,
  input_table = NULL,
  intermediate_calculations = NULL,
  results_raw
)
```

## Arguments

- input_args:

  `List` containingall arguments and values entered in attribute().

- input_table:

  `Tibble` containing the input_table data compiled and packed in a data
  frame.

- intermediate_calculations:

  `Tibble` containing intermediate calculations (e.g. from life table
  pathway).

- results_raw:

  `Tibble` containing all the calculation of health impacts.

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
