# Compile input

This function compiles the input data of the main function and
calculates the population attributable fraction based on the input data
(all in one data frame)

## Usage

``` r
compile_input(input_args, is_lifetable)
```

## Arguments

- input_args:

  `List` with all input data by argument

- is_lifetable:

  `Boolean` INTERNAL argument specifying if the life table approach is
  applied (TRUE) or not (FALSE)

## Value

This function returns a `data.frame` with all input data together
Moreover, the data frame includes columns such as:

- Attributable fraction

- Health impact

- Outcome metric

- And many more.

## Author

Alberto Castro & Axel Luyten
