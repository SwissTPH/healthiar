# Check the values of function arguments

Low-level helper used by the data validation of the healthiar functions
(e.g.
[`validate_input_attribute()`](https://swisstph.github.io/healthiar/reference/validate_input_attribute.md),
[`socialize()`](https://swisstph.github.io/healthiar/reference/socialize.md),
[`monetize()`](https://swisstph.github.io/healthiar/reference/monetize.md)).

It takes the list of argument values, the names of the arguments to be
checked and a predicate returning `TRUE` for the valid values. Arguments
that are `NULL` (i.e. not entered by the user) are skipped, so the
callers do not have to filter them beforehand.

## Usage

``` r
validate_args(
  args,
  arg_names,
  is_valid,
  message,
  report = "first",
  type = "error"
)
```

## Arguments

- args:

  `List` with the argument values, i.e. `input_args$value`.

- arg_names:

  `Character vector` with the names of the arguments to be checked.
  Names that are not in `args` are ignored.

- is_valid:

  `Function` taking the argument value and returning `TRUE` for the
  valid values. It can be vectorized; the argument is invalid if **any**
  of its values is not valid.

- message:

  `String` with the message. Write `{arg}` where the name(s) of the
  argument(s) concerned have to appear.

- report:

  `String` specifying which arguments are named in the message. Options:
  `"first"` (default, only the first argument found to be invalid),
  `"all"` (all invalid arguments, separated by commas).

- type:

  `String` specifying the condition to be signalled. Options: `"error"`
  (default), `"warning"`.

## Value

This function returns nothing. It is called for its side effect, i.e. an
error or a warning if any of the argument values is not valid.

## Details

**`report`**

Use `"first"` when the message is specific to one argument (e.g. the
options of a categorical argument), so that the users are not confronted
with the options of all arguments at once. Use `"all"` when the message
is the same for all arguments (e.g. "must not be lower than 0"), so that
the users can correct all of them in one go instead of one at a time.

**Order of the calls**

The order of the calls matters: a predicate is only safe once the
previous calls have ruled out the value types it cannot handle. E.g.
checking whether a value is a whole number (`x == base::floor(x)`)
requires that the numeric check has already been passed.

## Author

Alberto Castro & Axel Luyten

## Examples

``` r
if (FALSE) { # \dontrun{
# Message about one single argument
validate_args(
  args = input_args$value,
  arg_names = c("n_quantile", "population"),
  is_valid = base::is.numeric,
  message = "{arg} must contain numeric value(s).")

# Message listing all the arguments concerned
validate_args(
  args = input_args$value,
  arg_names = c("prop_pop_exp", "dw_central"),
  is_valid = function(x){x <= 1},
  message = "The values in the following arguments must not be higher than 1: {arg}.",
  report = "all")
} # }
```
