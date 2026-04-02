# Check whether an else expression needs braces

Checks whether an else expression in a nest needs braces. Note that for
if-else-if expressions, there is no need to add braces since the if in
else-if will be visited separately with the visitor. This applies to all
conditional statements with more than one alternative.

## Usage

``` r
contains_else_expr_that_needs_braces(pd)
```

## Arguments

- pd:

  A parse table
