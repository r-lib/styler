# Set space between levels of nesting

With the nested approach, certain rules do not have an effect anymore
because of the nature of the nested structure. Setting spacing before
curly brackets in for / if / while statements and function declarations
will be such a case since a curly bracket is always at the first
position in a parse table, so spacing cannot be set after the previous
token.

## Usage

``` r
set_space_between_levels(pd_flat)
```

## Arguments

- pd_flat:

  A flat parse table.
