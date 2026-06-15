# Update the indention reference

Update the indention reference

## Usage

``` r
update_indention_reference_function_declaration(pd_nested)
```

## Arguments

- pd_nested:

  A nested parse table.

## Functions

- `update_indention_reference_function_declaration()`: Updates the
  reference pos_id for all tokens in `pd_nested` if `pd_nested` contains
  a function declaration. Tokens inside a function declaration are are
  re-indented, that is, they are indented up to the level at which the
  token FUNCTION ends in terms of col2.

## Examples

``` r
if (FALSE) { # \dontrun{
a <- function(x,
              y) {
  x + y
}
} # }
```
