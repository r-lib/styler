# Propagate context to terminals

Implements a very specific pre-visiting scheme, namely to propagate
indention, spaces and lag_newlines to inner token to terminals. This
means that information regarding indention, line breaks and spaces
(which is relative in `pd_nested`) will be converted into absolute.

## Usage

``` r
context_to_terminals(
  pd_nested,
  outer_lag_newlines,
  outer_indent,
  outer_spaces,
  outer_indention_refs
)
```

## Arguments

- pd_nested:

  A nested parse table.

- outer_lag_newlines:

  The lag_newlines to be propagated inwards.

- outer_indent:

  The indention depth to be propagated inwards.

- outer_spaces:

  The number of spaces to be propagated inwards.

- outer_indention_refs:

  The reference pos id that should be propagated inwards.

## Value

An updated parse table.

## See also

context_towards_terminals visitors
