# Update the a parse table given outer context

`outer_lag_newlines` are added to the first token in `pd`,
`outer_indent` is added to all tokens in `pd`, `outer_spaces` is added
to the last token in `pd`.
[`context_to_terminals()`](https://styler.r-lib.org/reference/context_to_terminals.md)
calls this function repeatedly, which means the propagation of the parse
information to the terminal tokens.

## Usage

``` r
context_towards_terminals(
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

context_to_terminals
