# Wrap a multi-line statement in curly braces

Wrap a multi-line statement in curly braces

## Usage

``` r
wrap_multiline_curly(pd, indent_by, key_token, space_after = 1L)
```

## Arguments

- pd:

  A parse table.

- indent_by:

  The amount of spaces used to indent an expression in curly braces.
  Used for unindention.

- key_token:

  The token that comes right before the token that contains the
  expression to be wrapped (ignoring comments). For if and while loops,
  this is the closing "')'", for a for-loop it's "forcond".

- space_after:

  How many spaces should be inserted after the closing brace.
