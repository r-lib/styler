# Add curly braces to else

Wrap the else part of a conditional expression into curly braces if not
already wrapped into a such.

## Usage

``` r
wrap_else_multiline_curly(pd, indent_by = 2L, space_after = 0L)
```

## Arguments

- pd:

  A parse table.

- indent_by:

  The amount of spaces used to indent an expression in curly braces.
  Used for unindention.

- space_after:

  How many spaces should be inserted after the closing brace.
