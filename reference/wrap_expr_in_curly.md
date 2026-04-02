# Wrap an expression in curly braces

Adds curly braces to an expression (represented as a parse table) if
there are none.

## Usage

``` r
wrap_expr_in_curly(pd, stretch_out = c(FALSE, FALSE), space_after = 1L)
```

## Arguments

- pd:

  A parse table.

- stretch_out:

  Whether or not to create a line break after the opening curly brace
  and before the closing curly brace.

- space_after:

  How many spaces should be inserted after the closing brace.
