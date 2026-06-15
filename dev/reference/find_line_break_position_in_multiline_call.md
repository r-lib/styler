# Find index of the token before which the line should be broken

Given a multi-line function call parse table, this function finds the
position of the first named argument and breaks returns the index of it.
If there is no named argument, the line is broken right after the
opening parenthesis.

## Usage

``` r
find_line_break_position_in_multiline_call(pd)
```

## Arguments

- pd:

  A parse table.
