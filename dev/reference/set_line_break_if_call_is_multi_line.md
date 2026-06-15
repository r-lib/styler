# Set line break for multi-line function calls

Set line break for multi-line function calls

## Usage

``` r
set_line_break_before_closing_call(pd, except_token_before)

remove_line_break_in_fun_call(pd, strict)
```

## Arguments

- pd:

  A parse table.

- except_token_before:

  A character vector with tokens that do not cause a line break after
  them.

## Functions

- `set_line_break_before_closing_call()`: Sets line break before closing
  parenthesis.

- `remove_line_break_in_fun_call()`: Remove line breaks in function
  calls.
