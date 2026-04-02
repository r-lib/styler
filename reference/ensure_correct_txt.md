# Ensure a correct `text` of all strings and numeric constants

Make sure `text` of the tokens `STR_CONST` and `NUM_CONST` is correct
and adapt if necessary. We replace offending `text` in the terminal
expressions with the text of their parents if their line / col position
matches and return an error otherwise.

## Usage

``` r
ensure_correct_txt(pd, text)
```

## Arguments

- pd:

  A parse table.
