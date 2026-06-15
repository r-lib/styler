# Update the newlines attribute

As we work only with the `lag_newlines` attribute for setting the line
breaks (`R/rules-line_breaks.R`), but we need `newlines` to determine
whether or not to set `spaces` (`R/rules-spaces.R`), we have to update
the attribute. We cannot simply use `dplyr::lead(pd$lag_newlines)` since
we would lose information for the last token. `spaces` is left as is in
R/rules-spacing.R for tokens at the end of a line since this allows
styling without touching indention.

## Usage

``` r
update_newlines(pd)
```

## Arguments

- pd:

  A parse table.

## Value

A parse table with synchronized `lag_newlines` and `newlines` columns.

## See also

choose_indention
