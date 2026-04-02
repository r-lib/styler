# Choose the indention method for the tokens

Either use the raw indention, which is just the spaces computed between
the first token on a new line and the token before it, or use the
indention computed according to the transformer used, which is stored in
the column `indention`. All indention information will be combined with
the space information for the first token on a new line. If
`use_raw_indention` is set, information in the column `indention` will
be discarded anyways. If it is not set, the first token on a new line
will "inherit" the indention of the whole line. The column `indention`
will be removed since all information necessary is contained in the
spacing information of the first token on a new line and the position of
the tokens will not be changed anymore at this stage.

## Usage

``` r
choose_indention(flattened_pd, use_raw_indention)
```

## Arguments

- flattened_pd:

  A nested parse table that was turned into a flat parse table using
  [`extract_terminals()`](https://styler.r-lib.org/reference/extract_terminals.md).

- use_raw_indention:

  Boolean indicating whether or not the raw indention should be used.
