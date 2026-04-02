# Enrich flattened parse table

Enriches a flattened parse table with terminals only. In particular, it
is possible to compute the exact position a token will have (line and
column) when it will be serialized.

## Usage

``` r
enrich_terminals(flattened_pd, use_raw_indention = FALSE)
```

## Arguments

- flattened_pd:

  A nested parse table that was turned into a flat parse table using
  [`extract_terminals()`](https://styler.r-lib.org/reference/extract_terminals.md).

- use_raw_indention:

  Boolean indicating whether or not the raw indention should be used.

## Details

Since we have only terminal tokens now, the line on which a token starts
we also be the line on which it ends. We call `line1` the line on which
the token starts. `line1` has the same meaning as `line1` that can be
found in a flat parse table (see
[`tokenize()`](https://styler.r-lib.org/reference/tokenize.md)), just
that the `line1` created by `enrich_terminals()` is the updated version
of the former `line1`. The same applies for `col1` and `col2`. Note that
this function does remove the columns `indent` and `spaces.` All
information of the former is stored in `lag_spaces` now. The later was
removed because it is redundant after adding the column `lag_spaces`,
which is more convenient to work with, in particular when serializing
the parse table.
