# Add positional information of token to next terminal

This is needed because at serialization time, we also have terminals
only and positional argument of non-terminals were already propagated to
terminals with
[`context_to_terminals()`](https://styler.r-lib.org/reference/context_to_terminals.md).
Because tokens can be added or removed during styling, we must not only
keep the pos_id, but rather we must remember the pos_id of the first
token in the stylerignore sequence (the marker, or the first token on a
line if the stylerignore marker is an inline marker), for which we know
it will still be there, and join these markers later with all tokens in
the stylerignore sequence (this is a one to many join, i.e. one start
marker can have many tokens).

## Usage

``` r
env_add_stylerignore(pd_flat)
```

## Arguments

- pd_flat:

  A parse table.
