# Unindent a child if necessary

check whether any of the children of `pd` has `token` on the same line
as the closing `token` of pd. If so, unindent that token.

## Usage

``` r
set_unindention_child(pd, token = "')'", unindent_by)
```

## Arguments

- pd:

  A parse table.

- token:

  The token the unindention should be based on.

- unindent_by:

  By how many spaces one level of indention is reversed.
