# Check whether a parse table is a multi-line token

A token is a multi-line expression if and only if:

## Usage

``` r
pd_is_multi_line(pd)
```

## Arguments

- pd:

  A parse table.

## Details

- it contains a line break.

- it has at least one child that is a multi-line expression itself.
