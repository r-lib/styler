# Combine child and internal child

Binds two parse tables together and arranges them so that the tokens are
in the correct order.

## Usage

``` r
combine_children(child, internal_child)
```

## Arguments

- child:

  A parse table or `NULL`.

- internal_child:

  A parse table or `NULL`.

## Details

Essentially, this is a wrapper around vctrs::vec_rbind()\], but returns
`NULL` if the result of vctrs::vec_rbind()\] is a data frame with zero
rows.
